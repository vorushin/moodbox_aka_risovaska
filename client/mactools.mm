#include "mactools.h"
#include <Carbon/Carbon.h>
#include <QList>
#include <QPainter>
#include <Cocoa/Cocoa.h>

namespace MoodBox 
{
   
class MacToolsHelper
{
public:
    static CFStringRef bundlePath();
    static int indexOfStringInPrefsArray(CFArrayRef prefsArray, CFStringRef string);
};
    
CFStringRef MacToolsHelper::bundlePath()
{
    CFURLRef pluginRef = CFBundleCopyBundleURL(CFBundleGetMainBundle());
    CFStringRef macPath = CFURLCopyFileSystemPath(pluginRef,
                                                  kCFURLPOSIXPathStyle);
    CFRelease(pluginRef);
    return macPath;
}

int MacToolsHelper::indexOfStringInPrefsArray(CFArrayRef prefsArray, CFStringRef string)
{
    int idx = -1;
    int cnt = CFArrayGetCount(prefsArray);
    for (int i = 0; i < cnt; i++)
    {
        CFDictionaryRef value = (CFDictionaryRef)CFArrayGetValueAtIndex(prefsArray, i);
        if (CFDictionaryContainsValue(value, (const void*)string))
        {
            idx = i;
            break;
        }
    }
    return idx;
}
    
    
// Utility functions
static OSStatus appleEventProcessor(const AppleEvent *ae,
                                    AppleEvent *event,
                                    long handlerRefCon)
{
	Q_UNUSED(event)
	
    AppEventHandler *eventHandler = (AppEventHandler *) handlerRefCon;
    
    OSType aeID = typeWildCard;
    OSType aeClass = typeWildCard;
    
    AEGetAttributePtr(ae, keyEventClassAttr, typeType, 0,
                      &aeClass, sizeof(aeClass), 0);
    AEGetAttributePtr(ae, keyEventIDAttr, typeType, 0,
                      &aeID, sizeof(aeID), 0);
    
    if (aeClass == kCoreEventClass) {
        if (aeID == kAEReopenApplication) {
            eventHandler->emitEvent(ReopenApplication);          
        }
        else if (aeID == kAEQuitApplication) {
            eventHandler->emitEvent(QuitApplication);
        }
        return noErr;
    }
    
    return eventNotHandledErr;
}
    
static OSType macAppEventToOsType(MacAppEvent &event)
{
    OSType result = kAEReopenApplication;
    switch (event)
    {
        case ReopenApplication:
            result = kAEReopenApplication;
            break;
        case QuitApplication:
            result = kAEQuitApplication;
            break;            
    }
    return result;
}
    
// Cocoa Initializer
    
class CocoaInitializer::Private {
public:
    NSAutoreleasePool* autoReleasePool;
};

CocoaInitializer::CocoaInitializer() {
    d = new CocoaInitializer::Private();
    NSApplicationLoad();
    d->autoReleasePool = [[NSAutoreleasePool alloc] init];
}

CocoaInitializer::~CocoaInitializer() {
    [d->autoReleasePool release];
    delete d;
}
       
// AppEvent Handler
static AppEventHandler *handlerInstance = NULL;    
    
AppEventHandler* AppEventHandler::instance()
{
    if (!handlerInstance)
        handlerInstance = new AppEventHandler();
    
    return handlerInstance;
}
    
void AppEventHandler::cleanUp()
{
    if (handlerInstance)
    {
        delete handlerInstance;
        handlerInstance = NULL;
    }
}
    
void AppEventHandler::registerHandlerFor(MacAppEvent event)
{
    AEInstallEventHandler(kCoreEventClass, macAppEventToOsType(event), AEEventHandlerUPP(appleEventProcessor), /*(SRefCon)*/(long int)this, true);
}
    
void AppEventHandler::unregisterHandlerFor(MacAppEvent event)
{
    AERemoveEventHandler(kCoreEventClass, macAppEventToOsType(event), AEEventHandlerUPP(appleEventProcessor), true);                
}
    
void AppEventHandler::emitEvent(MacAppEvent event) 
{
    switch (event)
    {
        case ReopenApplication:
            emit reopenApplication();
            break;
        case QuitApplication:
            emit quitApplication();
            break;
    }        
}    
    
AppEventHandler::~AppEventHandler() 
{
    // todo: store all registered events and unregister them automatically
}

// MacTools
void MacTools::swapWidgetPositions(QWidget *first, QWidget *second)
{
    QPoint tmpPos = first->pos();
    first->move(second->pos());
    second->move(tmpPos);
}

void addWidgetToWindowGroup(QWidget *widget)
{
     static WindowGroupRef appWindowGroup = 0;
     if (appWindowGroup == 0)
        CreateWindowGroup(kWindowGroupAttrHideOnCollapse, &appWindowGroup);
     WindowRef wnd = qt_mac_window_for(widget);
     if (!IsWindowContainedInGroup(wnd, appWindowGroup))
         SetWindowGroup(wnd, appWindowGroup);
}

void MacTools::handleMacEvent(QWidget *widget, void* , void *eventPtr)
{
    EventRef event = (EventRef)eventPtr;
    OSType eventType = GetEventClass(event);
    unsigned int eventKind = GetEventKind(event);

    RetainEvent(event);
    if (eventType == kEventClassWindow)
    {
        fprintf(stdout, "window event %d\n", eventKind);
        fflush(stdout);

        switch (eventKind)
        {
            case kEventWindowShown:
            case kEventWindowActivated:
                fprintf(stdout, "hehe - adding to group\n");
                fflush(stdout);

                addWidgetToWindowGroup(widget);
                break;
        }

    }
}
    
static QList<QWidget*> registeredWindows;
static QList<QWidget*> collapsedWindows;

void MacTools::addWindow(QWidget *window)
{
    registeredWindows.append(window);
}
    
void MacTools::removeWindow(QWidget *window)
{
    registeredWindows.removeAll(window);
}

void MacTools::collapseWindows()
{
    QWidget *window;
    foreach(window, registeredWindows)
    {
        if (window->isVisible())
        {
            window->hide();
            collapsedWindows.append(window);
        }
    }
}

void MacTools::restoreWindows()
{
    QWidget *window;
    foreach(window, collapsedWindows)
        window->show();
    collapsedWindows.clear();
}

void MacTools::cleanUp()
{
}

QString MacTools::resourceDir()
{
    CFStringRef macPath = MacToolsHelper::bundlePath();
    char *buf = 0;
    const char *pathPtr = 0;
    pathPtr = CFStringGetCStringPtr(macPath, CFStringGetSystemEncoding());

    if (!pathPtr) 
    {
        buf = new char[1024];
        CFStringGetCString(macPath, buf, 1024, CFStringGetSystemEncoding());
        pathPtr = buf;
    }
    QString result(pathPtr+QString("/Contents/Resources/"));
    
    CFRelease(macPath);
    
    if (buf)
        delete [] buf;
    
    return result;
}
    
bool MacTools::setAutoStart(bool enabled)
{
    CFArrayRef prefCFArrayRef = (CFArrayRef)CFPreferencesCopyAppValue(CFSTR("AutoLaunchedApplicationDictionary"), CFSTR("loginwindow"));
    CFMutableArrayRef tCFMutableArrayRef = CFArrayCreateMutableCopy(NULL, 0, prefCFArrayRef);
    CFStringRef bundlePath = MacToolsHelper::bundlePath();

    int idx = MacToolsHelper::indexOfStringInPrefsArray(tCFMutableArrayRef, bundlePath);
    
    bool changed = false;
    if (enabled)
    {
        if (idx == -1)
        {
            CFMutableDictionaryRef dict = CFDictionaryCreateMutable(0, 0, 0, 0);
            CFDictionaryAddValue(dict, (const void*)CFSTR("Path"), (const void*)bundlePath);
            CFDictionaryAddValue(dict, (const void*)CFSTR("Hide"), (const void*)kCFBooleanFalse);
            CFArrayAppendValue(tCFMutableArrayRef, dict);
            CFRelease(dict);
            changed = true;
        }
    }
    else 
    {    
        if (idx != -1)
        {
            CFArrayRemoveValueAtIndex(tCFMutableArrayRef, idx);
            changed = true;            
        }
    }
    
    if (changed)
    {    
        CFPreferencesSetAppValue(CFSTR("AutoLaunchedApplicationDictionary"), tCFMutableArrayRef, CFSTR("loginwindow"));    
        CFPreferencesSynchronize(CFSTR("loginwindow"), kCFPreferencesCurrentUser, kCFPreferencesCurrentHost);
    }
    
    CFRelease(tCFMutableArrayRef);
    CFRelease(bundlePath);
    CFRelease(prefCFArrayRef);
    
    return changed;
}    

bool MacTools::getAutoStart()
{
    CFArrayRef prefCFArrayRef = (CFArrayRef)CFPreferencesCopyAppValue(CFSTR("AutoLaunchedApplicationDictionary"), CFSTR("loginwindow"));
    CFStringRef bundlePath = MacToolsHelper::bundlePath();
    
    bool result = MacToolsHelper::indexOfStringInPrefsArray(prefCFArrayRef, bundlePath) != -1;
    
    CFRelease(bundlePath);
    CFRelease(prefCFArrayRef);
    
    return result;    
}        
    
// DockIconHandler
static DockIconHandler* dockIconHandler = NULL;
DockIconHandler* DockIconHandler::instance()
{
    if (dockIconHandler == NULL)
        dockIconHandler = new DockIconHandler();
    return dockIconHandler;
}

void DockIconHandler::cleanUp()
{
    if (dockIconHandler != NULL)
        delete dockIconHandler;
}

void DockIconHandler::unreadCountChanged(int newCount)
{
    static int hasBadge = -1;
    if (hasBadge == -1) 
    {
        OSErr err;
        SInt32 majorVersion, minorVersion;
        if ((err = Gestalt(gestaltSystemVersionMinor, &minorVersion)) == noErr) 
        {
            if ((err = Gestalt(gestaltSystemVersionMajor, &majorVersion)) == noErr)
            {
                if ((majorVersion > 10) || (majorVersion == 10 && minorVersion >= 5))
                    hasBadge = 1;
                else
                    hasBadge = 0;
            }
        }
    }
    
    if (hasBadge == 1) 
    {
        NSString *badge = nil;
        
        if (newCount > 0)
            badge = [NSString stringWithFormat:@"%d", newCount];
        
        [[[NSApplication sharedApplication] dockTile] setBadgeLabel:badge];
    }
    else 
    {    
        if (newCount == 0)
        {
            RestoreApplicationDockTileImage();
        }
        else
        {
            QPixmap pixmap(128, 128);
            QPainter *painter = new QPainter(&pixmap);
            QRect rect(0, 0, 128, 128);

            QRect circleRect(rect.width()-48, 0, 48, 48);
            painter->setPen(Qt::NoPen);
            painter->setBrush(QColor(255, 0, 0));
            painter->drawEllipse(circleRect);

            QFont unreadFont("Verdana");
            unreadFont.setPixelSize(24);
            unreadFont.setBold(true);

            QString unreadText;
            if (newCount < 100)
                unreadText = QString::number(newCount);
            else
                unreadText = "...";

            QRect textRect;
            if (newCount < 10)
                textRect = QRect(circleRect.left(), circleRect.top() - 1, 48, 48);
            else
                textRect = QRect(circleRect.left(), circleRect.top() - 1, 48, 48);

            painter->setPen(Qt::white);
            painter->setFont(unreadFont);
            painter->drawText(textRect, Qt::AlignCenter, unreadText);
            painter->end();
            delete painter;

            CGImageRef cgImage = pixmap.toMacCGImageRef();
            OverlayApplicationDockTileImage(cgImage);
            CGImageRelease(cgImage);
        }
    }
}

}
