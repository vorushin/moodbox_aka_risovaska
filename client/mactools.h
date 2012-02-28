#ifndef MACTOOLS_H
#define MACTOOLS_H

#include <QObject>
#include <QWidget>

namespace MoodBox {

enum MacAppEvent {ReopenApplication = 1, QuitApplication = 2};

/*
 CocoaInitializer prepares all data to use Cocoa-based frameworks
 */
class CocoaInitializer
{
public:
    CocoaInitializer();
    ~CocoaInitializer();
private:
    class Private;
    Private*    d;
};
    
    
/*
   AppEventHandler is a gateway between OS X native application 
   events and the Qt world. 
   Singleton.
   After registering required events, client is notified 
   through signals.
 */
class AppEventHandler: public QObject
{
    Q_OBJECT

public:
    static AppEventHandler* instance();
    static void cleanUp();
    
    void registerHandlerFor(MacAppEvent event);
    void unregisterHandlerFor(MacAppEvent event);
    void emitEvent(MacAppEvent event);    

    virtual ~AppEventHandler();
signals:
    void reopenApplication();
    void quitApplication();
};
    
/*
  Contains collection of utility functions for Mac specific
  window management

  Every window, which should be hidden while main window
  is minimized, should be registered by calling MacTools::addWindow
  */
class MacTools
{
public:
    static void swapWidgetPositions(QWidget *first, QWidget *second);
    static void placeFirstToLeft(QWidget *first, QWidget *second);
    static void handleMacEvent(QWidget *widget, void* handler, void *event);

    static void addWindow(QWidget *window);
    static void removeWindow(QWidget *window);
    static void collapseWindows();
    static void restoreWindows();
    static void cleanUp();
    
    static QString resourceDir();
    static bool setAutoStart(bool enabled);
    static bool getAutoStart();
};

/*
  DockIconHandler updates overlay on dock icon to a number of unread contacts
  */
class DockIconHandler: public QObject
{
    Q_OBJECT
public:
    static DockIconHandler* instance();
    static void cleanUp();

public slots:
    void unreadCountChanged(int newCount);
};
     
}

#endif // MACTOOLS_H
