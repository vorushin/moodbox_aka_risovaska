#include "apptools.h"

#include <QCoreApplication>
#include <QDir>
#include <QFileInfo>
#include <QLibrary>
#include <QSettings>
#include <QWidget>

#include "common.h"

#ifdef WIN32
#include <Windows.h>
#include <TCHAR.H>
#include <Shfolder.h>
#else
#include <QApplication>
#endif

#ifdef Q_WS_MAC
#include "mactools.h"
#endif

namespace MoodBox
{

#ifdef WIN32

// TODO review, possible move to Qt version (see in #else section)
QString AppTools::getPathToExe()
{
	wchar_t fileName[MAX_PATH];

	if (GetModuleFileName(GetModuleHandle(NULL), fileName, MAX_PATH))
		return QDir::fromNativeSeparators(QString::fromWCharArray(fileName));

	return QString();
}

QString AppTools::getPathRelativeToExeDir(const QString &suffixDir)
{
	return QFileInfo(getPathToExe()).absolutePath() + PATH_SEPARATOR + suffixDir;
}

#else

QString AppTools::getPathToExe()
{
	return QCoreApplication::applicationFilePath();
}

QString AppTools::getPathRelativeToExeDir(const QString &pathAddon)
{
	return QCoreApplication::applicationDirPath() + PATH_SEPARATOR + pathAddon;
}

#endif

QString AppTools::addPathSeparator(const QString &path)
{
	QString readyPath = QDir::toNativeSeparators(path);

	if (!readyPath.endsWith(QDir::separator()))
		readyPath += QDir::separator();

	return QDir::fromNativeSeparators(readyPath);
}

QString AppTools::getPortableAppFolder(AppDataFolderType folderType)
{
	return addPathSeparator((folderType == Common) ? QDir::tempPath() : QDir::homePath());
}

#ifdef WIN32
QString AppTools::getAppDataFolder(const QString &postfix, AppDataFolderType folderType)
{
	wchar_t szPath[MAX_PATH];
	QString result;

	int folder = CSIDL_APPDATA;

	if (folderType == Local)
		folder = CSIDL_LOCAL_APPDATA;
	else
		if (folderType == Common)
			folder = CSIDL_COMMON_APPDATA;

	// Get path for each computer, non-user specific and non-roaming data.
	result = (SUCCEEDED(SHGetFolderPath(NULL, folder, NULL, 0, szPath))) ?
		addPathSeparator(QString::fromWCharArray(szPath)) : getPortableAppFolder(folderType);

	result = addPathSeparator(addPathSeparator(result + PUBLISHER_NAME) + APP_NAME);
	
	return addPathSeparator(result + postfix);
}

#endif
#ifdef Q_WS_MAC
#define MAC_PREFS_FOLDER        "Library/Application Support/"

QString AppTools::getAppDataFolder(const QString &postfix, AppDataFolderType folderType)
{
	QString result = getPortableAppFolder(folderType);
    	result = addPathSeparator(addPathSeparator(result + MAC_PREFS_FOLDER + PUBLISHER_NAME) + APP_NAME);
	
	return addPathSeparator(result + postfix);
}
#endif

#ifdef Q_WS_X11
QString AppTools::getAppDataFolder(const QString &postfix, AppDataFolderType folderType)
{
	QString result = getPortableAppFolder(folderType);
    	result = addPathSeparator(addPathSeparator(result + PUBLISHER_NAME) + APP_NAME);
	
	return addPathSeparator(result + postfix);
}
#endif


// Win32 has special blinker
#ifdef WIN32

typedef BOOL (WINAPI *PtrFlashWindowEx)(PFLASHWINFO pfwi);
PtrFlashWindowEx pFlashWindowEx = 0;

void AppTools::alertWidget(QWidget *widget, int duration)
{
    if (!pFlashWindowEx) 
	{
        QLibrary themeLib(QLatin1String("user32"));
        pFlashWindowEx  = (PtrFlashWindowEx)themeLib.resolve("FlashWindowEx");
    }

    if (pFlashWindowEx && widget) 
	{
        DWORD timeOut = GetCaretBlinkTime();
        if (timeOut <= 0)
            timeOut = 250;

        UINT flashCount;
        if (duration == 0)
            flashCount = 10;
        else
            flashCount = duration/timeOut;

        FLASHWINFO info;
        info.cbSize = sizeof(info);
        info.hwnd = widget->window()->winId();
        info.dwFlags = FLASHW_ALL;
        info.dwTimeout = timeOut;
        info.uCount = flashCount;

        pFlashWindowEx(&info);
    }
}
#else
void AppTools::alertWidget(QWidget *widget, int duration)
{
	QApplication::alert(widget, duration);
}
#endif
    
QString AppTools::getSoundsResourcesFolder()
{
    return getResourcesFolder()+QString(SOUNDS_DIR);
}
    
QString AppTools::getAvatarsResourcesFolder()
{
    return getResourcesFolder()+QString(AVATARS_DIR);
}
    
QString AppTools::getClipartResourcesFolder()
{
    return getResourcesFolder()+QString(CLIPART_DIR);
}
    
QString AppTools::getResourcesFolder()
{
#ifdef Q_WS_MAC
    return addPathSeparator(MacTools::resourceDir());
#else
    return QCoreApplication::applicationDirPath() + PATH_SEPARATOR;
#endif
}    
    
}
