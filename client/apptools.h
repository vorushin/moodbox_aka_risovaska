#ifndef APPTOOLS_H
#define APPTOOLS_H

#include <QString>

class QWidget;

// Utility functions
namespace MoodBox
{

#ifndef DELETE_AND_NULL
#define DELETE_AND_NULL(x)	delete x; x = NULL
#endif

#define MESSAGE_ALERT_DURATION	5000

#define AVATARS_DIR     "Avatars"
#define SOUNDS_DIR      "Sound"
#define CLIPART_DIR     "Clipart"
    
class AppTools
{
public:

	enum AppDataFolderType { Own, Local, Common };

	// Path to executable
	static QString getPathToExe();

	// Path to executable + suffix dir
	static QString getPathRelativeToExeDir(const QString &suffixDir);

	// Add path separator to end of path
	static QString addPathSeparator(const QString &path);

	// Get standard app folder
	static QString getPortableAppFolder(AppDataFolderType folderType);

	// Get application folder with postfix
	static QString getAppDataFolder(const QString &postfix, AppDataFolderType folderType = Own);

	// Blink widget window during specified time
	static void alertWidget(QWidget *widget, int duration = MESSAGE_ALERT_DURATION);
    
    static QString getSoundsResourcesFolder();
    static QString getAvatarsResourcesFolder();
    static QString getClipartResourcesFolder();
    static QString getResourcesFolder();
};

}

#endif // APPTOOLS_H
