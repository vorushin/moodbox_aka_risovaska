#ifndef PICTUREFILECACHE_H
#define PICTUREFILECACHE_H

#include <QPixmapCache>
#include <QDateTime>
#include <QDir>

namespace MoodBox
{

// Memory cache (in kilobytes)
#define MEMORY_CACHE_LIMIT		10240
// Disk cache (in bytes)
#define DISK_CACHE_LIMIT		8192000

// Cache folder name inside app folder
#define CACHE_FOLDER_NAME		"Cache"
#define CACHE_PICTURES_EXT		".dat"
#define CACHE_PICTURES_FORMAT	"PNG"

// Class for working with picture file cache
class PictureFileCache : public QPixmapCache
{
public:
	static void clear(bool memoryOnly = true);
	static bool find(const QString &key, QPixmap &picture);
	static bool getLastModifyDate(const QString &key, QDateTime &date);
	static bool insert(const QString &key, const QPixmap &picture, const QDateTime &date = QDateTime());
	static void remove(const QString &key);

	static inline int getCacheDiskLimit() { return diskCacheLimit; };
	static void setCacheDiskLimit(int n);

	static inline QString getCacheDiskFolder() { return diskFolder; };
	static void setCacheDiskFolder(const QString &folder);

private:
	// Cache limit
	static int diskCacheLimit;

	// Current cache size
	static qint64 cacheVolume;

	// Path to disk folder
	static QString diskFolder;
	static QDir cacheDir;

	static QFileInfoList cacheFiles;

	// Singleton
	static PictureFileCache instance;

	// Singleton class
	PictureFileCache();

	static void initDiskCache();

	static void addFileToList(const QString &fileName);

	static QFileInfoList::iterator findFile(const QString &fileName);
	
	// Check if file already in cache
	static bool fileExists(const QString &fileName);
	
	static bool removeFile(QFileInfoList::iterator i);
	static bool removeFile(const QString &fileName);
	
	// Generate file name
	static QString getPictureFileName(const QString &key);
	
	// Check cache for overflow
	static void checkDiskCache();

	// Clear files
	static void clearCacheFiles();
	
	static void setFileDateTime(QString fileName, const QDateTime& date);
};

}

#endif // PICTUREFILECACHE_H
