#include "picturefilecache.h"

#include <time.h>
#include <stdlib.h>

#ifdef WIN32
#include <sys/utime.h>
#endif 

#ifdef Q_WS_MAC
#include <utime.h>
#endif 

#include "common.h"
#include "apptools.h"

namespace MoodBox
{

// Init static members
int PictureFileCache::diskCacheLimit = DISK_CACHE_LIMIT;
qint64 PictureFileCache::cacheVolume = 0;

QString PictureFileCache::diskFolder = AppTools::getAppDataFolder(CACHE_FOLDER_NAME);
QDir PictureFileCache::cacheDir = QDir();

QFileInfoList PictureFileCache::cacheFiles = QFileInfoList();

// Init singleton
PictureFileCache PictureFileCache::instance = PictureFileCache();

void PictureFileCache::clear(bool memoryOnly)
{
	QPixmapCache::clear();

	if (memoryOnly)
		return;

	// Clear cache list and files
	clearCacheFiles();
}

bool PictureFileCache::find(const QString &key, QPixmap &picture)
{
	bool memoryFound = QPixmapCache::find(key, picture);

	if (memoryFound)
		return true;

	// Check on disk
	QString fileName(getPictureFileName(key));

	if (!fileExists(fileName))
	{
		return false;
	}
	else
		// Files with null size - are default userpics
		if (QFileInfo(fileName).size() != 0 && !picture.load(fileName))
		{
			return false;
		}

	// Insert in memory
	return insert(key, picture);
}

bool PictureFileCache::getLastModifyDate(const QString &key, QDateTime &date)
{
	// Get date from list
	QString fileName(key + CACHE_PICTURES_EXT);

	foreach (QFileInfo fileInfo, cacheFiles)
	{
		if (fileInfo.fileName().toLower() == fileName.toLower())
		{
			date = fileInfo.lastModified();
			return true;
		}
	}

	return false;
}

bool PictureFileCache::insert(const QString &key, const QPixmap &picture, const QDateTime &date)
{
	bool memoryInserted = QPixmapCache::insert(key, picture);

	if (!memoryInserted)
		return false;

	// Add and save
	QString fileName(getPictureFileName(key));
	
	if (picture.isNull())
	{
		// Create empty file
		QFile f(fileName);
		f.open(QIODevice::WriteOnly | QIODevice::Truncate);
		f.close();
	}
	else
	{
		picture.save(fileName, CACHE_PICTURES_FORMAT);
	}

	setFileDateTime(fileName, date);

	addFileToList(fileName);
	checkDiskCache();

	return true;
}

void PictureFileCache::remove(const QString &key)
{
	QPixmapCache::remove(key);

	removeFile(getPictureFileName(key));
}

void PictureFileCache::setCacheDiskLimit(int n)
{
	if (n > 0)
		diskCacheLimit = n;

	checkDiskCache();
}

void PictureFileCache::setCacheDiskFolder(const QString &folder)
{
	QDir path(folder);

	if (!path.exists())
		if (!path.mkpath(folder))
			return;

	diskFolder = folder;

	initDiskCache();
}

PictureFileCache::PictureFileCache()
{
	initDiskCache();
}

void PictureFileCache::initDiskCache()
{
	// Set cache size if need
	if (PictureFileCache::cacheLimit() < MEMORY_CACHE_LIMIT)
		PictureFileCache::setCacheLimit(MEMORY_CACHE_LIMIT);

	// Check if cache disk was initialized
	if (cacheDir.absolutePath() == diskFolder)
		return;

	// Setup folder
	cacheDir = QDir(diskFolder);

	if (!cacheDir.exists())
		cacheDir.mkpath(diskFolder);

	// Sort files (older on top)
	cacheDir.setSorting(QDir::Time | QDir::Reversed);
	cacheDir.setFilter(QDir::Files);

	// Load files
	cacheFiles = cacheDir.entryInfoList();

	cacheVolume = 0;
	
	// Calculate files size
	foreach(QFileInfo fileInfo, cacheFiles)
		cacheVolume += fileInfo.size();
}

void PictureFileCache::addFileToList(const QString &fileName)
{
	QFileInfo fileInfo(fileName);
	cacheFiles.append(fileInfo);

	cacheVolume += fileInfo.size();
}

QFileInfoList::iterator PictureFileCache::findFile(const QString &fileName)
{
	QFileInfoList::iterator i;

	for(i = cacheFiles.begin(); i != cacheFiles.end(); ++i)
		if ((*i).absoluteFilePath() == fileName)
				break;
	
	return i;
}

bool PictureFileCache::fileExists(const QString &fileName)
{
	return findFile(fileName) != cacheFiles.end();
}

bool PictureFileCache::removeFile(QFileInfoList::iterator i)
{
	qint64 fileSize = (*i).size();

	if (cacheDir.remove((*i).fileName()))
	{
		cacheFiles.erase(i);
		cacheVolume -= fileSize;

		return true;
	}
	
	return false;
}

bool PictureFileCache::removeFile(const QString &fileName)
{
	QFileInfoList::iterator i = findFile(fileName);

	if (i != cacheFiles.end())
		return removeFile(i);

	return false;
}

QString PictureFileCache::getPictureFileName(const QString &key)
{
	return AppTools::addPathSeparator(cacheDir.absolutePath()) + key + CACHE_PICTURES_EXT;
}

// Check cache for overflow
void PictureFileCache::checkDiskCache()
{
	if (cacheVolume < diskCacheLimit)
		return;

	// Remove files, until free space would be enough
	for (QFileInfoList::iterator i = cacheFiles.begin(); i != cacheFiles.end() ; ++i)
		if (removeFile(i) && cacheVolume < diskCacheLimit)
			break;
}

void PictureFileCache::clearCacheFiles()
{
	for (QFileInfoList::iterator i = cacheFiles.begin(); i != cacheFiles.end() ; ++i)
		removeFile(i);
}

void PictureFileCache::setFileDateTime(QString fileName, const QDateTime& date)
{
	if (!date.isValid())
		return;

#ifdef WIN32    
	struct tm tmm = {0};	
#else
	struct tm tmm = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};	
#endif
	tmm.tm_hour = date.time().hour();
	tmm.tm_isdst = 0;
	tmm.tm_mday = date.date().day();
	tmm.tm_min = date.time().minute();
	tmm.tm_mon = date.date().month();
	tmm.tm_sec = date.time().second();
	tmm.tm_year = date.date().year() - 1900;

#ifdef WIN32    
	struct _utimbuf ut;
	ut.actime = 0;
    ut.modtime = mktime(&tmm);

	wchar_t wFileName[_MAX_PATH]; // PATH_MAX
	QDir::toNativeSeparators(fileName).toWCharArray(wFileName);
    
	_wutime(wFileName, &ut);
#endif

#ifdef Q_WS_MAC
    const char *fNamePtr = QDir::toNativeSeparators(fileName).toUtf8().data();
    
    utimbuf ut;
    ut.actime = 0;
    ut.modtime = mktime(&tmm);
        
    utime(fNamePtr, &ut);
#endif
}

}
