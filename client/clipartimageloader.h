#ifndef CLIPARTIMAGELOADER_H
#define CLIPARTIMAGELOADER_H

#include <QFuture>
#include <QFutureWatcher>
#include <QHash>
#include <QImage>
#include <QObject>
#include <QStack>

namespace MoodBox
{

// Preview dimensions
#define PICTURE_PREVIEW_WIDTH           96
#define PICTURE_PREVIEW_HEIGHT          96

// Do not scale vector clipart
#define CLIPART_NOT_SCALED_PREVIEW_EXT	"svg"

// Images used by clipart list item
struct ItemImages
{
	QImage preview, icon;
	QString file;
};

typedef QPair<ItemImages,int> LoadResult;

class ClipartImageLoader : public QObject
{
	Q_OBJECT

public:
	ClipartImageLoader(QObject *parent, bool useRequestOrder = true);

	// Generates several versions of image from specified file, used by QtConcurrent call
	int loadPicture(const QString &imageFile, int id);

	void loadImages(const QString &fileName, int id);
	// Generates icon and preview images from original one and write them all in ItemImages
	static ItemImages generateItemImages(const QString &fileName, const QImage &originalImage);

	static int getMaxIconSize();

signals:
	void imagesLoaded(ItemImages images, int id);

public slots:
	void clear();

protected:
	QMap<int,QString> jobs;
	QFutureWatcher<int> watcher;
	ItemImages lastResult;
	QStack<int> requestOrder;
	bool useRequestOrder;
	static int maxIconSize;

	void runJobs();

protected slots:
	void onJobFinished();
};

}

#endif // CLIPARTIMAGELOADER_H
