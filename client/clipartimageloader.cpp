#include "clipartimageloader.h"

#include <QApplication>
#include <QDesktopWidget>
#include <QPainter>
#include <QSvgRenderer>
#include <QtConcurrentRun>

#include "clipartentrieslist.h"
#include "debug.h"

namespace MoodBox
{

int ClipartImageLoader::maxIconSize = 0;

ClipartImageLoader::ClipartImageLoader(QObject *parent, bool useRequestOrder)
	: QObject(parent), useRequestOrder(useRequestOrder)
{
	connect(&watcher, SIGNAL(finished()), this, SLOT(onJobFinished()));
	connect(qApp, SIGNAL(aboutToQuit()), this, SLOT(clear()));

	if (maxIconSize == 0)
	{
		QDesktopWidget *desktop = qApp->desktop();
		for (int i = 0; i < desktop->numScreens(); i++)
		{
			maxIconSize = qMax(desktop->screenGeometry(i).width(), maxIconSize);
		}
		Q_ASSERT_X(maxIconSize > 0, "ClipartImageLoader::ClipartImageLoader", "Unable to detect max screen width");
		if (maxIconSize <= 0) maxIconSize = 400;
		maxIconSize /= CLIPART_COLUMNS;
	}
}

int ClipartImageLoader::loadPicture(const QString &imageFile, int id)
{
	QImage original;

	bool isLoaded = false;

	if (imageFile.endsWith(CLIPART_NOT_SCALED_PREVIEW_EXT)) // Handle SVG separately
	{
		QSvgRenderer svg(imageFile);
		isLoaded = svg.isValid();
		if (isLoaded)
		{
			QSize svgSize = svg.defaultSize();
			svgSize.scale(maxIconSize, maxIconSize, Qt::KeepAspectRatio);
			original = QImage(svgSize, QImage::Format_ARGB32);
			original.fill(0);
			QPainter painter(&original);
			svg.render(&painter);
		}
	}
	else
	{
		isLoaded = original.load(imageFile);
	}
	
	if (isLoaded)
	{
		lastResult = ClipartImageLoader::generateItemImages(imageFile, original);
	}
	else
	{
		lastResult.icon = QImage();
		lastResult.preview = QImage();
	}

	return id;
}

void ClipartImageLoader::loadImages(const QString &fileName, int id)
{
	if (useRequestOrder)
		requestOrder.push(id);
	if (jobs.contains(id)) return;

	jobs.insert(id, fileName);
	runJobs();
}

ItemImages ClipartImageLoader::generateItemImages(const QString &fileName, const QImage &originalImage)
{
	ItemImages images;

	images.file = fileName;
	images.icon = originalImage;
	images.preview = originalImage;
	
	if (images.icon.width() > maxIconSize || images.icon.height() > maxIconSize)
	{
		images.icon = images.icon.scaled(maxIconSize, maxIconSize, Qt::KeepAspectRatio);
	}

	// Make scaled preview
	if (images.icon.width() > PICTURE_PREVIEW_WIDTH || images.icon.height() > PICTURE_PREVIEW_HEIGHT)
		images.preview = images.icon.scaled(PICTURE_PREVIEW_WIDTH, PICTURE_PREVIEW_HEIGHT, Qt::KeepAspectRatio);
	
	if (fileName.endsWith(CLIPART_NOT_SCALED_PREVIEW_EXT))
	{
		// Upsample too small icons
		if (images.icon.width() < maxIconSize && images.icon.height() < maxIconSize)
		{
			images.icon = images.icon.scaled(maxIconSize, maxIconSize, Qt::KeepAspectRatio);
		}
	}

	return images;
}

int ClipartImageLoader::getMaxIconSize()
{
	return maxIconSize;
}

void ClipartImageLoader::clear()
{
	jobs.clear();
	requestOrder.clear();
	watcher.waitForFinished();
}

void ClipartImageLoader::runJobs()
{
	if (jobs.isEmpty())
	{
		QDEBUG("ClipartImageLoader::runJobs: job list is empty");
		return;
	}

	if (!watcher.isRunning())
	{
		// Get next item to process
		int id = -1;
		if (useRequestOrder)
		{
			do {
				id = requestOrder.pop();
			} while (!requestOrder.isEmpty() && !jobs.contains(id));
		}
		else
		{
			id = jobs.begin().key();
		}

		if (!jobs.contains(id))
		{
			QDEBUG("ClipartImageLoader::runJobs: no more jobs to process");
			return;
		}

		QFuture<int> future = QtConcurrent::run(this, &MoodBox::ClipartImageLoader::loadPicture, jobs.value(id), id);
		watcher.setFuture(future);
	}
}

void ClipartImageLoader::onJobFinished()
{
	int id = watcher.result();

	if (!jobs.isEmpty() && jobs.contains(id))
	{
		jobs.remove(id);
		emit imagesLoaded(lastResult, id);
	}
	else
	{
		QDEBUG("ClipartImageLoader::onJobFinished: job result discarded, id" << id);
	}
	runJobs();
}

}
