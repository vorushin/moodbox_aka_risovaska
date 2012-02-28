#include "imagetools.h"

#include <QApplication>
#include <QColor>
#include <QBuffer>
#include <QFileDialog>
#include <QWidget>

#include "common.h"
#include "imageelement.h"
#include "messagefile.h"
#include "international.h"
#include "messageorganizer.h"

#include <Magick++.h>
#include <string>

using namespace Magick;
using namespace Velasquez;

namespace MoodBox
{

QByteArray ImageTools::saveToBytesAlternative(const QImage &image)
{
	initEngines();

	// Copy to Magick image
	Image mImage(Geometry(image.width(), image.height()), Color(MaxRGB, MaxRGB, MaxRGB, 0));

	Pixels mCache(mImage);
	PixelPacket* pixels;
	pixels = mCache.get(0, 0, image.width(), image.height());

	for (int y = 0; y < image.height(); y++)
	{
		QRgb* rgb = (QRgb*)image.scanLine(y);
    	for (int x = 0; x < image.width(); x++)		
		{
			QColor color(*rgb);
			Color mColor = ColorRGB(color.redF(), color.greenF(), color.blueF());
			*(pixels++) = mColor;
			++rgb;
		}
	}

	mCache.sync();

	// Save to blob
	Blob mData;

	mImage.magick(ALTERNATE_IMAGE_FORMAT);
	mImage.quality(ALTERNATE_IMAGE_QUALITY);
	mImage.write(&mData);

	const char *data = (const char*) mData.data();

	// Convert to QByteArray
	return QByteArray(data, mData.length());
}

void ImageTools::getSmallestImageContent(const QImage &image, QByteArray &content, QString &format)
{
	QByteArray defaultData;
	QBuffer buffer(&defaultData);	

	// Try default
	buffer.open(QIODevice::WriteOnly);
	image.save(&buffer, DEFAULT_IMAGE_FORMAT);

	qint64 defaultSize = defaultData.size();
	buffer.close();	

	// Try alternative
	QByteArray alternativeData = ImageTools::saveToBytesAlternative(image);
	qint64 alternateSize = alternativeData.size();

	if (alternateSize < defaultSize)
	{
		content = alternativeData;
		format = ALTERNATE_IMAGE_FORMAT;
	}
	else
	{
		content = defaultData;
		format = DEFAULT_IMAGE_FORMAT;
	}

	format = MetaInfoProvider::getTitleString(METAINFO_IMAGEFORMAT_TITLE, MetaInfoProvider::getTagString(METAINFO_IMAGEFORMAT_TAGNAME, METAINFO_IMAGEFORMAT_VALUEPREFIX + format));
}

void ImageTools::saveImageToFile(const QImage &image, QWidget *parent, bool useAlternative)
{
	static QString filter = QObject::tr(SAVE_FILTER).arg("*.jpg") + ";;" + QObject::tr(SAVE_FILTER).arg("*.png");
	static QString lastSaveFolder;

	QString fileName = QFileDialog::getSaveFileName(parent, QObject::tr(SAVE_TITLE), lastSaveFolder, filter);

	if (fileName.isEmpty())
		return;

	bool isJpg = fileName.endsWith(".jpg");

	if (useAlternative && isJpg)
	{
		QByteArray imageData = saveToBytesAlternative(image);
		QFile file(fileName);
		file.open(QIODevice::WriteOnly);
		file.write(imageData);
		file.close();
	}
	else
	{
		int quality = isJpg ? 100 : -1;

                if (QFileInfo(fileName).suffix().isEmpty())
                    fileName += MESSAGES_PREVIEW_FILE_EXT;

		image.save(fileName, NULL, quality);
	}

	lastSaveFolder = QFileInfo(fileName).absolutePath();
}

void ImageTools::initEngines()
{
	static int initStatus = 0;

	if (initStatus != 0)
		return;

	InitializeMagick(*qApp->argv());
	
	initStatus = 1;
}

}
