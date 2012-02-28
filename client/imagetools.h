#ifndef IMAGETOOLS_H
#define IMAGETOOLS_H

#include <QImage>
#include <QByteArray>

class QWidget;

namespace MoodBox
{

// Utility class for image operations
class ImageTools
{
public:
	// Save QImage to bytes using alternative engine
	static QByteArray saveToBytesAlternative(const QImage &image);

	static void getSmallestImageContent(const QImage &image, QByteArray &content, QString &format);

	// Save image to file using standard save dialog
	static void saveImageToFile(const QImage &image, QWidget *parent, bool useAlternative = true);

private:
	static void initEngines();
};

}
#endif // IMAGETOOLS_H