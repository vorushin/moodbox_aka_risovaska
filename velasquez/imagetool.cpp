#include "imagetool.h"

#include <QGraphicsSceneMouseEvent>
#include <QGraphicsScene>
#include <QMimeData>
#include <QUrl>

#include <QNetworkRequest>
#include <QNetworkReply>
#include <QNetworkAccessManager>

#include "imageelement.h"
#include "vcommon.h"

namespace Velasquez
{

ImageTool::ImageTool(QObject *parent)
	: TransformableTool(parent), loader(NULL)
{
}

ImageTool::~ImageTool()
{
    clearLoadRequests();
}

qint32 ImageTool::getElementType() const
{
	return ImageElement::Type;
}

QStringList ImageTool::getFileExtensions() const
{
	static QStringList ext = QString(SUPPORTED_IMAGE_FORMATS).split(" ");

	return ext;
}

bool ImageTool::canCreate(QGraphicsSceneMouseEvent *mouseEvent) const
{
	Q_UNUSED(mouseEvent)
	
	return false;
}

bool ImageTool::canCreate(QKeyEvent *keyEvent) const 
{ 
	Q_UNUSED(keyEvent)
	
	return false; 
}

bool ImageTool::canCreate(const QMimeData *data) const
{
	return isMimeSupported(data) || data->hasImage();
}

void ImageTool::createElement(QGraphicsSceneMouseEvent *mouseEvent)
{
	Q_UNUSED(mouseEvent)
}

void ImageTool::createElement(QKeyEvent *keyEvent)
{
	Q_UNUSED(keyEvent)
}

void ImageTool::createElement(const QMimeData *data, const QPointF &pos)
{
	ImageElement *element;
	
	// If we deal with image - we use it first
	if (data->hasImage())
	{
		element = new ImageElement(qvariant_cast<QImage>(data->imageData()));
		setupNewElement(element, getCenteredElementPosition(pos, element));
		
		return;
	}

	QPointF addPos;
	QStringList files = getSupportedFilesFromMime(data);
	
	foreach (QString fileName, files)
	{
        if (QUrl(fileName).toLocalFile().isEmpty())
        {
			loadImageFromUrl(fileName, pos);
        }
        else
        {
            element = new ImageElement(fileName);
            QPointF newPos = getCenteredElementPosition(pos, element) + addPos;
            setupNewElement(element, newPos);
            addPos = getNextElementPosition(addPos);
        }
	}
}

void ImageTool::createElement(const QImage &data, const QPointF &pos)
{
	ImageElement *element = new ImageElement(data);
	setupNewElement(element, pos);
}

bool ImageTool::isMimeSupported(const QMimeData *data) const
{
	bool isSupported = TransformableTool::isMimeSupported(data);

	// If there is the link to image we support it
	return (!isSupported) ? !getImageUrl(data).isEmpty() : isSupported;
}

QStringList ImageTool::getSupportedFilesFromMime(const QMimeData *data) const
{
	QStringList supportedList = TransformableTool::getSupportedFilesFromMime(data);
	QString imageUrl = getImageUrl(data);

	if (!imageUrl.isEmpty())
		supportedList << imageUrl;

	return supportedList;
}

void ImageTool::loadImageFromUrl(const QString &url, const QPointF &pos)
{
	if (loader == NULL)
	{
		loader = new QNetworkAccessManager(this);

		connect(loader, SIGNAL(finished(QNetworkReply*)), this, SLOT(onImageLoadReply(QNetworkReply*)));
	}

	QUrl u(url);
	QNetworkRequest request(u);
	QNetworkReply *reply = loader->get(request);

	loadRequests.insert(reply, pos);
}

void ImageTool::clearLoadRequests()
{
	foreach (QNetworkReply *reply, loadRequests.keys())
		delete reply;

	loadRequests.clear();

	DELETE_AND_NULL(loader);
}

QString ImageTool::getImageUrl(const QMimeData *data)
{
    if (data->hasHtml())
    {
        QString htmlText = data->html().remove(QChar(0));

        int startImageLink = htmlText.indexOf("<img ");
        int endImageLink = -1;

        if (startImageLink > -1)
        {
            startImageLink = htmlText.indexOf("src=\"", startImageLink);

            if (startImageLink > -1)
            {
                endImageLink = htmlText.indexOf("\"", startImageLink + 5);
                if (endImageLink > -1)
                {
                    return htmlText.mid(startImageLink + 5, endImageLink - startImageLink - 5);
                }
            }
        }
    }

    return QString();
}

void ImageTool::onImageLoadReply(QNetworkReply *reply)
{
	if (reply != NULL && reply->error() == QNetworkReply::NoError)
	{
		QByteArray data = reply->readAll();

		if (!data.isEmpty())
		{
			QImage image;
			image.loadFromData(data);

			if (!image.isNull())
			{
                ImageElement *element = new ImageElement(image);
                QPointF newPos = getCenteredElementPosition(loadRequests[reply], element);

                setupNewElement(element, newPos);
			}

			loadRequests.remove(reply);
		}
	}

	if (reply != NULL)
		reply->deleteLater();

	if (loadRequests.isEmpty())
		DELETE_AND_NULL(loader);
}

}
