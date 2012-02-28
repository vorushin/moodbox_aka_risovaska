#include "imageelement.h"

#include <QFile>
#include <QStyleOptionGraphicsItem>
#include <QPainter>
#include <QBuffer>
#include <QImageReader>

#include "debug.h"

namespace Velasquez
{

ImageElement::ImageElement(QGraphicsItem *parentItem)
	: TransformableElement(parentItem)
{
	setCursor(Qt::PointingHandCursor);
}

ImageElement::ImageElement(const QString &fileName, QGraphicsItem *parentItem)
	: TransformableElement(parentItem)
{
	QImageReader imageReader(fileName);
	this->format = imageReader.format();

	this->image = imageReader.read();
	rect.setSize(image.size());

	setCursor(Qt::PointingHandCursor);
}

ImageElement::ImageElement(const QImage &image, QGraphicsItem *parentItem)
	: TransformableElement(parentItem)
{
	this->image = image;
	this->format = DEFAULT_IMAGE_FORMAT;
	rect.setSize(image.size());

	setCursor(Qt::PointingHandCursor);
}

bool ImageElement::isEmpty() const
{
	return image.isNull();
}

void ImageElement::setSetting(qint32 id, const QVariant &value) 
{
	Q_UNUSED(id)
	Q_UNUSED(value)
}

QVariant ImageElement::getSetting(qint32 id) const 
{ 
	Q_UNUSED(id)
	
	return QVariant(); 
}

void ImageElement::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
	painter->setRenderHint(QPainter::SmoothPixmapTransform, true);

    painter->drawImage(rect.topLeft(), image);

	TransformableElement::paint(painter, option, widget);
}

QRectF ImageElement::boundingRect() const
{
	return rect;
}

qint32 ImageElement::getType() const
{
	return Type;
}

}
