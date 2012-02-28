#ifndef IMAGEELEMENT_H
#define IMAGEELEMENT_H

#include "transformableelement.h"

namespace Velasquez
{

// Default image format
#define DEFAULT_IMAGE_FORMAT		"png"

class ImageElement : public TransformableElement
{
	Q_OBJECT

public:
	enum {Type = 10002};

public:
	ImageElement(QGraphicsItem *parentItem = 0);
	ImageElement(const QString &fileName, QGraphicsItem *parentItem = 0);
    ImageElement(const QImage &image, QGraphicsItem *parentItem = 0);

	virtual bool isEmpty() const;
	
	virtual void setSetting(qint32 id, const QVariant &value);
	virtual QVariant getSetting(qint32 id) const;
	inline virtual QList <qint32> getSettingsList() const { return QList<qint32>(); };

	virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget = 0);
	virtual QRectF boundingRect() const;

protected:
	virtual qint32 getType() const;

private:
    QRectF rect;
	QImage image;
	QByteArray format;
};

}

#endif // IMAGEELEMENT
