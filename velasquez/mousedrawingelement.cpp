#include "mousedrawingelement.h"

namespace Velasquez
{

MouseDrawingElement::MouseDrawingElement(QGraphicsItem *parent)
	: DrawingElement(parent), currentColor(Qt::black), currentAlpha(1), currentWidth(1), drawingNow(false)
{
	setCacheMode(DeviceCoordinateCache);
}

QRectF MouseDrawingElement::boundingRect() const 
{ 
	return rect; 
}

void MouseDrawingElement::setSetting(qint32 id, const QVariant &value)
{
	switch (id)
	{
		case PenColor:
			if (!value.canConvert<QColor>())
				return;

			setColor(value.value<QColor>());

			break;

		case PenTransparency:

			if (!value.canConvert<qreal>())
				return;
			
			setAlpha(value.toDouble());
			
			break;

		case PenWidth:
			if (!value.canConvert<qreal>())
				return;
			
			setWidth(value.value<qreal>());

			break;
	}
}
	
QVariant MouseDrawingElement::getSetting(qint32 id) const
{
	switch (id)
	{
		case PenColor: return currentColor;
		case PenTransparency: return currentAlpha;
		case PenWidth: return currentWidth;
	}

	return QVariant();
}

QList <qint32> MouseDrawingElement::getSettingsList() const
{
	QList<qint32> settings;

	if (hasColor())
		settings << PenColor;
	
	if (hasAlpha())
		settings << PenTransparency;
	
	if (hasWidth())
		settings << PenWidth;

	return settings;
}

bool MouseDrawingElement::isSettingReversible(qint32 id) const
{
	Q_UNUSED(id)

	// No reversible settings in mouse elements
	return false;
}

void MouseDrawingElement::startDrawing()
{
	drawingNow = true;
}

void MouseDrawingElement::finishDrawing()
{
	drawingNow = false;
}

void MouseDrawingElement::updateRect(const QRectF &pointRect)
{
	QRectF newRect = rect.united(pointRect);

	if (newRect == rect)
		return;
	
	prepareGeometryChange();
	rect = newRect;
}

void MouseDrawingElement::setColor(const QColor &color)
{
	this->currentColor = color; 
}

void MouseDrawingElement::setAlpha(qreal alpha)
{
	this->currentAlpha = alpha; 
}

void MouseDrawingElement::setWidth(qreal width)
{
	this->currentWidth = width; 
}

}