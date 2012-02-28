#include "penelement.h"

#include <QPainter>
#include <QHash>

#include "debug.h"

namespace Velasquez
{

// Class for loading and manipulating of pen images
class PenImagesManager
{
public:
	PenImagesManager() : imagesCount(-1), imageWidth(-1)
	{
	};

	int getImagesCount()
	{
		if (imagesCount < 0)
			load();

		return imagesCount;
	};

	int getImageWidth()
	{
		if (imageWidth < 0)
			load();

		return imageWidth;
	};

	inline int getImageHeight() 
	{
		return PEN_SPACING;
	}

	QImage getAlphaImage(int index)
	{
		if (alphaImages.isEmpty())
			load();

		return alphaImages[index];
	};

private:
	int imagesCount;
	int imageWidth;

	QHash <qint32, QImage> alphaImages;

	void load()
	{
		QImage image(PEN_IMAGE);

		// Create rect for the image
		QRect imageRect(0, 0, image.width(), getImageHeight());

		imagesCount = image.height() / getImageHeight();
		imageWidth = image.width();

		for (int i = 0; i < imagesCount; i ++)
		{
			imageRect.moveTo(imageRect.x(), getImageHeight() * i);
			alphaImages.insert(i, image.copy(imageRect).alphaChannel());
		}
	};
};

// Global pen images manager
PenImagesManager penImagesManager;

// PenElement class
PenElement::PenElement(QGraphicsItem *parent) 
	: BrushDrawingElement(parent)
{
	currentColor = DEFAULT_PEN_COLOR;
	currentWidth = DEFAULT_PEN_WIDTH;
	currentAlpha = currentColor.alphaF();

	spacing = currentWidth / penImagesManager.getImageWidth() * PEN_SPACING;
}

qint32 PenElement::getType() const
{
	return Type;
}

qreal PenElement::getPaintTransparency(qreal speed) const
{
	// Minimum transparency
	static const qreal baseTransparency = 0.5;
	// Maximum speed
	static const qreal maxSpeed = 3.0;

	// Fit the speed into 0..1 range
	speed = qMin(speed, maxSpeed) / maxSpeed;

	qreal transparency = baseTransparency;

	transparency += speed * (1 - baseTransparency);

	return transparency;
}

qreal PenElement::getPaintAngle(qreal strokeAnlge) const
{
	return strokeAnlge - 90;
}

void PenElement::setAlpha(qreal alpha)
{
	int pos = qMin((int)alpha, PEN_TRANSPARENCY_COUNT - 1);
		
	alpha = PenTransparencies[pos];

	BrushDrawingElement::setAlpha(alpha);
}

void PenElement::setWidth(qreal width)
{
	width++;

	qreal scale = width / penImagesManager.getImageWidth();
	spacing = scale * PEN_SPACING;
		
	if (width <= 2)
		spacing = 1;

	BrushDrawingElement::setWidth(width);
}

void PenElement::paintStroke(QPainter *painter, int index)
{
	BrushStroke stroke = strokes.value(index);
	int imageIndex = index % penImagesManager.getImagesCount();

	// Set alpha
	QColor imageColor = currentColor;
	imageColor.setAlphaF(currentAlpha * getPaintTransparency(stroke.getSpeed()));

	// Create image
	QImage strokeImage(penImagesManager.getImageWidth(), penImagesManager.getImageHeight(), QImage::Format_ARGB32);
	strokeImage.fill(imageColor.rgba());

	// Apply alpha
	strokeImage.setAlphaChannel(penImagesManager.getAlphaImage(imageIndex));

	// Apply scale and transformation
	strokeImage = strokeImage.scaled(currentWidth, getSpacing(), Qt::IgnoreAspectRatio, Qt::SmoothTransformation);
	strokeImage = strokeImage.transformed(QTransform().rotate(getPaintAngle(stroke.getAngle())));

	// Finally paint
	painter->drawImage(getStrokeRect(index).topLeft(), strokeImage);
}
	
QRectF PenElement::getStrokeRect(int index) const
{
	// The size is always the same
	QSizeF strokeSize(getSpacing(), currentWidth);

	BrushStroke stroke = strokes.at(index);

	QTransform strokeTransform;

	QRectF strokeRect(stroke, strokeSize);
	strokeRect = strokeRect.translated(-strokeRect.width() / 2, -strokeRect.height());

	// Apply transformation
	strokeTransform.translate(strokeRect.center().x(), strokeRect.center().y());
	strokeTransform.rotate(getPaintAngle(stroke.getAngle()));
	strokeTransform.translate(-strokeRect.center().x(), -strokeRect.center().y());

	strokeRect = strokeTransform.mapRect(strokeRect);

	return strokeRect;
}

}