#include "brushstyle.h"

#include <QPainter>
#include <QLinearGradient>

namespace MoodBox
{

// Return brush size sample
QPixmap BrushStyle::getSizeSample(int sizeIndex, bool selected, const QColor &color, bool emptyBackground)
{
	if (sizeIndex < 0 || sizeIndex >= BRUSH_SIZE_COUNT)
		return QPixmap();
	else
		return getSample(getScale(sizeIndex), 1.0, selected, color, emptyBackground);
}

// Return brush alpha sample
QPixmap BrushStyle::getAlphaSample(int alphaIndex, int sizeIndex, bool selected, bool emptyBackground)
{
	if (alphaIndex < 0 || alphaIndex >= BRUSH_ALPHA_COUNT || sizeIndex < 0 || sizeIndex >= BRUSH_SIZE_COUNT)
		return QPixmap();
	else
		return getSample(getScale(sizeIndex), getAlpha(alphaIndex), selected, QColor(), emptyBackground);
}

QSize BrushStyle::getSampleSize()
{
	static QSize size = QPixmap(BRUSH_SAMPLE_BACKGROUND).size();

	return size;
}

qreal BrushStyle::getScale(int sizeIndex)
{
	static const qreal MinimalScale = 0.4;

	qreal scale = (1 - MinimalScale) / (BRUSH_SIZE_COUNT - 1) * sizeIndex + MinimalScale;
	return scale;
}

qreal BrushStyle::getAlpha(int alphaIndex)
{
	return (alphaIndex + 1.0) / BRUSH_ALPHA_COUNT;
}

QPixmap BrushStyle::getSample(qreal scale, qreal alpha, bool selected, const QColor &color, bool emptyBackground)
{
	QPixmap backgroundPixmap( (selected) ? BRUSH_SAMPLE_SELECTED_BG : BRUSH_SAMPLE_BACKGROUND);

	if (emptyBackground)
		backgroundPixmap.fill(Qt::transparent);

	// Scale the sample
	QSizeF pointSize(BRUSH_SAMPLE_SIDE, BRUSH_SAMPLE_SIDE);

	if (scale != 1.0)
	{
		pointSize.setWidth(pointSize.width() * scale);
		pointSize.setHeight(pointSize.height() * scale);
	}

	// Apply color and alpha
	QColor alphaColor(Qt::black);

	if (color.isValid())
		alphaColor = color;

	if (alpha != 1.0)
		alphaColor.setAlphaF(alpha);
	
	// Draw sample in the middle of background
	QPainter painter(&backgroundPixmap);
	painter.setRenderHint(QPainter::Antialiasing);

	QRectF pointRect(QPointF(0, 0), pointSize);
	pointRect.moveTo(backgroundPixmap.width() / 2 - pointSize.width() / 2, backgroundPixmap.height() / 2 - pointSize.height() / 2);

	if (color.isValid() && color.value() > 210 && color.saturation() < 50)
		painter.setPen("#d0d0d0");
	else
		painter.setPen(Qt::NoPen);

	painter.setBrush(alphaColor);
	painter.drawEllipse(pointRect);

	painter.end();

	return backgroundPixmap;
}

}