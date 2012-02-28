#include "brushstyle.h"

#include <QPainter>

QPixmap getBrushSample(qreal size, const QSize &sampleSize, qreal alpha, const QColor &color)
{
	QPixmap samplePixmap(sampleSize);
	
	QColor backgroundColor = QColor(Qt::transparent);

	QColor penColor = color;
	penColor.setAlphaF(alpha);

	samplePixmap.fill(backgroundColor);

	QPainter painter(&samplePixmap);

	painter.setPen(penColor);
	painter.setBrush(QBrush(penColor));

	QPointF center(samplePixmap.width() / 2, samplePixmap.height() / 2);

	painter.drawEllipse(center, size / 2, size / 2);
	
	return samplePixmap;
}
