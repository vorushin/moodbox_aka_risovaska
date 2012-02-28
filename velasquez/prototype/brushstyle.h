#ifndef BRUSHSTYLE_H
#define BRUSHSTYLE_H

#include <QPixmap>

// Size
#define BRUSH_SIZE_COUNT			12

const qreal BrushSizes [BRUSH_SIZE_COUNT] = {2, 3, 5, 12, 15, 20, 25, 30, 40, 50, 60, 75};

#define DEFAULT_BRUSH_SIZE_INDEX	0

// Transparency
#define BRUSH_ALPHA_COUNT			6

const qreal BrushAlphas [BRUSH_ALPHA_COUNT] = {0, 0.1, 0.25, 0.50, 0.75, 1};

#define DEFAULT_BRUSH_ALPHA_INDEX	5

// Returns brush sample image
QPixmap getBrushSample(qreal size, const QSize &sampleSize, qreal alpha = 1, const QColor &color = QColor(Qt::black));

#endif // BRUSHSTYLE_H
