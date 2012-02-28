#ifndef BRUSHSTYLE_H
#define BRUSHSTYLE_H

#include <QPixmap>

namespace MoodBox
{

// Brush size/alpha are measured in index with predefined maximum count of indices,
// while drawing index is converted to pen size.

// Size
#define BRUSH_SIZE_COUNT				7
#define DEFAULT_BRUSH_SIZE_INDEX		3

// Alpha
#define BRUSH_ALPHA_COUNT				7
#define DEFAULT_BRUSH_ALPHA_INDEX		6

// Samples
#define BRUSH_SAMPLE_SIDE				18

#define BRUSH_SAMPLE_BACKGROUND			":/MoodBox/Resources/brush_sample_bg.png"
#define BRUSH_SAMPLE_SELECTED_BG		":/MoodBox/Resources/brush_sample_selected_bg.png"

// Brush style & samples
class BrushStyle
{
public:
	enum BrushStyleEnum { Size, Alpha };

	// Return brush size sample
	static QPixmap getSizeSample(int sizeIndex, bool selected, const QColor &color = QColor(), bool emptyBackground = false);

	// Return brush alpha sample
	static QPixmap getAlphaSample(int alphaIndex, int sizeIndex, bool selected, bool emptyBackground = false);

	// Return sample size
	static QSize getSampleSize();

private:
	static qreal getScale(int sizeIndex);
	static qreal getAlpha(int alphaIndex);

	static QPixmap getSample(qreal scale, qreal alpha, bool selected, const QColor &color, bool emptyBackground);
};

}
#endif // BRUSHSTYLE_H
