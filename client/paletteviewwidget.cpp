#include "paletteviewwidget.h"
#include "testtools.h"

#include <QPainter>

namespace MoodBox 
{

PaletteViewWidget::PaletteViewWidget(QWidget *parent)
	: QWidget(parent)
{
	TimeMeasure t("PaletteViewWidget");
}

void PaletteViewWidget::setPalette(const Palette &palette)
{
	this->palette = palette;

	update();
}

void PaletteViewWidget::paintEvent(QPaintEvent *)
{
	QPainter painter(this);
	painter.setPen(Qt::NoPen);

	// background image
	QImage img(":/MoodBox/Resources/palette_view_widget.png");
	QPainter imgPainter(&img);

	imgPainter.setCompositionMode(QPainter::CompositionMode_Multiply);

	// draw all the colors
	int xPositions[PALETTE_COLOR_COUNT / 2] = {5, 24, 43, 63, 82, 102, 121, 141};
	int widths[PALETTE_COLOR_COUNT / 2] = {18, 18, 19, 18, 19, 18, 19, 17};

	for (int i = 0; i < PALETTE_COLOR_COUNT; i++) 
	{
		int x = xPositions[i % 8];
		int y = 4 + (i / 8) * 17;

		imgPainter.fillRect(x, y, widths[i % 8], 16, palette.getColor(i));
	}

	imgPainter.fillRect(xPositions[0] - 1, 5, 1, 15, palette.getColor(0));
	imgPainter.fillRect(xPositions[0] - 1, 21, 1, 15, palette.getColor(8));
	imgPainter.fillRect(xPositions[7] + widths[7], 5, 1, 15, palette.getColor(7));
	imgPainter.fillRect(xPositions[7] + widths[7], 21, 1, 15, palette.getColor(15));	

	painter.drawImage(QPoint(0, 0), img);
}

}