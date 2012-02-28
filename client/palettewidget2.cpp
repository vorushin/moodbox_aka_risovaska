#include "palettewidget2.h"

#include <QMouseEvent>
#include <QPainter>
#include <QPaintEvent>

#include "colorwidget.h"
#include "testtools.h"

namespace MoodBox
{

PaletteWidget2::PaletteWidget2(QWidget *parent)
	: QWidget(parent), selectedItem(-1), hoveredItem(-1), requireNewColor(true), colorEditor(NULL)
{
	TimeMeasure t("PaletteWidget2");

	setMouseTracking(true);

	for(int i = 0; i < PALETTE_COLOR_COUNT; i++)
	{
		int width = 19;
		int x = 4;

		if (i > 7) 
		{
			x += width + 1;
		}

		int height = 16;
		int y = 4 + (i % 8) * (height + 1);
		
		rects[i] = QRect(x, y, width, height);
	}
}

void PaletteWidget2::reset()
{
	setPalette(Palette());	
}

void PaletteWidget2::setSelectedColorIndex(const int i)
{
	if (i != selectedItem && i > -1 && i < PALETTE_COLOR_COUNT)
	{
		selectedItem = i;
		update();

		emit colorSelected(i);
	}
}

void PaletteWidget2::setRequireNewColor(bool requireNew)
{
	if (requireNewColor == requireNew)
		return;

	requireNewColor = requireNew;
}

void PaletteWidget2::setPalette(const Palette &palette)
{
	if (palette == this->palette)
		return;

	this->palette = palette;
	update();	
}

void PaletteWidget2::updateSelectedColor(const QColor &color)
{
	onColorSelected(Color(color));
}

void PaletteWidget2::paintEvent(QPaintEvent *event)
{
	// hint: use event->rect() to repaint small pieces (when single color is changed) if painting is slow

	QPainter painter(this);
	painter.setPen(Qt::NoPen);

	for(int i = 0; i < PALETTE_COLOR_COUNT; i++)
	{
		QImage img;
		if (i == selectedItem)
		{
			img = ColorWidget::getSelectedMask();
			QPainter imgPainter(&img);
			imgPainter.setCompositionMode(QPainter::CompositionMode_Multiply);
			imgPainter.fillRect(img.rect(), palette.getColor(i));	
		}
		else if (i == hoveredItem)
		{
			img = ColorWidget::getHoverMask();
			QPainter imgPainter(&img);
			imgPainter.setCompositionMode(QPainter::CompositionMode_DestinationAtop);
			imgPainter.fillRect(img.rect(), palette.getColor(i));
		}
		else 
		{
			img = ColorWidget::getNormalMask();
			QPainter imgPainter(&img);
			imgPainter.setCompositionMode(QPainter::CompositionMode_Multiply);
			imgPainter.fillRect(img.rect(), palette.getColor(i));
		}

		painter.drawImage(rects[i], img);
	}

	QImage img(":/MoodBox/Resources/palette_widget.png");
	painter.drawImage(0, 0, img);

	event->accept();

	QWidget::paintEvent(event);
}

void PaletteWidget2::mousePressEvent(QMouseEvent *event)
{
	if (event->button() == Qt::LeftButton)
	{
		event->accept();

		int i = getRectIndex(event->pos());
		if (i == -1) // no new selection
		{
			return;
		}
		if (i != selectedItem) // first click, color selecting
		{
			selectedItem = i;
			update();

			emit colorSelected(i);
		}
		else // second click, color editing
		{
			if (colorEditor == NULL)
			{
				colorEditor = new ColorEditor(this);
				connect(colorEditor, SIGNAL(colorSelected(const Color &)), this, SLOT(onColorSelected(const Color &)));
			}

			colorEditor->setColorAndMove(palette.getColor(i), mapToGlobal(rects[i].topLeft()));
			colorEditor->show();
		}

		return;
	}

	QWidget::mousePressEvent(event);
}

void PaletteWidget2::mouseMoveEvent(QMouseEvent *event)
{
	int i = getRectIndex(event->pos());
	if (i != hoveredItem)
	{
		hoveredItem = i;
		update();

		event->accept();

		return;
	}

	QWidget::mouseMoveEvent(event);
}

void PaletteWidget2::leaveEvent(QEvent *event)
{
	if (hoveredItem > -1)
	{
		hoveredItem = -1;
		update();
	}

	QWidget::leaveEvent(event);
}

int PaletteWidget2::getRectIndex(QPoint p)
{
	for (int i = 0; i < PALETTE_COLOR_COUNT; i++)
	{
		if (rects[i].contains(p))
			return i;
	}

	return -1;
}

void PaletteWidget2::onColorSelected(const Color &color)
{
	if (color == palette.getColor(selectedItem) && getRequireNewColor())
		return;
	
	palette.setColor(selectedItem, color);
	update();

	emit colorSelected(selectedItem);
	emit colorChanged(selectedItem, color);
}

}