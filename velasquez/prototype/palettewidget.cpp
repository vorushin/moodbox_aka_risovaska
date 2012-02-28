#include "palettewidget.h"

#include <QGridLayout>
#include <math.h>

#include "coloreditdialog.h"

PaletteWidget::PaletteWidget(QWidget *parent)
	: QWidget(parent), direction(Vertical), columnCount(1), activeColor(NULL), readOnly(false), spacing(1)
{
	layout = new QGridLayout(this);

	layout->setHorizontalSpacing(spacing);
	layout->setVerticalSpacing(spacing);
	layout->setContentsMargins(0, 0, 0, 0);

	for (int i = 0; i < PALETTE_COLOR_COUNT; i++)
	{
		colors[i] = new ColorWidget(palette.getColor(i), i, this);
		colors[i]->setIndex(i);
		
		connect(colors[i], SIGNAL(activationRequest(ColorWidget *)), this, SLOT(setActiveColor(ColorWidget *)));
		connect(colors[i], SIGNAL(editRequest(ColorWidget *)), this, SLOT(editColor(ColorWidget *)));
	}

	setupLayout();
}

Palette PaletteWidget::getPalette() const
{
	return this->palette;
}

void PaletteWidget::setDirection(Direction direction)
{
	this->direction = direction;
	
	setupLayout();
	update();
}

PaletteWidget::Direction PaletteWidget::getDirection() const
{
	return this->direction;
}

void PaletteWidget::setColumnCount(int columnCount)
{
	if (columnCount < 0)
		return;

	this->columnCount = columnCount;
	
	setupLayout();
	update();
}
	
int PaletteWidget::getColumnCount() const
{
	return this->columnCount;
}

void PaletteWidget::setReadOnly(bool readOnly)
{
	this->readOnly = readOnly;
}

bool PaletteWidget::getReadOnly() const
{
	return this->readOnly;
}

void PaletteWidget::setSpacing(int spacing)
{
	if (spacing < 0)
		return;

	this->spacing = spacing;

	if (this->direction == Vertical)
		layout->setVerticalSpacing(spacing);
	else
		layout->setHorizontalSpacing(spacing);
}

int PaletteWidget::getSpacing() const
{
	return this->spacing;
}

void PaletteWidget::setPalette(const Palette &palette)
{
	this->palette = palette;

	for (int i = 0; i < PALETTE_COLOR_COUNT; i++)
		colors[i]->setColor(palette.getColor(i));

	update();
}

void PaletteWidget::setActiveColor(ColorWidget *newColor)
{
	if (readOnly)
		return;

	if (activeColor != NULL)
		activeColor->setActive(false);

	newColor->setActive(true);
	activeColor = newColor;

	emit colorSelected(newColor->getColor());
}

void PaletteWidget::editColor(ColorWidget *editingColor)
{
	if (readOnly)
		return;

	ColorEditDialog d(this);
	d.attachToColorWidget(editingColor);

	if (d.exec() == QDialog::Accepted)
	{
		editingColor->setColor(d.getColor());

		palette.setColor(editingColor->getIndex(), editingColor->getColor());
	}
}

void PaletteWidget::setupLayout()
{
	// remove old ones
	for (int i = 0; i < PALETTE_COLOR_COUNT; i++)
		if (layout->indexOf(colors[i]) > 0)
			layout->removeWidget(colors[i]);

	int colorsInColumn = (int) ceil((float)PALETTE_COLOR_COUNT / columnCount);

	int rowIndex = 0;
	int columnIndex = 0;

	for (int i = 0; i < PALETTE_COLOR_COUNT; i++)
	{
		layout->addWidget(colors[i], rowIndex, columnIndex);

		if (this->direction == Vertical)
		{
			rowIndex++;

			if (rowIndex == colorsInColumn)
			{
				rowIndex = 0;
				columnIndex ++;
			}
		}
		else
		{
			columnIndex++;

			if (columnIndex == colorsInColumn)
			{
				rowIndex ++;
				columnIndex = 0;
			}
		}

		colors[i]->show();
	}
}
