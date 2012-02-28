#ifndef PALETTEWIDGET_H
#define PALETTEWIDGET_H

#include <QWidget>

#include "palette.h"
#include "colorwidget.h"

class QGridLayout;

class PaletteWidget : public QWidget
{
	Q_OBJECT

public:
	enum Direction {Horizonal, Vertical};

	PaletteWidget(QWidget *parent = 0);

	Palette getPalette() const;

	void setDirection(Direction direction);
	Direction getDirection() const;

	void setColumnCount(int columnCount);
	int getColumnCount() const;

	void setReadOnly(bool readOnly);
	bool getReadOnly() const;

	void setSpacing(int spacing);
	int getSpacing() const;

public slots:
	void setPalette(const Palette &palette);

signals:
	void colorSelected(const QColor &color);

private slots:
	void setActiveColor(ColorWidget *newColor);
	void editColor(ColorWidget *editingColor);

private:
	void setupLayout();

	Palette palette;

	Direction direction;

	int columnCount;

	ColorWidget *activeColor;

	bool readOnly;

	ColorWidget *colors[PALETTE_COLOR_COUNT];

	QGridLayout *layout;

	int spacing;
};

#endif // PALETTEWIDGET_H
