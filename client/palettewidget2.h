#ifndef PALETTEWIDGET2_H
#define PALETTEWIDGET2_H

#include <QPointer>
#include <QWidget>

#include "coloreditor.h"
#include "palette.h"

namespace MoodBox 
{

// vertical palette widget for selecting/editing colors
class PaletteWidget2 : public QWidget
{
	Q_OBJECT

public:
	PaletteWidget2(QWidget *parent = 0);

	Palette getPalette() const { return palette; };

	void reset();

	void setSelectedColorIndex(const int i);
	const int getSelectedColorIndex() const { return selectedItem; }

	const Color getColorByIndex(qint32 index) const { return palette.getColor(index); };
	const Color getSelectedColor() const { return (selectedItem > -1 ? palette.getColor(selectedItem) : Color()); }

	void setRequireNewColor(bool requireNew);
	inline bool getRequireNewColor() const { return requireNewColor; };

public slots:
	void setPalette(const Palette &palette);

	void updateSelectedColor(const QColor &newColor);

signals:
	void colorSelected(qint32 index);
	void colorChanged(qint32 index, const QColor &newColor);

protected:
	void paintEvent(QPaintEvent *event);
	
	void mousePressEvent(QMouseEvent *event);
	void mouseMoveEvent(QMouseEvent *event);
	
	void leaveEvent(QEvent *event);
	
private:
	Palette palette;
	int selectedItem;
	int hoveredItem;
	bool requireNewColor;

	QRect rects[PALETTE_COLOR_COUNT];

	QPointer<ColorEditor> colorEditor;

	int getRectIndex(QPoint p);

private slots:
	void onColorSelected(const Color &color);
	
};

}

#endif // PALETTEWIDGET2_H