#ifndef PALETTEVIEWWIDGET_H
#define PALETTEVIEWWIDGET_H

#include <QWidget>

#include "palette.h"

namespace MoodBox
{

// Palette viewing (and only viewing, no editing!) widget
class PaletteViewWidget : public QWidget
{
	Q_OBJECT

public:
	PaletteViewWidget(QWidget *parent = 0);

	inline Palette getPalette() const { return palette; }

public slots:
	void setPalette(const Palette &palette);

protected:
	virtual void paintEvent(QPaintEvent *);

private:
	Palette palette;
};

}

#endif // PALETTEVIEWWIDGET_H