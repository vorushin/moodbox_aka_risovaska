#ifndef PALETTELISTWIDGETITEM_H
#define PALETTELISTWIDGETITEM_H

#include <QFrame>

#include "palettewidget.h"

class QToolButton;

#define PALETTE_REMOVE_BUTTON_SIZE	QSize(16, 16)
#define PALETTE_INTERNAL_SPACING	2

class PaletteListWidgetItem : public QFrame
{
	Q_OBJECT

public:
	PaletteListWidgetItem(QWidget *parent = NULL);
	~PaletteListWidgetItem();

	void setPalette(const Palette &palette);
	Palette getPalette() const;

	void setSelected(bool selected);
	bool getSelected() const;

	void setRemovable(bool removable);
	bool getRemovable() const;

signals:
	void selectRequest(PaletteListWidgetItem *item);
	void removeRequest(PaletteListWidgetItem *item);

protected:
	virtual void mousePressEvent(QMouseEvent *event);

	virtual void enterEvent(QEvent *event);
	virtual void leaveEvent(QEvent *event);

private slots:
	void onRemovePressed();

private:
	PaletteWidget *paletteWidget;

	bool selected;
	bool removable;

	QToolButton *removeButton;
};

#endif // PALETTELISTWIDGETITEM_H
