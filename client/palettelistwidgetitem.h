#ifndef PALETTELISTWIDGETITEM_H
#define PALETTELISTWIDGETITEM_H

#include <QFrame>

#include "ui_palettelistwidgetitem.h"

class QToolButton;
class QLabel;

namespace MoodBox
{

using namespace Ui;

#define ADD_NEW_PALETTE_TEXT	QT_TRANSLATE_NOOP("MoodBox::PaletteListWidgetItem", "AddNewPalette")

// Item of palette list widget, contains palette and has actions to delete or add new palette
class PaletteListWidgetItem : public QFrame, public PaletteListWidgetItemClass
{
	Q_OBJECT

public:
	enum ActionType { None, Remove, Add };

public:
	PaletteListWidgetItem(QWidget *parent = 0);

	void setPalette(const Palette &palette);
	Palette getPalette() const;

	void setHasAction(bool hasAction);
	inline bool getHasAction() const { return hasAction; };

	void setActionType(ActionType type);
	inline ActionType getActionType() const { return actionType; };

signals:
	void clicked(PaletteListWidgetItem *item);
	void actionActivated(PaletteListWidgetItem *item);

protected:
	virtual void mouseReleaseEvent(QMouseEvent *event);

	virtual void enterEvent(QEvent *event);
	virtual void leaveEvent(QEvent *event);

private:
	bool hasAction;
	ActionType actionType;

	void updateButtons();

private slots:
	void onActionActivated();

};

}

#endif // PALETTELISTWIDGETITEM_H
