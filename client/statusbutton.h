#ifndef STATUSBUTTON_H
#define STATUSBUTTON_H

#include <QToolButton>
#include <QMenu>

#include "ui_statusbutton.h"

namespace MoodBox
{

using namespace Ui;

// Contact list window - status button with menu
class StatusButton : public QToolButton, public StatusButtonClass
{
	Q_OBJECT

public:
	StatusButton(QWidget *parent = NULL);
	
	void setUserMenu(QMenu *menu);
	void setIconLabel(QPixmap icon);
	void setTextLabel(QPixmap icon);
	void setArrowLabel(QPixmap arrow, QPixmap downArrow);

public slots:
	void onMenuAboutToShow();
	void onMenuAboutToHide();

private:
	QPixmap arrow;
	QPixmap downArrow;

};

}

#endif // STATUSBUTTON_H
