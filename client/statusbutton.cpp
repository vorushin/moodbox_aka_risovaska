#include "statusbutton.h"

#include "testtools.h"

namespace MoodBox
{

StatusButton::StatusButton(QWidget *parent)
	: QToolButton(parent)
{
	TimeMeasure t("StatusButton");

	setupUi(this);
}

void StatusButton::setUserMenu(QMenu *menu)
{
	setMenu(menu);

	connect(menu, SIGNAL(aboutToShow()), this, SLOT(onMenuAboutToShow()));
	connect(menu, SIGNAL(aboutToHide()), this, SLOT(onMenuAboutToHide()));
}

void StatusButton::setIconLabel(QPixmap icon)
{
	iconLabel->setPixmap(icon);
}

void StatusButton::setTextLabel(QPixmap icon)
{
	textLabel->setPixmap(icon);
}

void StatusButton::setArrowLabel(QPixmap arrow, QPixmap downArrow)
{
	this->arrow = arrow;
	this->downArrow = downArrow;

	arrowLabel->setPixmap(arrow);
}

void StatusButton::onMenuAboutToShow()
{
	arrowLabel->setPixmap(downArrow);
}

void StatusButton::onMenuAboutToHide()
{
	arrowLabel->setPixmap(arrow);
}

}