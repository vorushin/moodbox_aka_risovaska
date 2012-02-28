#include "finddialog.h"

#include <QSizeGrip>

#include "uitools.h"
#include "testtools.h"

namespace MoodBox
{

FindDialog::FindDialog(QWidget *parent)
	: ServerDialog(parent)
{
	TimeMeasure t("FindDialog");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	UiTools::moveWindowToScreenCenter(this);

	connect(findPeopleFrame, SIGNAL(canAddFriend(bool)), addUserButton, SLOT(setEnabled(bool)));
	connect(closeToolButton, SIGNAL(clicked()), this, SLOT(onClose()));
	connect(closeButton, SIGNAL(clicked()), this, SLOT(onClose()));

	new QSizeGrip(widget);
}

void FindDialog::clearData()
{
	findPeopleFrame->clearData();
}

void FindDialog::resizeEvent(QResizeEvent *event)
{
	setMask(WidgetMaskCreator::createMask(WidgetMaskCreator::WidgetMaskSmall, width(), height()));

	QDialog::resizeEvent(event);
}

void FindDialog::showEvent(QShowEvent *event)
{
	findChannelFrame->queryChannels();

	QDialog::showEvent(event);
}

void FindDialog::on_addUserButton_clicked()
{
	findPeopleFrame->addAsFriend();
}

void FindDialog::on_findTabWidget_currentChanged(int tabIndex)
{
	addUserButton->setVisible(tabIndex == 0);
}

void FindDialog::onClose()
{
	findPeopleFrame->onRequestCancelled();

	reject();
}

}