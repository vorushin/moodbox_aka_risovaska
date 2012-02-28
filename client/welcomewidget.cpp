#include "welcomewidget.h"

#include "uitools.h"
#include "testtools.h"

namespace MoodBox
{

WelcomeWidget::WelcomeWidget(QWidget *parent)
	: QWidget(parent)
{
	TimeMeasure t("WelcomeWidget");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	connect(profileButton, SIGNAL(clicked()), this, SLOT(onFillProfileAction()));
	connect(findFriendsButton, SIGNAL(clicked()), this, SLOT(onFindFriendsAction()));
	connect(finishButton, SIGNAL(clicked()), this, SLOT(onFinishLinkAction()));
}

void WelcomeWidget::onFillProfileAction()
{
	emit showProfileDialog();
}

void WelcomeWidget::onFindFriendsAction()
{
	emit showFindFriendsDialog();
}

void WelcomeWidget::onFinishLinkAction()
{
	emit finished();
}

}
