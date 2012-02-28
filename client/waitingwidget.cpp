#include "waitingwidget.h"


#include <QMessageBox>

#include "serverproxysingleton.h"
#include "moodboxcustomserver.h"
#include "international.h"
#include "uitools.h"
#include "formblocker.h"
#include "testtools.h"

namespace MoodBox 
{
	int WaitingWidget::currentWaitingPixmap = 0;

WaitingWidget::WaitingWidget(QWidget *parent)
	: ServerWidget(parent), waitingTimer(-1)
{
	TimeMeasure t("WaitingWidget");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	connect(cancelButton, SIGNAL(clicked()), this, SLOT(onCancelLinkAction()));
}

void WaitingWidget::timerEvent(QTimerEvent *event)
{
	if (event->timerId() == waitingTimer)
	{
		waitingLabel->setPixmap(getNextWaitingPixmap());
	}

	QObject::timerEvent(event);
}

void WaitingWidget::resetWaitingPixmapIndex()
{
	currentWaitingPixmap = -1;
}

QPixmap& WaitingWidget::getNextWaitingPixmap()
{
		static QPixmap pixmaps[] = {QPixmap(":/MoodBox/Resources/waiting_circle_1.png"), 
									QPixmap(":/MoodBox/Resources/waiting_circle_2.png"),
									QPixmap(":/MoodBox/Resources/waiting_circle_3.png"),
									QPixmap(":/MoodBox/Resources/waiting_circle_4.png"),
									QPixmap(":/MoodBox/Resources/waiting_circle_5.png"),
									QPixmap(":/MoodBox/Resources/waiting_circle_6.png"),
									QPixmap(":/MoodBox/Resources/waiting_circle_7.png"),
									QPixmap(":/MoodBox/Resources/waiting_circle_8.png")};

	currentWaitingPixmap++;
	if (currentWaitingPixmap >= (int) (sizeof(pixmaps) / sizeof(pixmaps[0])))
			currentWaitingPixmap = 0;

	return pixmaps[currentWaitingPixmap];
}

void WaitingWidget::onCancelLinkAction()
{
	stopWaitingTimer();
	emit cancel();
}

void WaitingWidget::startWaitingTimer()
{
	resetWaitingPixmapIndex();
	waitingLabel->setPixmap(getNextWaitingPixmap());
	if (waitingTimer < 0)
	{
		waitingTimer = startTimer(200);
	}
}

void WaitingWidget::stopWaitingTimer()
{
	if (waitingTimer > 0)
	{
		killTimer(waitingTimer);
		waitingTimer = -1;
	}
}

}
