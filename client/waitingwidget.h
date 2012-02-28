#ifndef WAITINGWIDGET_H
#define WAITINGWIDGET_H

#include "ui_waitingwidget.h"
#include "verifiers.h"
#include "fault.h"
#include "accountresultcode.h"
#include "servercontrol.h"
#include "serverrequest.h"

namespace MoodBox 
{

using namespace Ui;

// Forgot password widget
class WaitingWidget : public ServerWidget, public WaitingWidgetClass
{
	Q_OBJECT

public:
	WaitingWidget(QWidget *parent = NULL);

	void startWaitingTimer();
	void stopWaitingTimer();

signals:
	void cancel();

protected:
	virtual void timerEvent(QTimerEvent *event);

private:
	int waitingTimer;
	static int currentWaitingPixmap;
	static void resetWaitingPixmapIndex();
	static QPixmap& getNextWaitingPixmap();

private slots:
	void onCancelLinkAction();
};

}

#endif // WAITINGWIDGET_H
