#ifndef WELCOMEWIDGET_H
#define WELCOMEWIDGET_H

#include <QWidget>

#include "ui_welcomewidget.h"

namespace MoodBox
{

using namespace Ui;

class FindFriendsDialog;
class SetupDialog;

class WelcomeWidget : public QWidget, public WelcomeWidgetClass
{
	Q_OBJECT

public:
	WelcomeWidget(QWidget *parent = NULL);

signals:
	void showProfileDialog();
	void showFindFriendsDialog();
	void finished();

private slots:
	void onFillProfileAction();
	void onFindFriendsAction();
	void onFinishLinkAction();
};

}

#endif // WELCOMEWIDGET_H
