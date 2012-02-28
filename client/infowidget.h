#ifndef INFOWIDGET_H
#define INFOWIDGET_H

#include <QWidget>

#include "ui_infowidget.h"

namespace MoodBox
{

using namespace Ui;

class InfoWidget : public QWidget, public InfoWidgetClass
{
	Q_OBJECT

public:
	InfoWidget(QWidget *parent = NULL);

	void setTitleLabel(const QString &title) { titleLabel->setText(title); } ;
	void setDescriptionLabel(const QString &descr) { desriptionLabel->setText(descr); } ;

signals:
	void finished();

private slots:
	void onFinishLinkAction();
};

}

#endif // INFOWIDGET_H
