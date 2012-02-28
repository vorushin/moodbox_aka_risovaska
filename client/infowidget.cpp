#include "infowidget.h"

#include "uitools.h"
#include "testtools.h"
#include "international.h"

namespace MoodBox
{

InfoWidget::InfoWidget(QWidget *parent)
	: QWidget(parent)
{
	TimeMeasure t("InfoWidget");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	connect(finishButton, SIGNAL(clicked()), this, SLOT(onFinishLinkAction()));
}

void InfoWidget::onFinishLinkAction()
{
	emit finished();
}

}