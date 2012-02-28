#include "newversionavailabledialog.h"

#include "apptools.h"

#include "testtools.h"

namespace MoodBox
{

NewVersionAvailableDialog::NewVersionAvailableDialog(QWidget *parent)
: MoodBoxDialog(parent)
{
	TimeMeasure t("NewVersionAvailableDialog");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	isForcedParentWindowShow = false;

	setWindowFlags(windowFlags() & ~Qt::WindowContextHelpButtonHint);

	connect(downloadButton, SIGNAL(pressed()), this, SLOT(accept()));
	connect(closeButton, SIGNAL(pressed()), this, SLOT(reject()));
	
#ifdef Q_WS_MAC
	downloadButton->hide();
#endif
#ifdef Q_WS_X11
	downloadButton->hide();
#endif

}

bool NewVersionAvailableDialog::getIsForcedParentWindowShow()
{
	return isForcedParentWindowShow;
}

void NewVersionAvailableDialog::showDialog(QString descriptionText, bool isInteractive, bool isOverMainWindow)
{
	isForcedParentWindowShow = false;

	descriptionBox->setHtml(descriptionText);

	if (!isInteractive && isOverMainWindow)
	{
		QWidget *widget = parentWidget();

		if (!widget->isVisible())
		{
			widget->showMinimized();
			isForcedParentWindowShow = true;

			UiTools::moveWindowToScreenCenter(this);
		}

		// We need to blink in two cases:  when window isn't visible or when it's inactive
		if (widget->isMinimized() || !widget->isActiveWindow())
			AppTools::alertWidget(widget);
	}

	show();
}

void NewVersionAvailableDialog::onSkip()
{
	done(-1);
}

}
