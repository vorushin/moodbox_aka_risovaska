#include "blockedlistframe.h"

#include <QComboBox>
#include <QMessageBox>

#include "common.h"
#include "testtools.h"

namespace MoodBox
{

BlockedListFrame::BlockedListFrame(QWidget *parent)
	: SetupDialogFrame(parent), blockedListWasChanged(false)
{
	TimeMeasure t("BlockedListFrame");

	setupUi(this);

	// TODO FUNC connect server response
	//connect(server, SIGNAL(updateAccountResponse(IMessage *, RegistrationResult)), this, SLOT(onBlockedListUpdateResponse(IMessage *, RegistrationResult)));
}

bool BlockedListFrame::isValid()
{
	return true;
}

void BlockedListFrame::startUpdate()
{
	if (!blockedListWasChanged)
	{
		emit updateFinished();
		return;
	}

	if (!isValid())
		return;
}

void BlockedListFrame::blockedListUpdateError(qint32 result, bool isUpdate)
{
	Q_UNUSED(result)
	Q_UNUSED(isUpdate)
}

void BlockedListFrame::on_unblockButton_clicked() 
{
	blockedListWasChanged = true;
}

void BlockedListFrame::on_blockButton_clicked() 
{
	QString personToBlock = personToBlockCombo->currentText();
	
	blockedListWasChanged = true;
}

// TODO FUNC process server response
/*void BlockedListFrame::onBlockedListUpdateResponse(IMessage *exception, RegistrationResult result)
{
	formBlocker->unblock();
	
	if (exception != NULL)
	{
		// TODO FUNC
		UiTools::handleError(this, exception, tr(UPDATE_ERROR_TITLE));
		return;
	}

	if (result == Registered)
		emit blockedListUpdated();
	else
		blockedListUpdateError(result, true);
}*/

}