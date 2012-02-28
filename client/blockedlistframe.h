#ifndef BLOCKEDLISTFRAME_H
#define BLOCKEDLISTFRAME_H

#include "setupdialogframe.h"

#include "ui_blockedlistframe.h"

namespace MoodBox
{

using namespace Ui;

// Frame for add/remove person from blocked list
class BlockedListFrame : public SetupDialogFrame, public BlockedListFrameClass
{
	Q_OBJECT

public:
	BlockedListFrame(QWidget *parent);

	virtual bool isValid();
	virtual void startUpdate();

public slots:
	// TODO FUNC
	//virtual void onRequestCancelled();

private:
	bool blockedListWasChanged;

	void blockedListUpdateError(qint32 result, bool isUpdate);

private slots:
	void on_unblockButton_clicked();
	void on_blockButton_clicked();

	// TODO FUNC process server response
	//void onBlockedListUpdateResponse(IMessage *exception, RegistrationResult result);
};

}

#endif // BLOCKEDLISTFRAME_H
