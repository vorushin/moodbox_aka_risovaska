#include "servercontrol.h"

#include <QMouseEvent>

namespace MoodBox
{

// ServerControl class
ServerControl::ServerControl()
: formVerifier(NULL), formBlocker(NULL)
{
}

// ServerDialog class
ServerDialog::ServerDialog(QWidget *parent, Qt::WindowFlags flags)
	: MoodBoxDialog(parent, flags), ServerControl()
{
	//setAttribute(Qt::WA_DeleteOnClose);

	formVerifier = new FormVerifier(this);
	formBlocker = new FormBlocker(this);

	connect(formBlocker, SIGNAL(progressCancelled()), this, SLOT(onRequestCancelled()));
}

// ServerWidget class
ServerWidget::ServerWidget(QWidget *parent)
	: QWidget(parent), ServerControl()
{
	formVerifier = new FormVerifier(this);
	formBlocker = new FormBlocker(this);

	connect(formBlocker, SIGNAL(progressCancelled()), this, SLOT(onRequestCancelled()));
}

// ServerFrame class
ServerFrame::ServerFrame(QWidget *parent)
	: QFrame(parent), ServerControl()
{
	formVerifier = new FormVerifier(this);
	formBlocker = new FormBlocker(this);

	connect(formBlocker, SIGNAL(progressCancelled()), this, SLOT(onRequestCancelled()));
}

}
