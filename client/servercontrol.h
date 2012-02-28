#ifndef SERVERCONTROL_H
#define SERVERCONTROL_H

#include "uitools.h"

#include <QFrame>
#include <QPoint>

#include "verifiers.h"
#include "formblocker.h"
#include "serverproxysingleton.h"

class QMouseEvent;

namespace MoodBox
{

// Mix class for widgets or windows that work with server (e.g. logon widget)
class ServerControl
{
public:
	ServerControl();

	inline QList <QWidget *> getBlockingWidgets() const { return formBlocker->getBlockingWidgets(); };

protected:
	FormVerifier *formVerifier;
	FormBlocker *formBlocker;
};


// Base class for dialogs used to communicate with server (e.g. registration)
class ServerDialog: public MoodBoxDialog, public ServerControl
{
	Q_OBJECT

public:
	ServerDialog(QWidget *parent, Qt::WindowFlags flags = Qt::FramelessWindowHint | Qt::Dialog);

public slots:
	virtual void onRequestCancelled() {};
};

// Base class for widgets used to communicate with server
class ServerWidget: public QWidget, public ServerControl
{
	Q_OBJECT

public:
	ServerWidget(QWidget *parent);

public slots:
	virtual void onRequestCancelled() {};
};

// Base class for frames used to communicate with server
class ServerFrame: public QFrame, public ServerControl
{
	Q_OBJECT

public:
	ServerFrame(QWidget *parent);

public slots:
	virtual void onRequestCancelled() {};
};

}

#endif // SERVERCONTROL_H
