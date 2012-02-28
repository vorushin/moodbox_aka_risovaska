#ifndef SETUPDIALOGFRAME_H
#define SETUPDIALOGFRAME_H

#include "servercontrol.h"

namespace MoodBox
{

// Base class for setup dialog tabs
class SetupDialogFrame : public ServerFrame
{
	Q_OBJECT

public:
	SetupDialogFrame(QWidget *parent);
	
	virtual bool isValid() = 0;
	virtual void startUpdate() = 0;

signals:
	void updateFinished();
	void updateError();
};

}

#endif // SETUPDIALOGFRAME_H
