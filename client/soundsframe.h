#ifndef SOUNDSFRAME_H
#define SOUNDSFRAME_H

#include <QFrame>
#include "ui_soundsframe.h"

namespace MoodBox
{

#define SOUND_DIR						"Sound"

#define FRIEND_MESSAGE_RECEIVED_SOUND	"message_rcv_2.wav"
#define PRIVATE_MESSAGE_RECEIVED_SOUND	"message_rcv_3.wav"
#define MESSAGE_SENT_SOUND				"message_send_1.wav"

using namespace Ui;

class SoundsFrame : public QFrame, public SoundsFrameClass
{
	Q_OBJECT

public:
	SoundsFrame(QWidget *parent = 0);
	~SoundsFrame();
};

}

#endif // SOUNDSFRAME_H
