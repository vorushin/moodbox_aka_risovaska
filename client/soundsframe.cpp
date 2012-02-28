#include "soundsframe.h"

#include "testtools.h"

namespace MoodBox
{

SoundsFrame::SoundsFrame(QWidget *parent)
	: QFrame(parent)
{
	TimeMeasure t("SoundsFrame");

	setupUi(this);
}

SoundsFrame::~SoundsFrame()
{

}

}