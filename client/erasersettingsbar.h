#ifndef ERASERSETTINGSBAR_H
#define ERASERSETTINGSBAR_H

#include "brushsettingsbar.h"

namespace MoodBox
{

// Eraser has limited set of controls enabled
class EraserSettingsBar : public BrushSettingsBar
{
	Q_OBJECT

public:
	EraserSettingsBar(QWidget *parent);
};

}

#endif // ERASERSETTINGSBAR_H
