#include "erasersettingsbar.h"

#include "testtools.h"

namespace MoodBox
{

EraserSettingsBar::EraserSettingsBar(QWidget *parent)
	: BrushSettingsBar(parent)
{
	TimeMeasure t("EraserSettingsBar");

	getPaletteWidget()->setEnabled(false);
	getPaletteWidget()->setVisible(false);

	getSizeButton()->setColor(Qt::gray);
	getPaletteManagerButton()->hide();
	getAlphaButton()->hide();
}

}
