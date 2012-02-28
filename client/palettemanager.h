#ifndef PALETTEMANAGER_H
#define PALETTEMANAGER_H

#include <QFrame>

#include "ui_palettemanager.h"

namespace MoodBox
{

#define PALETTEMANAGER					PaletteManager::getCommonManager()

#define STANDARD_PALETTES_FILENAME		"catalogue.pal"
#define CUSTOM_PALETTES_FILENAME		"custom.pal"

using namespace Ui;

// Manager of custom and standard palettes, can add/load/save palettes
class PaletteManager : public QWidget, public PaletteManagerClass
{
	Q_OBJECT

public:
	PaletteManager(QWidget *parent = 0);

	void setPalette(const Palette &palette);
	Palette getPalette() const;

	Palette getDefaultPalette() const { return defaultPalette; };

	void managePalette(const Palette &palette, const QPoint &showLocation = QPoint());

	static PaletteManager *getCommonManager();

signals:
	void paletteSelected(const Palette &palette);

	void finished();

protected:
	virtual void showEvent(QShowEvent *event);
	virtual void resizeEvent(QResizeEvent *event);
	virtual void closeEvent(QCloseEvent *event);

private:
	Palette defaultPalette;
	
	static PaletteManager *commonPaletteManager;

private slots:
	void onPaletteSelected(const Palette &palette);
	void on_savePaletteButton_clicked();

	void showHelp();
};

}

#endif // PALETTEMANAGER_H
