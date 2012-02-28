#ifndef PALETTEMANAGER_H
#define PALETTEMANAGER_H

#include <QDialog>

#include "ui_palettemanager.h"

using namespace Ui;

class PaletteManager : public QDialog, public PaletteManagerClass
{
	Q_OBJECT

public:
	PaletteManager(QWidget *parent = 0);
	~PaletteManager();

	void setPalette(const Palette &palette);
	Palette getPalette() const;

private slots:
	void onAddPalette();

};

#endif // PALETTEMANAGER_H
