#ifndef PALETTELISTWIDGET_H
#define PALETTELISTWIDGET_H

#include <QScrollArea>
#include <QList>

#include "palettelist.h"

class QSpacerItem;

namespace MoodBox
{

class PaletteListWidgetItem;

#define PALETTE_LIST_INTERNAL_SPACING	0

// List of palette widget items, able to load and save list of palettes to xml
class PaletteListWidget : public QScrollArea
{
	Q_OBJECT

public:
	PaletteListWidget(QWidget *parent = 0);

	void setFileName(const QString &fileName);
	QString getFileName() const;

	void setPalettesRemovable(bool removable);
	inline bool getPalettesRemovable() const { return removable; };

	void addPalette(const Palette &palette);

	qint32 getPaletteCount() const;
	Palette getPalette(qint32 index) const;

signals:
	void paletteSelected(const Palette &palette);

private:
	QList <PaletteListWidgetItem *> items;

	bool removable;

	PaletteList paletteList;

	QWidget *listHost;
	QSpacerItem *spacer;

	void createItems();
	void removeItems();
	void reloadItems();

private slots:
	void onPaletteClick(PaletteListWidgetItem *item);
	void onPaletteAction(PaletteListWidgetItem *item);
};

}

#endif // PALETTELISTWIDGET_H
