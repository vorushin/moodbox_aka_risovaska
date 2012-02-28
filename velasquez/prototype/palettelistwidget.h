#ifndef PALETTELISTWIDGET_H
#define PALETTELISTWIDGET_H

#include <QScrollArea>
#include <QList>

#include "palettelist.h"

class QSpacerItem;
class PaletteListWidgetItem;

#define PALETTE_LIST_INTERNAL_SPACING	1

class PaletteListWidget : public QScrollArea
{
	Q_OBJECT

public:
	PaletteListWidget(QWidget *parent = NULL);
	~PaletteListWidget();

	void setFileName(const QString &fileName);
	QString getFileName() const;

	void setPalettesRemovable(bool removable);
	bool getPalettesRemovable() const;

	void addPalette(const Palette &palette);

signals:
	void paletteSelected(const Palette &palette);
	void addNewPalette();

private slots:
	void selectPalette(PaletteListWidgetItem *item);
	void removePalette(PaletteListWidgetItem *item);

private:
	void createItems();
	void removeItems();

	void createAddItem();
	void removeAddItem();

	QList <PaletteListWidgetItem *> items;
	PaletteListWidgetItem *selectedItem;

	bool removable;

	PaletteList paletteList;

	QWidget *listHost;
	QWidget *addItemWidget;
	QSpacerItem *spacer;
};

#endif // PALETTELISTWIDGET_H
