#include "palettelistwidget.h"

#include <QToolButton>
#include <QVBoxLayout>
#include <QSpacerItem>

#include "palettelistwidgetitem.h"

PaletteListWidget::PaletteListWidget(QWidget *parent)
	: QScrollArea(parent), selectedItem(NULL), removable(true), addItemWidget(NULL), spacer(NULL)
{
	listHost = new QWidget(this);
	listHost->setLayout(new QVBoxLayout(listHost));
	listHost->layout()->setMargin(PALETTE_LIST_INTERNAL_SPACING);
	listHost->layout()->setSpacing(PALETTE_LIST_INTERNAL_SPACING);
}

PaletteListWidget::~PaletteListWidget()
{
}

void PaletteListWidget::setFileName(const QString &fileName)
{
	if (paletteList.getFileName() == fileName)
		return;

	paletteList.setFileName(fileName);

	if (paletteList.load())
		paletteList.sort();

	removeItems();
	createItems();
}

QString PaletteListWidget::getFileName() const
{
	return paletteList.getFileName();
}

void PaletteListWidget::setPalettesRemovable(bool removable)
{
	if (this->removable == removable)
		return;

	this->removable = removable;

	foreach(PaletteListWidgetItem *item, items)
		item->setRemovable(removable);

	if (!removable)
		removeAddItem();
	else
		createAddItem();
}

bool PaletteListWidget::getPalettesRemovable() const
{
	return removable;
}

void PaletteListWidget::selectPalette(PaletteListWidgetItem *item)
{
	if (selectedItem != NULL)
		selectedItem->setSelected(false);

	item->setSelected(true);
	selectedItem = item;

	emit paletteSelected(item->getPalette());
}

void PaletteListWidget::addPalette(const Palette &palette)
{	
	paletteList.append(palette);
	paletteList.sort();
	paletteList.save();

	removeItems();
	createItems();
}

void PaletteListWidget::removePalette(PaletteListWidgetItem *item)
{
	int i = items.indexOf(item);

	if (i < 0)
		return;

	items.removeAt(i);
	paletteList.removeAt(i);
	item->deleteLater();

	if (selectedItem == item)
		selectedItem = NULL;

	paletteList.save();
}

void PaletteListWidget::createItems()
{
	foreach(Palette palette, paletteList)
	{
		PaletteListWidgetItem *item = new PaletteListWidgetItem(listHost);

		listHost->layout()->addWidget(item);

		item->setPalette(palette);
		item->setRemovable(removable);

		item->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);

		connect(item, SIGNAL(selectRequest(PaletteListWidgetItem*)), this, SLOT(selectPalette(PaletteListWidgetItem*)));
		connect(item, SIGNAL(removeRequest(PaletteListWidgetItem*)), this, SLOT(removePalette(PaletteListWidgetItem*)));

		items.append(item);
	}

	if (removable)
		createAddItem();

	spacer = new QSpacerItem(1, 1, QSizePolicy::Minimum, QSizePolicy::Expanding);
	listHost->layout()->addItem(spacer);

	setWidget(listHost);
}

void PaletteListWidget::removeItems()
{
	foreach(PaletteListWidgetItem *item, items)
		item->deleteLater();

	items.clear();
	removeAddItem();

	listHost->layout()->removeItem(spacer);
	delete spacer;
	spacer = NULL;

	selectedItem = NULL;	

	setWidget(NULL);
}

void PaletteListWidget::createAddItem()
{
	QToolButton *button = new QToolButton(listHost);
	button->setText("Add current");

	connect(button, SIGNAL(pressed()), this, SIGNAL(addNewPalette()));

	addItemWidget = button;
	
	listHost->layout()->addWidget(addItemWidget);
}

void PaletteListWidget::removeAddItem()
{
	if (addItemWidget == NULL)
		return;

	addItemWidget->hide();
	addItemWidget->deleteLater();
	addItemWidget = NULL;
}
