#include "palettelistwidget.h"

#include <QVBoxLayout>
#include <QSpacerItem>

#include "palettelistwidgetitem.h"
#include "vcommon.h"
#include "testtools.h"

namespace MoodBox
{

using namespace Velasquez;

PaletteListWidget::PaletteListWidget(QWidget *parent)
	: QScrollArea(parent), removable(true), spacer(NULL)
{
	TimeMeasure t("PaletteListWidget");

	listHost = new QWidget(this);
	listHost->setLayout(new QVBoxLayout(listHost));
	listHost->layout()->setMargin(PALETTE_LIST_INTERNAL_SPACING);
	listHost->layout()->setSpacing(PALETTE_LIST_INTERNAL_SPACING);
}

void PaletteListWidget::setFileName(const QString &fileName)
{
	if (paletteList.getFileName() == fileName)
		return;

	paletteList.setFileName(fileName);
	paletteList.clear();

	paletteList.load();
	reloadItems();
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
		item->setActionType( (removable) ? PaletteListWidgetItem::Remove : PaletteListWidgetItem::None);
}

void PaletteListWidget::addPalette(const Palette &palette)
{	
	paletteList.push_front(palette);
	paletteList.save();

	reloadItems();
}

qint32 PaletteListWidget::getPaletteCount() const
{
	return paletteList.count();
}

Palette PaletteListWidget::getPalette(qint32 index) const
{
	if (index >= paletteList.count())
		return Palette();
	else
		return paletteList.at(index);
}

void PaletteListWidget::createItems()
{
	foreach(Palette palette, paletteList)
	{
		PaletteListWidgetItem *item = new PaletteListWidgetItem(listHost);

		listHost->layout()->addWidget(item);

		item->setPalette(palette);
		item->setActionType((removable) ? PaletteListWidgetItem::Remove : PaletteListWidgetItem::None);

		item->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);

		connect(item, SIGNAL(clicked(PaletteListWidgetItem*)), this, SLOT(onPaletteClick(PaletteListWidgetItem*)));
		connect(item, SIGNAL(actionActivated(PaletteListWidgetItem*)), this, SLOT(onPaletteAction(PaletteListWidgetItem*)));

		items.append(item);
	}

	spacer = new QSpacerItem(1, 1, QSizePolicy::Minimum, QSizePolicy::Expanding);
	listHost->layout()->addItem(spacer);

	setWidget(listHost);
}

void PaletteListWidget::removeItems()
{
	foreach(PaletteListWidgetItem *item, items)
		item->deleteLater();

	items.clear();

	listHost->layout()->removeItem(spacer);
	DELETE_AND_NULL(spacer);

	setWidget(NULL);
}

void PaletteListWidget::reloadItems()
{
	removeItems();
	createItems();
}

void PaletteListWidget::onPaletteClick(PaletteListWidgetItem *item)
{
	emit paletteSelected(item->getPalette());
}

void PaletteListWidget::onPaletteAction(PaletteListWidgetItem *item)
{
	int i = items.indexOf(item);

	if (i < 0)
		return;

	QString paletteName = paletteList.at(i).getName();

	items.removeAt(i);
	paletteList.removeAt(i);
	item->deleteLater();

	paletteList.save();

	reloadItems();
}

}