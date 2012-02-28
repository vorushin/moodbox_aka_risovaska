#include "palettelistwidgetitem.h"

#include <QHBoxLayout>
#include <QToolButton>
#include <QMouseEvent>

PaletteListWidgetItem::PaletteListWidgetItem(QWidget *parent)
	: QFrame(parent), removable(true)
{
	paletteWidget = new PaletteWidget(this);
	paletteWidget->setDirection(PaletteWidget::Horizonal);
	paletteWidget->setReadOnly(true);
	paletteWidget->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

	QWidget *removeButtonHost = new QWidget(this);
	removeButtonHost->setFixedSize(PALETTE_REMOVE_BUTTON_SIZE);

	removeButton = new QToolButton(removeButtonHost);
	removeButton->setText("x");
	removeButton->hide();

	connect(removeButton, SIGNAL(pressed()), this, SLOT(onRemovePressed()));

	QHBoxLayout *layout = new QHBoxLayout(removeButtonHost);
	layout->addWidget(removeButton);
	layout->setMargin(0);
	layout->setSpacing(0);

	removeButtonHost->setLayout(layout);

	layout = new QHBoxLayout(); 
	layout->setMargin(PALETTE_INTERNAL_SPACING);
	layout->setSpacing(PALETTE_INTERNAL_SPACING);

	layout->addWidget(paletteWidget);
	layout->addWidget(removeButtonHost);

	setLayout(layout);
}

PaletteListWidgetItem::~PaletteListWidgetItem()
{
}

void PaletteListWidgetItem::setPalette(const Palette &palette)
{
	paletteWidget->setPalette(palette);
}

Palette PaletteListWidgetItem::getPalette() const
{
	return paletteWidget->getPalette();
}

void PaletteListWidgetItem::setSelected(bool selected)
{
	if (getSelected() == selected)
		return;

	this->selected = selected;

	setFrameStyle((selected) ? QFrame::Box : QFrame::NoFrame);
}

bool PaletteListWidgetItem::getSelected() const
{
	return selected;
}

void PaletteListWidgetItem::setRemovable(bool removable)
{
	if (getRemovable() == removable)
		return;
	
	this->removable = removable;

	if (!removable && removeButton->isVisible())
		removeButton->hide();

	if (removable && underMouse())
		removeButton->show();
}

bool PaletteListWidgetItem::getRemovable() const
{
	return removable;
}

void PaletteListWidgetItem::mousePressEvent(QMouseEvent *event)
{
	if (event->button() == Qt::LeftButton)
		emit selectRequest(this);

	QFrame::mousePressEvent(event);
}

void PaletteListWidgetItem::enterEvent(QEvent *event)
{
	if (getRemovable())
		removeButton->show();

	QFrame::enterEvent(event);
}

void PaletteListWidgetItem::leaveEvent(QEvent *event)
{
	if (getRemovable())
		removeButton->hide();

	QFrame::leaveEvent(event);
}

void PaletteListWidgetItem::onRemovePressed()
{
	emit removeRequest(this);
}
