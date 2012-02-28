#include "palettelistwidgetitem.h"

#include <QMouseEvent>

#include "testtools.h"

namespace MoodBox
{

PaletteListWidgetItem::PaletteListWidgetItem(QWidget *parent)
	: QFrame(parent), hasAction(true), actionType(None)
{
	TimeMeasure t("PaletteListWidgetItem");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	setFrameStyle(QFrame::Raised | QFrame::StyledPanel);

	updateButtons();

	connect(addPaletteButton, SIGNAL(pressed()), this, SLOT(onActionActivated()));
	connect(deletePaletteButton, SIGNAL(pressed()), this, SLOT(onActionActivated()));
}

void PaletteListWidgetItem::setPalette(const Palette &palette)
{
	paletteWidget->setPalette(palette);
	nameLabel->setText(palette.getName());
}

Palette PaletteListWidgetItem::getPalette() const
{
	return paletteWidget->getPalette();
}

void PaletteListWidgetItem::setHasAction(bool hasAction)
{
	if (getHasAction() == hasAction)
		return;
	
	this->hasAction = hasAction;

	updateButtons();
}

void PaletteListWidgetItem::setActionType(ActionType type)
{
	if (getActionType() == type)
		return;

	this->actionType = type;

	updateButtons();
}

void PaletteListWidgetItem::mouseReleaseEvent(QMouseEvent *event)
{
	if (event->button() == Qt::LeftButton)
	{
		if (paletteWidget->rect().contains(paletteWidget->mapFromParent(event->pos())))
		{
			event->setAccepted(true);

			emit clicked(this);
			return;
		}
	}

	QFrame::mouseReleaseEvent(event);
}

void PaletteListWidgetItem::enterEvent(QEvent *event)
{
	setFrameStyle(QFrame::Sunken | QFrame::StyledPanel);

	QFrame::enterEvent(event);
}

void PaletteListWidgetItem::leaveEvent(QEvent *event)
{
	setFrameStyle(QFrame::Raised | QFrame::StyledPanel);

	QFrame::leaveEvent(event);
}

void PaletteListWidgetItem::updateButtons()
{
	if (!hasAction)
	{
		addPaletteButton->hide();
		deletePaletteButton->hide();
		nameLabel->hide();

		return;
	}

	switch (this->actionType)
	{
		case None: 
			addPaletteButton->hide();
			deletePaletteButton->hide();
			nameLabel->show();
			break;
		
		case Remove:
			addPaletteButton->hide();
			deletePaletteButton->show();
			nameLabel->hide();
			break;
		
		case Add: 
			addPaletteButton->show();
			deletePaletteButton->hide();;
			nameLabel->hide();

			Palette palette = Palette::transparentPalette();
			palette.setName(tr(ADD_NEW_PALETTE_TEXT));
			setPalette(palette);			
			break;
	}
}

void PaletteListWidgetItem::onActionActivated()
{
	emit actionActivated(this);
}

}