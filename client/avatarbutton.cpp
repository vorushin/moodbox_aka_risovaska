#include "avatarbutton.h"

#include <QEvent>
#include <QPainter>
#include <QMimeData>
#include <QDrag>
#include <QMouseEvent>
#include <QApplication>

#include "apptools.h"

#include "peopleinfomanager.h"
#include "testtools.h"

namespace MoodBox
{

AvatarButton::AvatarButton(QWidget *parent)
	: QToolButton(parent), thumbnailMode(false)
{
	TimeMeasure t("AvatarButton");

	mouseIsHovered = false;

	setIconSize(getIconSizeHint());
}

void AvatarButton::setThumbnailMode(bool thumbnailMode)
{
	if (this->thumbnailMode == thumbnailMode)
		return;

	this->thumbnailMode = thumbnailMode;

	setIconSize(getIconSizeHint());
	
	updateAvatar();
}

QSize AvatarButton::sizeHint() const
{
	return iconSize() + QSize(USERPIC_BUTTON_MARGIN, USERPIC_BUTTON_MARGIN);
}

QPixmap AvatarButton::paintRoundBorder(QPixmap &userPicture, bool thumbnailMode, bool mouseIsHovered)
{
	// Paint round border for userpic
	QPixmap avatarMask;
	QIcon avatarBorder;
	QIcon userpic;

	if (thumbnailMode)
	{
		avatarMask.load(AVATAR_TRANSPARENCY_MASK_SMALL);
		if (mouseIsHovered)
			avatarBorder.addFile(AVATAR_HOVER_BORDER_SMALL);
		else
			avatarBorder.addFile(AVATAR_BORDER_SMALL);

		userpic.addPixmap(userPicture.scaled(USERPIC_SMALL_WIDTH - 4, USERPIC_SMALL_HEIGHT - 4, Qt::KeepAspectRatioByExpanding));
	}
	else
	{
		avatarMask.load(AVATAR_TRANSPARENCY_MASK_MAX);
		if (mouseIsHovered)
			avatarBorder.addFile(AVATAR_HOVER_BORDER_MAX);
		else
			avatarBorder.addFile(AVATAR_BORDER_MAX);

		userpic.addPixmap(userPicture.scaled(USERPIC_MAX_WIDTH - 2, USERPIC_MAX_HEIGHT - 2, Qt::KeepAspectRatioByExpanding));
	}
	
	QPainter imgPainter(&avatarMask);
	imgPainter.setCompositionMode(QPainter::CompositionMode_SourceAtop);
	imgPainter.fillRect(avatarMask.rect(), QBrush(Qt::white));

	userpic.paint(&imgPainter, avatarMask.rect(), Qt::AlignCenter, QIcon::Normal);
	
	imgPainter.setCompositionMode(QPainter::CompositionMode_SourceOver);
	avatarBorder.paint(&imgPainter, avatarMask.rect(), Qt::AlignCenter, QIcon::Normal);

	return avatarMask;
}

void AvatarButton::mousePressEvent(QMouseEvent *event)
{
	if ( (event->button() == Qt::LeftButton) && !icon().isNull() )
		dragStartPosition = event->pos();

	QToolButton::mousePressEvent(event);
}

void AvatarButton::mouseMoveEvent(QMouseEvent *event)
{
	if (!dragStartPosition.isNull())
	{
		if (!(event->buttons() & Qt::LeftButton))
			return;

		if ((event->pos() - dragStartPosition).manhattanLength() < QApplication::startDragDistance())
			return;
		
		dragAvatar();

		dragStartPosition = QPoint();

		event->accept();
		return;
	}

	QToolButton::mouseMoveEvent(event);
}

void AvatarButton::enterEvent(QEvent *event)
{
	QToolButton::enterEvent(event);
	mouseIsHovered = true;

	setIconSize(getIconSizeHint());
	
	updateCache();
}

void AvatarButton::leaveEvent(QEvent *event)
{
	QToolButton::leaveEvent(event);
	mouseIsHovered = false;

	setIconSize(getIconSizeHint());
	
	updateCache();
}

void AvatarButton::updateCache()
{
	TimeMeasure t("AvatarButton::updateCache()");

	QPixmap picture = getSourcePicture(false);
	avatarCache = paintRoundBorder(picture, false, mouseIsHovered);
	
	QPixmap pictureThumbnail = getSourcePicture(true);
	avatarThumbnailCache = paintRoundBorder(pictureThumbnail, true, mouseIsHovered);

	updateAvatar();
}

void AvatarButton::onPictureChanged()
{
	updateCache();
}

void AvatarButton::dragAvatar()
{
	QPixmap currentPixmap = getSourcePicture(false);
		
	// Arrange data
	QMimeData *data = new QMimeData;
	data->setImageData(currentPixmap.toImage());

	QPixmap preview = getSourcePicture(true);

	// Go drag
	QDrag *drag = new QDrag(this);

	drag->setPixmap(preview);
    drag->setMimeData(data);
	drag->setHotSpot(QPoint(preview.width() / 2, preview.height() / 2));

    // Let's go!
	drag->exec(Qt::MoveAction);
}

void AvatarButton::updateAvatar()
{
	if (thumbnailMode)
		setIcon(avatarThumbnailCache);
	else
		setIcon(avatarCache);
}

QSize AvatarButton::getIconSizeHint() const
{
	QSize iconSize;

	if (thumbnailMode)
		iconSize = QSize(USERPIC_SMALL_WIDTH, USERPIC_SMALL_HEIGHT);
	else
		iconSize = QSize(USERPIC_MAX_WIDTH, USERPIC_MAX_HEIGHT);
	
	return iconSize;
}

}
