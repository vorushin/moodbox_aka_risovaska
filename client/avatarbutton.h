#ifndef AVATARBUTTON_H
#define AVATARBUTTON_H

#include <QToolButton>

namespace MoodBox
{

#define USERPIC_BUTTON_MARGIN						0
#define USERPIC_BUTTON_TOOLTIP						QT_TRANSLATE_NOOP("MoodBox::AvatarButton", "AvatarButtonHint")

#define AVATAR_TRANSPARENCY_MASK_MAX				":/MoodBox/Resources/avatar_transparency_mask.png"
#define AVATAR_BORDER_MAX							":/MoodBox/Resources/avatar_border.png"
#define AVATAR_HOVER_BORDER_MAX							":/MoodBox/Resources/avatar_hover_border.png"

#define AVATAR_TRANSPARENCY_MASK_SMALL				":/MoodBox/Resources/avatar_transparency_mask_small.png"
#define AVATAR_BORDER_SMALL							":/MoodBox/Resources/avatar_border_small.png"
#define AVATAR_HOVER_BORDER_SMALL					":/MoodBox/Resources/avatar_hover_border_small.png"

// Class for manage user and contact avatars
class AvatarButton : public QToolButton
{
	Q_OBJECT

public:
	AvatarButton(QWidget *parent);

	inline bool getThumbnailMode() const { return thumbnailMode; };
	void setThumbnailMode(bool thumbnailMode);

	virtual QSize sizeHint() const;

	static QPixmap paintRoundBorder(QPixmap &userPicture, bool thumbnailMode, bool mouseIsHovered = false);

protected:
	QPixmap avatarCache;
	QPixmap avatarThumbnailCache;

	virtual void mousePressEvent(QMouseEvent *event);
	virtual void mouseMoveEvent(QMouseEvent *event);
	
	virtual void enterEvent(QEvent *event);
	virtual void leaveEvent(QEvent *event);

	void updateCache();

	virtual QPixmap getSourcePicture(bool thumbnail) = 0;

protected slots:
	void onPictureChanged();

private:
	// Display modes
	bool thumbnailMode;
	bool mouseIsHovered;
	QPoint dragStartPosition;

	void dragAvatar();

	void updateAvatar();

	// Optimal image size
	virtual QSize getIconSizeHint() const;
};

}

#endif // AVATARBUTTON_H
