#ifndef CONTACTLISTBUTTON_H
#define CONTACTLISTBUTTON_H

#include <QToolButton>

namespace MoodBox
{

class ContactListButton : public QToolButton
{
	Q_OBJECT

public:
	ContactListButton(QWidget *parent);

public slots:
	void update(int contactsCountWithNewMessages);

protected:
	virtual void dragEnterEvent(QDragEnterEvent *event);	

	virtual void enterEvent(QEvent *event);
	virtual void leaveEvent(QEvent *event);

private:
	bool mouseIsHovered;
	bool isPressed;
	int contactsWithNewMessages;

private slots:
	void updateImage();

	void onPressed();
	void onReleased();
};

}

#endif // CONTACTLISTBUTTON_H
