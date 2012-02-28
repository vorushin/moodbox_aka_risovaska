#ifndef POPUPCOMBOBOX_H
#define POPUPCOMBOBOX_H

#include <QComboBox>

namespace MoodBox
{

// Sets popup to default index
class PopUpComboBox : public QComboBox
{
	Q_OBJECT

public:
	PopUpComboBox(QWidget *parent);
	
	void setDefaultItemIndex(int Index) { defaultItemIndex = Index; };

	virtual void showPopup();

private:
	int defaultItemIndex;
};

}

#endif // POPUPCOMBOBOX_H
