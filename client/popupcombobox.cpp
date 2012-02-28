#include "popupcombobox.h"

namespace MoodBox
{

PopUpComboBox::PopUpComboBox(QWidget *parent)
	: QComboBox(parent), defaultItemIndex(-1)
{
}

void PopUpComboBox::showPopup()
{
	if (this->currentIndex() < 0 && defaultItemIndex >= 0)
	{
		this->setCurrentIndex(defaultItemIndex);
	}

	QComboBox::showPopup();	
}

}