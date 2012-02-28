#include "textstylebutton.h"

#include <QDesktopWidget>

#include "textstylewidget.h"
#include "testtools.h"
#include "uitools.h"

namespace MoodBox
{

TextStyleButton::TextStyleButton(QWidget *parent)
	: QToolButton(parent)
{
	TimeMeasure t("TextStyleButton");

	setCheckable(true);
	setPopupMode(QToolButton::InstantPopup);
	
	styleWidget = new TextStyleWidget(this);
	updateText();
	
	connect(this, SIGNAL(toggled(bool)), this, SLOT(onToggled(bool)));
	connect(this->styleWidget, SIGNAL(closed()), this, SLOT(onStyleWidgetClosed()));
	connect(this->styleWidget, SIGNAL(fontNameSelected(const QString &)), this, SIGNAL(fontNameSelected(const QString &)));
}

QString TextStyleButton::getCurrentFontName() const
{
	return styleWidget->getCurrentFontName();
}

void TextStyleButton::reset()
{
	styleWidget->reset();
	updateText();
}

void TextStyleButton::updateText()
{
	QString fontName = getCurrentFontName();
	QFont Font(fontName, font().pointSize());
	QFontMetrics fontMetrics(Font);
	setText(fontMetrics.elidedText(fontName, Qt::ElideRight, width() - 14));

	setFont(Font);
}

void TextStyleButton::onToggled(bool toggled)
{
	if (toggled)
	{
		QRect availableGeometry = QApplication::desktop()->availableGeometry(this);
		QPoint p = mapToGlobal(QPoint(0, height() + styleWidget->height()));
		if (p.y() > availableGeometry.bottom())
			styleWidget->move(mapToGlobal(QPoint(0, -styleWidget->height())));
		else
			styleWidget->move(mapToGlobal(QPoint(0, height())));

	}

	styleWidget->setVisible(toggled);
}

void TextStyleButton::onStyleWidgetClosed()
{
	if (isChecked())
	{
		setChecked(false);
		updateText();
	}
}

}