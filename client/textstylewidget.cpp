#include "textstylewidget.h"

#include "testtools.h"

namespace MoodBox
{

TextStyleWidget::TextStyleWidget(QWidget *parent)
	: QWidget(parent, Qt::Popup)
{
	TimeMeasure t("TextStyleWidget");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	setupButtons();
}

void TextStyleWidget::reset()
{
	currentFontName = defaultFontName;
	
	emit fontNameSelected(defaultFontName);
}

void TextStyleWidget::closeEvent(QCloseEvent *event)
{
	QWidget::closeEvent(event);

	emit closed();
}

void TextStyleWidget::setupButtons()
{
	int itemsCount = layout()->count();

	for (int i = 0; i < itemsCount; i++)
	{
		QLayoutItem *layoutItem = layout()->itemAt(i);

		QWidget *widget = layoutItem->widget();

		if (widget != NULL)
		{
			QToolButton *toolButton = qobject_cast<QToolButton*>(widget);

			if (toolButton != NULL)
				addAction(toolButton);

			if (i == 0)
			{
				currentFontName = toolButton->font().family();
				defaultFontName = currentFontName;
			}
		}
	}
}

void TextStyleWidget::addAction(QToolButton *styleButton)
{
	QString fontName = styleButton->font().family();
	QAction *styleAction = new QAction(fontName, this);
	styleButton->setDefaultAction(styleAction);
	
	connect(styleButton, SIGNAL(triggered(QAction *)), this, SLOT(onStyleActionTriggered(QAction *)));
}

void TextStyleWidget::onStyleActionTriggered(QAction *action)
{
	QString name = action->text();
	currentFontName = name;
	
	emit fontNameSelected(name);
	close();
}

}