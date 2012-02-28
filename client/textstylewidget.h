#ifndef TEXTSTYLEWIDGET_H
#define TEXTSTYLEWIDGET_H

#include <QWidget>

#include "ui_textstylewidget.h"

namespace MoodBox
{

using namespace Ui;

// Text style selection
class TextStyleWidget : public QWidget, public TextStyleWidgetClass
{
	Q_OBJECT

public:
	TextStyleWidget(QWidget *parent = 0);

	inline QString getCurrentFontName() const { return currentFontName; };
	
	void reset();

signals:
	void fontNameSelected(const QString &fontName);
	void closed();

protected:
	virtual void closeEvent(QCloseEvent *event);

private:
	QString currentFontName, defaultFontName;

	void setupButtons();
	void addAction(QToolButton *styleButton);

private slots:
	void onStyleActionTriggered(QAction *action);
};

}

#endif // TEXTSTYLEWIDGET_H
