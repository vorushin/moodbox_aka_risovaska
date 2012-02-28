#ifndef TEXTSTYLEBUTTON_H
#define TEXTSTYLEBUTTON_H

#include <QToolButton>

namespace MoodBox
{

class TextStyleWidget;

// Text style selection
class TextStyleButton : public QToolButton
{
	Q_OBJECT

public:
	TextStyleButton(QWidget *parent);

	QString getCurrentFontName() const;

	void reset();

signals:
	void fontNameSelected(const QString &fontName);

private:
	TextStyleWidget *styleWidget;

	void updateText();

private slots:
	virtual void onToggled(bool toggled);
	void onStyleWidgetClosed();
};

}

#endif // TEXTSTYLEBUTTON_H
