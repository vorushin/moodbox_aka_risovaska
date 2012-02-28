#ifndef BRUSHSTYLEBUTTONS_H
#define BRUSHSTYLEBUTTONS_H

#include <QToolButton>

namespace MoodBox
{

class BrushStyleWidget;

// Basic style button
class BrushStyleButton : public QToolButton
{
	Q_OBJECT

public:
	BrushStyleButton(QWidget *parent);

	virtual QColor getColor() const;

	virtual void reset();

public slots:
	virtual void setColor(const QColor &color);

protected:
	BrushStyleWidget *styleWidget;
	void updateIcon();

protected slots:
	virtual void onToggled(bool toggled);
	void onStyleWidgetClosed();
};

// Brush size button
class BrushSizeButton : public BrushStyleButton
{
	Q_OBJECT

public:
	BrushSizeButton(QWidget *parent);
	
	int getSizeIndex() const;

public slots:
	void setSizeIndex(int index);

signals:
	void sizeSelected(int sizeIndex);
};

// Brush alpha button
class BrushAlphaButton : public BrushStyleButton
{
	Q_OBJECT

public:
	BrushAlphaButton(QWidget *parent);

	int getAlphaIndex() const;

public slots:
	void setAlphaIndex(int index);
	void setSizeIndex(int index);

signals:
	void alphaSelected(int alphaIndex);
};

}

#endif // BRUSHSTYLEBUTTONS_H