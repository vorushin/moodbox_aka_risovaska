#ifndef BRUSHSTYLEBUTTON_H
#define BRUSHSTYLEBUTTON_H

#include <QToolButton>

#include "brushstylewidget.h"

class BrushStyleButton : public QToolButton
{
	Q_OBJECT

public:
	BrushStyleButton(QWidget *parent);
	~BrushStyleButton();

	BrushStyleWidget *getStyleWidget();
	QColor getColor() const;

public slots:
	void setColor(const QColor &color);

protected slots:
	virtual void onToggled(bool toggled);
	void onStyleWidgetClosed();

protected:
	void updateIcon();

	BrushStyleWidget *styleWidget;	
};

class BrushSizeButton : public BrushStyleButton
{
	Q_OBJECT

public:
	BrushSizeButton(QWidget *parent);
	
	qreal getSize() const;

public slots:
	void setSize(qreal size);

signals:
	void sizeSelected(qreal size);
};

class BrushAlphaButton : public BrushStyleButton
{
	Q_OBJECT

public:
	BrushAlphaButton(QWidget *parent);

	qreal getAlpha() const;

public slots:
	void setAlpha(qreal alpha);
	void setSize(qreal size);

signals:
	void alphaSelected(qreal alpha);
};

#endif // BRUSHSTYLEBUTTON_H
