#ifndef BrushStyleWidget_H
#define BrushStyleWidget_H

#include <QDialog>

#include "ui_brushstylewidget.h"

using namespace Ui;

class QStandardItemModel;

class BrushStyleWidget : public QWidget, public BrushStyleWidgetClass
{
	Q_OBJECT

public:
	enum Style {Size, Alpha};

	BrushStyleWidget(QWidget *parent = 0);
	~BrushStyleWidget();

	void setStyle(Style style);
	Style getStyle() const;

	void setCurrentSize(qreal size);
	qreal getCurrentSize() const;

	void setCurrentAlpha(qreal alpha);
	qreal getCurrentAlpha() const;

	void setColor(const QColor &color);
	QColor getColor() const;

signals:
	void sizeSelected(qreal size);
	void alphaSelected(qreal alpha);

	void closed();

protected:
	virtual void closeEvent(QCloseEvent *event);

private slots:
	void onItemSelected();

private:
	void buildStyleList();

	void setCurrentStyleValue(qreal value);
	qreal getCurrentStyleValue() const;

	Style style;
	qreal currentSize, currentAlpha;

	QColor color;

	QStandardItemModel *itemModel;
};

#endif // BrushStyleWidget_H
