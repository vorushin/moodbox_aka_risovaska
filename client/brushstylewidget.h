#ifndef BRUSHSTYLEWIDGET_H
#define BRUSHSTYLEWIDGET_H

#include <QWidget>
#include <QItemDelegate>
#include <QPersistentModelIndex>

#include "brushstyle.h"
#include "ui_brushstylewidget.h"

class QStandardItemModel;

namespace MoodBox
{

using namespace Ui;

class BrushStyleListItemDelegate;

// List of brush styles: alpha or size
class BrushStyleWidget : public QWidget, public BrushStyleWidgetClass
{
	Q_OBJECT

public:
	BrushStyleWidget(QWidget *parent = 0);

	void setStyle(BrushStyle::BrushStyleEnum style);
	BrushStyle::BrushStyleEnum getStyle() const;

	void setSizeIndex(int sizeIndex);
	int getSizeIndex() const;

	void setAlphaIndex(int alphaIndex);
	int getAlphaIndex() const;

	void setColor(const QColor &color);
	inline QColor getColor() const { return currentColor; };

	QListView* getStyleList() const { return styleList; };

	void reset();

signals:
	void sizeSelected(int sizeIndex);
	void alphaSelected(int alphaIndex);

	void closed();

protected:
	virtual void closeEvent(QCloseEvent *event);
	virtual void leaveEvent(QEvent *event);

private:
	BrushStyle::BrushStyleEnum style;
	int currentSizeIndex, currentAlphaIndex;
	QColor currentColor;

	QStandardItemModel *itemModel;
	BrushStyleListItemDelegate *itemDelegate;

	void buildStyleList();

	// Styles are displayed from big to small and this function converts index to reverse order
	int revertIndex(int index) const;

private slots:
	void onItemSelected(const QModelIndex &index);
	void onItemHovered(const QModelIndex &index);

};

// Custom painting of list items
class BrushStyleListItemDelegate : public QItemDelegate
{
    Q_OBJECT

public:
    BrushStyleListItemDelegate(BrushStyleWidget *parent): QItemDelegate(parent), parentStyleWidget(parent), hoveredIndexSet(false) { };

    virtual void paint(QPainter *painter, const QStyleOptionViewItem &option, const QModelIndex &index) const;
    virtual QSize sizeHint(const QStyleOptionViewItem &option, const QModelIndex &index) const;
	
	virtual QWidget *createEditor(QWidget *parent, const QStyleOptionViewItem &option, const QModelIndex &index); 

	void setHoveredIndex(const QModelIndex &index);
	inline QModelIndex getHoveredIndex() const { return hoveredIndex; };
	void clearHoveredIndex();

private:
	BrushStyleWidget *parentStyleWidget;
	QPersistentModelIndex hoveredIndex;
	bool hoveredIndexSet;
};

}

#endif // BRUSHSTYLEWIDGET_H
