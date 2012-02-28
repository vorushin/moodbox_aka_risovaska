#ifndef DRAWINGELEMENT_H
#define DRAWINGELEMENT_H

#include <QGraphicsItem>
#include <QList>
#include <QImage>

#include "metainfoprovider.h"

class QPainter;
class QMenu;

namespace Velasquez
{

// Base class for drawing elements
// Responsible for: painting, synchronizing actions with tool, user interaction 
class DrawingElement : public QGraphicsItem, public MetaInfoProvider
{
public:
	DrawingElement(QGraphicsItem *parent = 0);

	virtual int type() const;

	// Check object for emptyness
	virtual bool isEmpty() const = 0;

	// Checks object for selection/deselection availability
	inline virtual bool isSelectable() const { return true; };
	inline virtual bool isDeselectable() const { return true; };
	
	// Settings management functions
	virtual void setSetting(qint32 id, const QVariant &value) = 0;
	virtual QVariant getSetting(qint32 id) const = 0;
	virtual QList <qint32> getSettingsList() const = 0;
	virtual bool isSettingReversible(qint32 id) const;

	// Scene event function
	virtual void sceneEvent(qint32 id, const QVariant &data);

protected:
	// This function should be overridden to return element type
	virtual qint32 getType() const = 0;

	// Utility function to check item bounds when painting
	virtual void paintBoundingFrame(QPainter *painter);
	
	// Get context menu
	virtual QMenu *getContextMenu() { return NULL; };
	// Run context menu
	virtual bool showContextMenu(const QPointF &pos);
	// Call context menu event
	void contextMenuEvent(QGraphicsSceneContextMenuEvent *event);
};

}

#endif // DRAWINGELEMENT_H
