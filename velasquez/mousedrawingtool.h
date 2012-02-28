#ifndef MOUSEDRAWINGTOOL_H
#define MOUSEDRAWINGTOOL_H

#include "drawingtool.h"

namespace Velasquez
{

class MouseDrawingElement;

// Basic tool for mouse drawing elements
class MouseDrawingTool : public DrawingTool
{
	Q_OBJECT

public:
	MouseDrawingTool(QObject *parent);

	virtual bool hasCursor() const;
	virtual QCursor getDefaultCursor() const;

	virtual QStringList getFileExtensions() const;

	virtual bool canCreate(QGraphicsSceneMouseEvent *mouseEvent) const;
	virtual bool canCreate(QKeyEvent *keyEvent) const;
	virtual bool canCreate(const QMimeData *data) const;

	virtual void createElement(QGraphicsSceneMouseEvent *mouseEvent);
	virtual void createElement(QKeyEvent *keyEvent);
	virtual void createElement(const QMimeData *data, const QPointF &pos = QPointF());

protected:
	MouseDrawingElement *currentElement;

	virtual MouseDrawingElement *createNewElement() const = 0;
	
	virtual void startCreating();
	virtual void runCreating(QGraphicsSceneMouseEvent *mouseEvent);
	virtual void finishCreating();

};

}

#endif // MOUSEDRAWINGTOOL_H
