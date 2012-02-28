#ifndef EYEDROPPERTOOL_H
#define EYEDROPPERTOOL_H

#include "drawingtool.h"

#include <QStringList>

namespace Velasquez
{

#define EYEDROPPER_CURSOR		":/Velasquez/Resources/Eyedropper.png"

// Cursor hotspot coordinates
#define EYEDROPPER_CURSOR_X		1
#define EYEDROPPER_CURSOR_Y		13

class EyedropperTool : public DrawingTool
{
	Q_OBJECT

public:
	enum {Type = 10009};

public:
	EyedropperTool(QObject *parent);

	virtual bool hasCursor() const;
	virtual QCursor getDefaultCursor() const;

	virtual qint32 getElementType() const;
	virtual QStringList getFileExtensions() const;

	virtual bool canCreate(QGraphicsSceneMouseEvent *mouseEvent) const;
	virtual bool canCreate(QKeyEvent *keyEvent) const;
	virtual bool canCreate(const QMimeData *data) const;

	virtual void createElement(QGraphicsSceneMouseEvent *mouseEvent);
	virtual void createElement(QKeyEvent *keyEvent);
	virtual void createElement(const QMimeData *data, const QPointF &pos = QPointF());

signals:
	void colorPicked(const QColor &color);

};

}

#endif // EYEDROPPERTOOL_H
