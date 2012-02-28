#ifndef SVGTOOL_H
#define SVGTOOL_H

#include "transformabletool.h"

#include <QSizeF>

namespace Velasquez
{

// Tool for SVG elements
class SvgTool : public TransformableTool
{
	Q_OBJECT

public:
	SvgTool(QObject *parent);

	virtual qint32 getElementType() const;
	virtual QStringList getFileExtensions() const;

	virtual bool canCreate(QGraphicsSceneMouseEvent *mouseEvent) const;
	virtual bool canCreate(QKeyEvent *keyEvent) const;
	virtual bool canCreate(const QMimeData *data) const;

	virtual void createElement(QGraphicsSceneMouseEvent *mouseEvent);
	virtual void createElement(QKeyEvent *keyEvent);
	virtual void createElement(const QMimeData *data, const QPointF &pos = QPointF());

	static void setDefaultSize(const QSizeF &size);

private:
	static QSizeF defaultSize;
};

}

#endif // SVGTOOL_H
