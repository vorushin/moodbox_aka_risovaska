#ifndef BACKGROUNDTOOL_H
#define BACKGROUNDTOOL_H

#include "drawingtool.h"

namespace Velasquez
{

class BackgroundElement;

// Background management tool
class BackgroundTool : public DrawingTool
{
	Q_OBJECT

public:
	BackgroundTool(QObject *parent);

	virtual void setScene(EditorScene *scene);

	virtual qint32 getElementType() const;
	virtual QStringList getFileExtensions() const;

	virtual bool addElement(DrawingElement *element);

	virtual bool canCreate(QGraphicsSceneMouseEvent *mouseEvent) const;
	virtual bool canCreate(QKeyEvent *keyEvent) const;
	virtual bool canCreate(const QMimeData *data) const;

	virtual void createElement(QGraphicsSceneMouseEvent *mouseEvent);
	virtual void createElement(QKeyEvent *keyEvent);
	virtual void createElement(const QMimeData *data, const QPointF &pos = QPointF());

	virtual bool hasDeletableElements() const;
	virtual void deleteAll();

protected slots:
	virtual void onSettingChanged(qint32 id);

	void onSceneCleared();
};

}

#endif // BACKGROUNDTOOL_H