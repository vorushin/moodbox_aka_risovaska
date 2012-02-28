#ifndef SCENECURSORLAYER_H
#define SCENECURSORLAYER_H

#include <QObject>
#include <QGraphicsItem>

namespace Velasquez
{

// Cursor layer item
#define CURSOR_LAYER_ZVALUE			2000000

// SceneCursorLayer is a class used to set scene cursors
class SceneCursorLayer : public QObject, public QGraphicsItem
{
	Q_OBJECT

public:
	enum { Type = 20000 };

public:
	SceneCursorLayer(QGraphicsScene *scene);

	virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget = 0);
	virtual QRectF boundingRect() const;

	virtual int type () const { return Type; };

private:
	QRectF rect;
	
private slots:
	void onSceneRectChanged(const QRectF &rect);

};

}

#endif // SCENECURSORLAYER_H
