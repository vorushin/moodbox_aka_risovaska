#ifndef SVGELEMENT_H
#define SVGELEMENT_H

#include <QByteArray>

#include "transformableelement.h"

class QSvgRenderer;

namespace Velasquez
{
	
// SVG tags and attributes
#define START_SVG_XML_TAGNAME			"<svg"
#define END_SVG_XML_TAGNAME				"svg>"

// Class for SVG elements
class SvgElement : public TransformableElement
{
	Q_OBJECT

public:
	enum {Type = 10001};

public:	
	SvgElement(QGraphicsItem *parentItem = 0);
	SvgElement(const QString &fileName, QGraphicsItem *parentItem = 0);
	SvgElement(const QByteArray &content, QGraphicsItem *parentItem = 0);

	virtual bool isEmpty() const;
		
	virtual void setSetting(qint32 id, const QVariant &value);
	virtual QVariant getSetting(qint32 id) const;
	inline virtual QList <qint32> getSettingsList() const { return QList<qint32>(); };

	virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget = 0);
	virtual QRectF boundingRect() const;
	
	// Clears dirty content and store data only from <svg> to </svg> tags
	bool clearAndSetContent(const QByteArray &dirtyContent);

protected:
	virtual qint32 getType() const;

private:
	QSvgRenderer *renderer;
    QRectF rect;
	QByteArray content;

	void init();
	void load();

private slots:
	void onRepaintNeeded();
	
};

}

#endif // SVGELEMENT_H
