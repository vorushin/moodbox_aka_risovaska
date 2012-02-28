#include "svgelement.h"

#include <QSvgRenderer>
#include <QFile>
#include <QCursor>

namespace Velasquez
{

SvgElement::SvgElement(QGraphicsItem *parentItem)
	: TransformableElement(parentItem)
{
	init();
}

SvgElement::SvgElement(const QString &fileName, QGraphicsItem *parentItem )
	: TransformableElement(parentItem)
{
	init();

	// We need to cache data for further serialization
	QFile file(fileName);

	if (!file.open(QIODevice::ReadOnly))
		return;

	this->content = file.readAll();
	
	clearAndSetContent(this->content);

	load();
}

SvgElement::SvgElement(const QByteArray &content, QGraphicsItem *parentItem)
	: TransformableElement(parentItem)
{
	init();

	this->content = content;
	
	clearAndSetContent(this->content);

	load();
}

bool SvgElement::isEmpty() const
{
	return (content.isEmpty() || !renderer->isValid());
}

void SvgElement::setSetting(qint32 id, const QVariant &value)
{
	Q_UNUSED(id)
	Q_UNUSED(value)
}

QVariant SvgElement::getSetting(qint32 id) const 
{
	Q_UNUSED(id)
	
	return QVariant(); 
}
	
void SvgElement::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
	if (isEmpty())
		return;

	renderer->render(painter, rect);

	TransformableElement::paint(painter, option, widget);
}

QRectF SvgElement::boundingRect() const
{
	return rect;
}

// clear SvgContent and remain data only between <svg>...</svg> tags
bool SvgElement::clearAndSetContent(const QByteArray &dirtyContent)
{
	if (dirtyContent.isEmpty())
		return false;

	qint64 startFrom = dirtyContent.indexOf(START_SVG_XML_TAGNAME);

	qint64 endOn = dirtyContent.indexOf(END_SVG_XML_TAGNAME);

	if (startFrom == -1 || endOn == -1)
		return false;
	
	endOn += QString(END_SVG_XML_TAGNAME).length();
	
	this->content = dirtyContent.mid(startFrom, endOn - startFrom);

	return true;
}

qint32 SvgElement::getType() const
{
	return Type;
}

void SvgElement::init()
{
	setCursor(Qt::PointingHandCursor);

	renderer = new QSvgRenderer(this);

    connect(renderer, SIGNAL(repaintNeeded()), this, SLOT(onRepaintNeeded()));
}

void SvgElement::load()
{
	if (!renderer->load(content))
	{
		rect = QRectF();
	}
	else
	{
		QRectF bounds = QRectF(QPointF(0, 0), renderer->defaultSize());

        if (rect.size() != bounds.size()) 
		{
            prepareGeometryChange();
            rect.setSize(bounds.size());
        }
	}
}

void SvgElement::onRepaintNeeded()
{
	update();
}

}
