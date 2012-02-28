#ifndef COLOR_H
#define COLOR_H

#include <QColor>

#include "xmlserializable.h"

#define COLOR_XML_TAGNAME			"Color"
#define COLOR_VALUE_XML_ATTRIBUTE	"Value"

namespace MoodBox
{

// Palette color class, has the same functionality as QColor but can serialize to XML
class Color : public QColor, public XmlSerializable
{
public:
	Color();
	Color(int r, int g, int b, int a = 255);
	Color(QRgb color);
	Color(const QString &name);
	Color(const char *name);
	Color(const QColor &color);
	Color(Qt::GlobalColor color);

	virtual SerializationResult saveToXml(QXmlStreamWriter *writer) const;
	virtual SerializationResult loadFromXml(QXmlStreamReader *reader);
};

}

#endif // COLOR_H
