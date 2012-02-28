#ifndef COLOR_H
#define COLOR_H

#include <QColor>

#include "xmlserializable.h"

#define COLOR_ELEMENT_NAME		"color"
#define COLOR_RED_NAME			"r"
#define COLOR_GREEN_NAME		"g"
#define COLOR_BLUE_NAME			"b"
#define COLOR_ALPHA_NAME		"a"

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
	
	virtual ~Color() {};

	virtual SerializationResult saveToXml(QXmlStreamWriter *writer) const;
	virtual SerializationResult loadFromXml(QXmlStreamReader *reader);
};

#endif // COLOR_H
