#include "color.h"

#include "xmlutils.h"

namespace MoodBox
{

Color::Color() 
	: QColor()
{
}

Color::Color(int r, int g, int b, int a)
	: QColor(r, g, b, a)
{
}
	
Color::Color(QRgb color)
	: QColor(color)
{
}

Color::Color(const QString &name)
	: QColor(name)
{
}

Color::Color(const char *name)
	: QColor(name)
{
}

Color::Color(const QColor &color)
	: QColor(color)
{
}

Color::Color(Qt::GlobalColor color)
	: QColor(color)
{
}

Color::SerializationResult Color::saveToXml(QXmlStreamWriter *writer) const
{
	if (writer == NULL)
		return BadDevice;

	writer->writeEmptyElement(COLOR_XML_TAGNAME);
	QString colorString = XmlUtils::colorToHexString(*this);
	
	writer->writeAttribute(COLOR_VALUE_XML_ATTRIBUTE, colorString);

	return Ok;
}
	
Color::SerializationResult Color::loadFromXml(QXmlStreamReader *reader)
{
	if (!moveToElement(reader, COLOR_XML_TAGNAME))
		return BadFormat;

	QXmlStreamAttributes attributes = reader->attributes();

	QString colorString = attributes.value(COLOR_VALUE_XML_ATTRIBUTE).toString();

	QColor color = XmlUtils::hexStringToColor(colorString);

	if (!color.isValid())
		return BadFormat;

	setRgba(color.rgba());

	return Ok;
}

}