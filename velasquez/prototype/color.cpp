#include "color.h"

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

	writer->writeEmptyElement(COLOR_ELEMENT_NAME);
	
	writer->writeAttribute(COLOR_RED_NAME, QString::number(red()));
	writer->writeAttribute(COLOR_GREEN_NAME, QString::number(green()));
	writer->writeAttribute(COLOR_BLUE_NAME, QString::number(blue()));
	writer->writeAttribute(COLOR_ALPHA_NAME, QString::number(alpha()));

	return Ok;
}
	
Color::SerializationResult Color::loadFromXml(QXmlStreamReader *reader)
{
	if (!moveToElement(reader, COLOR_ELEMENT_NAME))
		return BadFormat;

	QXmlStreamAttributes attrs = reader->attributes();

	if (attrs.count() < 3)
		return BadFormat;

	QStringRef attrValue = attrs.value(COLOR_RED_NAME);
	setRed(attrValue.toString().toInt());

	attrValue = attrs.value(COLOR_GREEN_NAME);
	setGreen(attrValue.toString().toInt());
	
	attrValue = attrs.value(COLOR_BLUE_NAME);
	setBlue(attrValue.toString().toInt());

	attrValue = attrs.value(COLOR_ALPHA_NAME);
	setAlpha(attrValue.toString().toInt());

	return Ok;
}
