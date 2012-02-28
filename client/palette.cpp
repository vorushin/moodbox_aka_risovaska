#include "palette.h"

#include <QObject>

namespace MoodBox
{

Palette::Palette()
{
	for (int i = 0; i < PALETTE_COLOR_COUNT; i++)
		this->colors[i] = DefaultPalette[i];

	name = QObject::tr(PALETTE_DEFAULT_NAME);
}

Palette::Palette(const Palette &palette)
	: XmlSerializable()
{
	for (int i = 0; i < PALETTE_COLOR_COUNT; i++)
		this->colors[i] = palette.colors[i];

	name = palette.name;
}
	
void Palette::setColor(int index, const QColor &color)
{
	if (index < 0 || index >= PALETTE_COLOR_COUNT)
		return;

	Q_ASSERT_X(color.isValid(), "Palette::setColor", "Trying to set invalid color");

	colors[index] = color;
}

QColor Palette::getColor(int index) const
{
	if (index < 0 || index >= PALETTE_COLOR_COUNT)
		return QColor();

	return colors[index];
}

bool Palette::operator < (const Palette &other) const
{
	return this->getComparisonCode() < other.getComparisonCode();
}

void Palette::setName(const QString &name)
{
	this->name = name;
}

Palette::SerializationResult Palette::saveToXml(QXmlStreamWriter *writer) const
{
	if (writer == NULL)
		return BadDevice;

	writer->writeStartElement(PALETTE_XML_TAGNAME);
	writer->writeAttribute(PALETTE_NAME_XML_ATTRIBUTE, name);
	
	for (int i = 0; i < PALETTE_COLOR_COUNT; i ++)
		colors[i].saveToXml(writer);

	writer->writeEndElement();

	return Ok;
}

Palette::SerializationResult Palette::loadFromXml(QXmlStreamReader *reader)
{
	if (!moveToElement(reader, PALETTE_XML_TAGNAME))
		return BadFormat;

	QXmlStreamAttributes attributes = reader->attributes();
	name = attributes.value(PALETTE_NAME_XML_ATTRIBUTE).toString();

	for (int i = 0; i < PALETTE_COLOR_COUNT; i++)
	{
		reader->readNext();

		// Fill the rest with transparent color
		if (colors[i].loadFromXml(reader) != Ok)
		{
			for (int j = i; j < PALETTE_COLOR_COUNT; j++)
				colors[j] = QColor(0, 0, 0, 0);

			return Ok;
		}
	}

	reader->readNext();

	return Ok;
}

Palette Palette::transparentPalette()
{
	Palette palette;

	for (int i = 0; i < PALETTE_COLOR_COUNT; i++)
		palette.setColor(i, QColor(0, 0, 0, 0));

	return palette;
}

int Palette::getComparisonCode() const
{
	int code = 0;

	for (int i = 0; i < PALETTE_COLOR_COUNT; i++)
		code += getColor(i).rgba();

	return code;
}

bool operator== (const Palette &p1, const Palette &p2)
{
	for (int i = 0; i < PALETTE_COLOR_COUNT; i++)
		if (p1.getColor(i) != p2.getColor(i))
			return false;

	return true;
}

}