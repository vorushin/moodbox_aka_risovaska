#include "palette.h"

Palette::Palette()
{
	for (int i = 0; i < PALETTE_COLOR_COUNT; i++)
		this->colors[i] = DefaultPalette[i];
}

Palette::Palette(const Palette &palette)
	: XmlSerializable()
{
	for (int i = 0; i < PALETTE_COLOR_COUNT; i++)
		this->colors[i] = palette.colors[i];
}
	
void Palette::setColor(int index, const QColor &color)
{
	if (index < 0 || index >= PALETTE_COLOR_COUNT)
		return;

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

Palette::SerializationResult Palette::saveToXml(QXmlStreamWriter *writer) const
{
	if (writer == NULL)
		return BadDevice;

	writer->writeStartElement(PALETTE_ELEMENT_NAME);
	
	for (int i = 0; i < PALETTE_COLOR_COUNT; i ++)
		colors[i].saveToXml(writer);

	writer->writeEndElement();

	return Ok;
}

Palette::SerializationResult Palette::loadFromXml(QXmlStreamReader *reader)
{
	if (!moveToElement(reader, PALETTE_ELEMENT_NAME))
		return BadFormat;

	for (int i = 0; i < PALETTE_COLOR_COUNT; i ++)
	{
		reader->readNext();

		if (colors[i].loadFromXml(reader) != Ok)
			break;
	}

	return Ok;
}

int Palette::getComparisonCode() const
{
	int code = 0;

	for (int i = 0; i < PALETTE_COLOR_COUNT; i++)
		code += getColor(i).rgba();

	return code;
}
