#include "palettelist.h"

#include <QFile>
#include <QtAlgorithms>

namespace MoodBox
{

bool paletteLessByName(const Palette &p1, const Palette &p2)
{ 
	return p1.getName().toLower() < p2.getName().toLower();
}

// PaletteList class
PaletteList::PaletteList()
{
}

void PaletteList::setFileName(const QString &fileName)
{
	this->fileName = fileName;
}

bool PaletteList::load()
{
	QFile paletteFile(fileName);

	if (!paletteFile.open(QIODevice::ReadOnly))
		return false;

	QXmlStreamReader reader(&paletteFile);

	return (loadFromXml(&reader) == Ok);
}

bool PaletteList::save(bool overwrite) const
{
	QFile paletteFile(fileName);

	QFile::OpenMode openMode = QIODevice::ReadWrite | QIODevice::Text;

	if (overwrite)
		openMode |= QIODevice::Truncate;

	if (!paletteFile.open(openMode))
		return false;

	QXmlStreamWriter writer(&paletteFile);

	return (saveToXml(&writer) == Ok);
}

void PaletteList::sort(SortMode mode)
{
	if (mode == SortByPalette)
		qSort(begin(), end());
	else
		qSort(begin(), end(), paletteLessByName);
}

PaletteList::SerializationResult PaletteList::saveToXml(QXmlStreamWriter *writer) const
{
	if (writer == NULL)
		return BadDevice;

	writer->writeStartElement(PALETTELIST_XML_TAGNAME);
	
	for (int i = 0; i < size(); i++)
		at(i).saveToXml(writer);

	writer->writeEndElement();

	return Ok;
}

PaletteList::SerializationResult PaletteList::loadFromXml(QXmlStreamReader *reader)
{
	if (!moveToElement(reader, PALETTELIST_XML_TAGNAME))
		return BadFormat;

	clear();

	while (!reader->atEnd())
	{
		reader->readNext();

		Palette palette;

		if (palette.loadFromXml(reader) == Ok)
			append(palette);
		else
			break;
	}

	return Ok;
}

}