#include "palettelist.h"

#include <QFile>
#include <QtAlgorithms>

PaletteList::PaletteList()
{
}

void PaletteList::setFileName(const QString &fileName)
{
	this->fileName = fileName;
}

QString PaletteList::getFileName() const
{
	return fileName;
}

bool PaletteList::load()
{
	QFile paletteFile(fileName);

	if (!paletteFile.open(QIODevice::ReadOnly | QIODevice::Text))
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

void PaletteList::sort()
{
	qSort(begin(), end());
}

PaletteList::SerializationResult PaletteList::saveToXml(QXmlStreamWriter *writer) const
{
	if (writer == NULL)
		return BadDevice;

	writer->writeStartElement(PALETTE_LIST_ELEMENT_NAME);
	
	for (int i = 0; i < size(); i++)
		at(i).saveToXml(writer);

	writer->writeEndElement();

	return Ok;
}

PaletteList::SerializationResult PaletteList::loadFromXml(QXmlStreamReader *reader)
{
	if (!moveToElement(reader, PALETTE_LIST_ELEMENT_NAME))
		return BadFormat;

	clear();

	while(!reader->atEnd())
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
