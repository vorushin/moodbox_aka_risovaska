#ifndef PALETTELIST_H
#define PALETTELIST_H

#include <QList>

#include "palette.h"

namespace MoodBox
{

#define PALETTELIST_XML_TAGNAME		"Palettes"

// List of palettes, can serialize to xml
class PaletteList : public QList<Palette>, public XmlSerializable
{
public:
	enum SortMode { SortByName, SortByPalette };

public:
	PaletteList();

	void setFileName(const QString &fileName);
	inline QString getFileName() const { return fileName; };

	bool load();
	bool save(bool overwrite = true) const;

	void sort(SortMode mode);

	virtual SerializationResult saveToXml(QXmlStreamWriter *writer) const;
	virtual SerializationResult loadFromXml(QXmlStreamReader *reader);

private:
	QString fileName;

};

}

#endif // PALETTELIST_H
