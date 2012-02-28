#ifndef PALETTELIST_H
#define PALETTELIST_H

#include <QList>

#include "palette.h"

class PaletteList : public QList<Palette>, public XmlSerializable
{
public:
	PaletteList();

	void setFileName(const QString &fileName);
	QString getFileName() const;

	bool load();
	bool save(bool overwrite = true) const;

	void sort();

	virtual SerializationResult saveToXml(QXmlStreamWriter *writer) const;
	virtual SerializationResult loadFromXml(QXmlStreamReader *reader);

private:
	QString fileName;

};

#endif // PALETTELIST_H
