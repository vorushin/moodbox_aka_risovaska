#ifndef PALETTE_H
#define PALETTE_H

#include "color.h"

namespace MoodBox
{

#define PALETTE_COLOR_COUNT			16

#define PALETTE_XML_TAGNAME			"Palette"
#define PALETTE_NAME_XML_ATTRIBUTE	"Name"

#define PALETTE_DEFAULT_NAME		QT_TRANSLATE_NOOP("MoodBox::Palette", "NewPaletteDefaultName")

// Default palette filled with these colors
const Color DefaultPalette[PALETTE_COLOR_COUNT] =
{
	QColor(255, 0, 0),
	QColor(255, 170, 0),	
	QColor(170, 255, 0),	
	QColor(0, 255, 0),	
	QColor(0, 255, 170),
	QColor(0, 170, 255),
	QColor(255, 0, 255),
	QColor(255, 255, 255),

	QColor(255, 85, 0),
	QColor(255, 255, 0),
	QColor(85, 255, 0),
	QColor(0, 255, 85),
	QColor(0, 255, 255),	
	QColor(0, 0, 255),	
	QColor(170, 0, 255),			
	QColor(0, 0, 0)
};

// Palette is a limited set of colors, can serialize to xml
class Palette : public XmlSerializable
{
public:
	Palette();
	Palette(const Palette &palette);
	
	void setColor(int index, const QColor &color);
	QColor getColor(int index) const;

	virtual bool operator < (const Palette &other) const;	

	void setName(const QString &name);
	inline QString getName() const { return name; };

	virtual SerializationResult saveToXml(QXmlStreamWriter *writer) const;
	virtual SerializationResult loadFromXml(QXmlStreamReader *reader);

	// Returns all transparent palette
	static Palette transparentPalette();

protected:
	// Used in comparison operations
	virtual int getComparisonCode() const;

private:
	Color colors[PALETTE_COLOR_COUNT];
	QString name;
};

bool operator== (const Palette &p1, const Palette &p2);

}

#endif // PALETTE_H
