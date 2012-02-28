#ifndef PALETTE_H
#define PALETTE_H

#include "color.h"

#define PALETTE_COLOR_COUNT			18

#define PALETTE_ELEMENT_NAME		"palette"

#define PALETTE_LIST_ELEMENT_NAME	"palettes"

const Color DefaultPalette[PALETTE_COLOR_COUNT] =
{
	QColor(255, 0, 0),
	QColor(255, 170, 0),	
	QColor(170, 255, 0),	
	QColor(0, 255, 0),	
	QColor(0, 255, 170),
	QColor(0, 170, 255),
	QColor(0, 0, 255),
	QColor(170, 0, 255),
	QColor(255, 0, 85),	

	QColor(255, 85, 0),
	QColor(255, 255, 0),
	QColor(85, 255, 0),
	QColor(0, 255, 85),
	QColor(0, 255, 255),	
	QColor(0, 85, 255),	
	QColor(85, 0, 255),	
	QColor(255, 0, 255),			
	QColor(255, 255, 255)
};

class Palette : public XmlSerializable
{
public:
	Palette();
	Palette(const Palette &palette);
	
	virtual ~Palette() {};
	
	void setColor(int index, const QColor &color);
	QColor getColor(int index) const;

	virtual bool operator < (const Palette &other) const;

	virtual SerializationResult saveToXml(QXmlStreamWriter *writer) const;
	virtual SerializationResult loadFromXml(QXmlStreamReader *reader);

protected:
	virtual int getComparisonCode() const;

private:
	Color colors[PALETTE_COLOR_COUNT];
};

#endif // PALETTE_H
