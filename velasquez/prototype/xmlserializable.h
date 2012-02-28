#ifndef XMLSERIALIZABLE_PROTO_H
#define XMLSERIALIZABLE_PROTO_H

#include <QString>

#include <QXmlStreamWriter>
#include <QXmlStreamReader>

// Implementers of this class can load and store their content to XML
class XmlSerializable
{
public:
	enum SerializationResult {Ok, BadDevice, BadFormat};
	
	virtual ~XmlSerializable() {};

	// Save object state to XML
	virtual SerializationResult saveToXml(QXmlStreamWriter *writer) const = 0;

	// Load object state from XML
	virtual SerializationResult loadFromXml(QXmlStreamReader *reader) = 0;

	// Move to start element in specified reader, check name if specified. If global is true the search will check all document
	static bool moveToElement(QXmlStreamReader* reader, const QString &name = QString(), bool global = false);

	// Move out of the element
	static bool moveFromElement(QXmlStreamReader *reader, QXmlStreamReader::TokenType tokenType);
};

#endif // XMLSERIALIZABLE_PROTO_H
