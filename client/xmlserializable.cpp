#include "xmlserializable.h"

#include <QString>

#include "debug.h"

namespace MoodBox
{

// Comment to hide debug
//#define SHOW_XMLSERIALIZE_DEBUG					true

#if (defined(UDEBUG)) && (defined(SHOW_XMLSERIALIZE_DEBUG))
#define XMLSERDEBUG(x)	QDEBUG(x)
#else
#define XMLSERDEBUG(x)
#endif

XmlSerializable::~XmlSerializable()
{
}

// Move to start element in specified reader, check name if specified. If global is true the search will check all document
bool XmlSerializable::moveToElement(QXmlStreamReader* reader, const QString &name, SearchMode mode, const QString &scope)
{
	if (reader == NULL)
		return false;

	QString stopName = (scope.isEmpty()) ? reader->name().toString() : scope;

	while (!reader->atEnd()) 
	{
        if (reader->isStartElement())
		{
			if (reader->name() == name)
			{
				XMLSERDEBUG("XmlSerializable::moving, successful search name - " << name << " mode - " << mode << " pos - " << reader->characterOffset());
				return true;
			}
			else
			{
				if (mode == Local)
				{
					XMLSERDEBUG("XmlSerializable::moving - local only, giving up - " << name << " pos - " << reader->characterOffset());
					return false;
				}
			}
		}
		else
			if (reader->isEndElement())
				if (mode == Scope && reader->name() == stopName)
				{
					XMLSERDEBUG("XmlSerializable::moving - end of scope, giving up - " << name << " pos - " << reader->characterOffset());
					return false;
				}

				XMLSERDEBUG("XmlSerializable::moving, now - " << reader->name().toString() << " needed - " << name << " mode - " << mode << "scope" << stopName << " token type - " << reader->tokenString() << " pos - " << reader->characterOffset());

		reader->readNext();
	}

	XMLSERDEBUG("XmlSerializable::moving - end of file, giving up - " << name << " pos - " << reader->characterOffset());
	return false;
}

bool XmlSerializable::leaveElement(QXmlStreamReader* reader, const QString &name, SearchMode mode, const QString &scope)
{
	if (reader == NULL)
		return false;

	QString stopName = (scope.isEmpty()) ? reader->name().toString() : scope;

	while (!reader->atEnd()) 
	{
        if (reader->isEndElement())
		{
			if (reader->name() == name)
			{
				XMLSERDEBUG("XmlSerializable::leaving, successful search name - " << name << " mode - " << mode << " pos - " << reader->characterOffset());
				return true;
			}
			else
			{
				if (mode == Local)
				{
					XMLSERDEBUG("XmlSerializable::leaving - local only, giving up - " << name << " pos - " << reader->characterOffset());
					return false;
				}
			}
		}
		else
			if (reader->isEndElement())
				if (mode == Scope && reader->name() == stopName)
				{
					XMLSERDEBUG("XmlSerializable::leaving - end of scope, giving up - " << name << " pos - " << reader->characterOffset());
					return false;
				}

		XMLSERDEBUG("XmlSerializable::leaving, now - " << reader->name().toString() << " needed - " << name << " mode - " << mode << "scope" << stopName << " token type - " << reader->tokenString() << " pos - " << reader->characterOffset());

		reader->readNext();
	}

	XMLSERDEBUG("XmlSerializable::leaving - end of file, giving up - " << name << " pos - " << reader->characterOffset());
	return false;
}

// Move out of the element
bool XmlSerializable::moveFromElement(QXmlStreamReader *reader, QXmlStreamReader::TokenType tokenType)
{
	if (reader == NULL)
		return false;

	while (reader->tokenType() == tokenType)
	{

		XMLSERDEBUG("XmlSerializable::moveFromElement token type " << reader->tokenType() << " pos - " << reader->characterOffset());

		reader->readNext();
	}

	return true;
}


}