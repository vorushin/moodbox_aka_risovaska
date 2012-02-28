#include "xmlserializable.h"

bool XmlSerializable::moveToElement(QXmlStreamReader* reader, const QString &name, bool global)
{
	if (reader == NULL)
		return false;

	while (!reader->atEnd()) 
	{

#ifdef _DEBUG
		QString debugName = reader->name().toString();
#endif

        if (reader->isStartElement())
		{
			if (!name.isEmpty())
			{
				if (reader->name() == name)
					return true;
				else
					if (!global)
						return false;
			}
			else
				return true;
		}

		reader->readNext();
	}

	return false;
}

bool XmlSerializable::moveFromElement(QXmlStreamReader *reader, QXmlStreamReader::TokenType tokenType)
{
	if (reader == NULL)
		return false;

	while(reader->tokenType() == tokenType)
	{

#ifdef _DEBUG
		QXmlStreamReader::TokenType debugType = reader->tokenType();
#endif
		reader->readNext();
	}

	return true;
}
