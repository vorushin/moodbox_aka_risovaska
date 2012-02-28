#ifndef PROPERTYREADER_H
#define PROPERTYREADER_H

#include <QByteArray>
#include <QString>
#include <QDateTime>

namespace MoodBox
{

class TransportableObject;

class PropertyReadResult
{
public:
	PropertyReadResult(bool isPropertyFound);
	PropertyReadResult(TransportableObject *resultObject);

	bool getIsPropertyFound();
	TransportableObject *getResultObject();

private:
	bool isPropertyFound;
	TransportableObject *resultObject;
};

class PropertyReader
{
public:
	virtual ~PropertyReader()
	{
	}

	virtual QString readString() = 0;
	virtual QByteArray readBytes() = 0;
	virtual qint32 readInt32() = 0;
	virtual qint64 readInt64() = 0;
	virtual QDateTime readDateTime() = 0;
	virtual QDate readDate() = 0;
	virtual bool readBool() = 0;
	virtual qreal readDouble() = 0;

	virtual qint32 readEnum(qint32 enumId) = 0;
};

}

#endif // PROPERTYREADER_H
