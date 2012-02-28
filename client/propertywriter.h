#ifndef PROPERTYWRITER_H
#define PROPERTYWRITER_H

#include <QByteArray>
#include <QString>
#include <QDateTime>

#include "transportablelistwrapper.h"

#define Macros_WRITE(TYPE)	virtual void writeProperty(TransportableObject* object, qint32 propertyId, TYPE value) = 0;\
	virtual void writeProperty(TransportableObject* object, qint32 propertyId, QList<TYPE> value) = 0;

namespace MoodBox
{

class TransportableObject;

class PropertyWriter
{
public:
	virtual ~PropertyWriter()
	{
	}

	virtual void writeProperty(TransportableObject* object, qint32 propertyId, TransportableObject* value) = 0;
	Macros_WRITE(QByteArray)
	Macros_WRITE(QString)
	Macros_WRITE(qint32)
	Macros_WRITE(qint64)
	Macros_WRITE(QDateTime)
	Macros_WRITE(QDate)
	Macros_WRITE(bool)
	Macros_WRITE(qreal)

	virtual void writeProperty(TransportableObject* object, qint32 propertyId, AbstractTransportableListWrapper* value) = 0;
	virtual void writeProperty(TransportableObject* object, qint32 propertyId, AbstractTransportableListOfEnumWrapper* value) = 0;

	virtual void writeEnumProperty(TransportableObject* object, qint32 propertyId, qint32 enumId, qint32 value) = 0;
};

}

#undef Macros_WRITE

#endif // PROPERTYWRITER_H
