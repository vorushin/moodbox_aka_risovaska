#ifndef XMLPROPERTYWRITER_H
#define XMLPROPERTYWRITER_H

#include <QIODevice>
#include <QXmlStreamWriter>

#include "propertywriter.h"
#include "model.h"
#include "transportableobject.h"

#define Macros_WRITE(TYPE)	virtual void writeProperty(TransportableObject* object, qint32 propertyId, TYPE value);\
	virtual void writeProperty(TransportableObject* object, qint32 propertyId, QList<TYPE> value);

namespace MoodBox
{

class XmlPropertyWriter : public PropertyWriter
{
public:
	XmlPropertyWriter(Model *model, QIODevice *device);
	virtual ~XmlPropertyWriter();

	static void write(Model *model, QIODevice *device, TransportableObject* object);
	void write(TransportableObject* object);

	virtual void writeProperty(TransportableObject* object, qint32 propertyId, TransportableObject* value);
	Macros_WRITE(QByteArray)
	Macros_WRITE(QString)
	Macros_WRITE(qint32)
	Macros_WRITE(qint64)
	Macros_WRITE(QDateTime)
	Macros_WRITE(QDate)
	Macros_WRITE(bool)
	Macros_WRITE(qreal)

	virtual void writeProperty(TransportableObject* object, qint32 propertyId, AbstractTransportableListWrapper* value);
	virtual void writeProperty(TransportableObject* object, qint32 propertyId, AbstractTransportableListOfEnumWrapper* value);

	virtual void writeEnumProperty(TransportableObject* object, qint32 propertyId, qint32 enumId, qint32 value);

private:
	Model *model;

	QIODevice *device;
	QXmlStreamWriter writer;

	void writeEmptyElement(QString name, bool isNull);
};

}

#undef Macros_WRITE

#endif // XMLPROPERTYWRITER_H
