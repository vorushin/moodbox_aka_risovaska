#include "xmlpropertywriter.h"

#include "xmlformats.h"

#define Macros_WRITE_TEXT_NULLABLE(TYPE, TRANSLATION) void XmlPropertyWriter::writeProperty(TransportableObject* object, qint32 propertyId, TYPE value)\
{\
	QString name = model->getPropertyInfo(object->getTypeId(), propertyId).name;\
\
	if(value.isNull())\
		writeEmptyElement(name, true);\
	else\
		writer.writeTextElement(name, TRANSLATION);\
}\
void XmlPropertyWriter::writeProperty(TransportableObject* object, qint32 propertyId, QList<TYPE> list)\
{\
	QString name = model->getPropertyInfo(object->getTypeId(), propertyId).name;\
\
	writer.writeStartElement(name);\
	TYPE value;\
	foreach(value, list)\
	{\
		if(value.isNull())\
			writeEmptyElement(name, true);\
		else\
			writer.writeTextElement("li", TRANSLATION);\
	}\
	writer.writeEndElement();\
}\
\

#define Macros_WRITE_TEXT(TYPE, TRANSLATION) void XmlPropertyWriter::writeProperty(TransportableObject* object, qint32 propertyId, TYPE value)\
{\
	writer.writeTextElement(model->getPropertyInfo(object->getTypeId(), propertyId).name, TRANSLATION);\
}\
void XmlPropertyWriter::writeProperty(TransportableObject* object, qint32 propertyId, QList<TYPE> list)\
{\
	QString name = model->getPropertyInfo(object->getTypeId(), propertyId).name;\
\
	writer.writeStartElement(name);\
	TYPE value;\
	foreach(value, list)\
		writer.writeTextElement("li", TRANSLATION);\
	\
	writer.writeEndElement();\
}\
\


#define Macros_WRITE_LIST_INSTRUCTION(INSTRUCTION)	QString name = model->getPropertyInfo(object->getTypeId(), propertyId).name;\
\
	if(value == NULL)\
		writeEmptyElement(name, true);\
	else\
	{\
		writer.writeStartElement(name);\
		while(value->hasNext())\
			INSTRUCTION;\
		writer.writeEndElement();\
	}\

#define Macros_WRITE_LIST(TRANSLATION) Macros_WRITE_LIST_INSTRUCTION(writer.writeTextElement("li", TRANSLATION))

namespace MoodBox
{

XmlPropertyWriter::XmlPropertyWriter(Model *model, QIODevice *device)
{
	this->device = device;
	writer.setDevice(device);
	this->model = model;
}
XmlPropertyWriter::~XmlPropertyWriter()
{
}

void XmlPropertyWriter::write(Model *model, QIODevice *device, TransportableObject* object)
{
	XmlPropertyWriter writer(model, device);
	writer.write(object);
}

void XmlPropertyWriter::write(TransportableObject* object)
{
	writer.writeStartElement(model->getTypeInfo(object->getTypeId()).name);
	object->writeProperties(this);
	writer.writeEndElement();
}

void XmlPropertyWriter::writeProperty(TransportableObject* object, qint32 propertyId, TransportableObject* value)
{
	PropertyInfo info = model->getPropertyInfo(object->getTypeId(), propertyId);
	QString name = model->getPropertyInfo(object->getTypeId(), propertyId).name;

	if(value == NULL || value->isNull())
		writeEmptyElement(name, true);
	else
	{		
		writer.writeStartElement(name);
		if(info.isUnion)
			write(value);
		else
			value->writeProperties(this);
		writer.writeEndElement();
	}
}

Macros_WRITE_TEXT_NULLABLE(QByteArray, QString(value.toBase64()))
Macros_WRITE_TEXT_NULLABLE(QString, value)
Macros_WRITE_TEXT(qint32, QString::number(value))
Macros_WRITE_TEXT(qint64, QString::number(value))
Macros_WRITE_TEXT_NULLABLE(QDateTime, value.toString(XML_DATE_TIME_FORMAT))
Macros_WRITE_TEXT_NULLABLE(QDate, QDateTime(value).toString(XML_DATE_TIME_FORMAT))
Macros_WRITE_TEXT(bool, value ? "true" : "false")
Macros_WRITE_TEXT(qreal, QString::number(value))

void XmlPropertyWriter::writeProperty(TransportableObject* object, qint32 propertyId, AbstractTransportableListWrapper* value)
{
	Macros_WRITE_LIST_INSTRUCTION(value->getNextItem()->writeProperties(this))
}
void XmlPropertyWriter::writeProperty(TransportableObject* object, qint32 propertyId, AbstractTransportableListOfEnumWrapper* value)
{
	Macros_WRITE_LIST(model->getEnumValueInfo(value->getEnumId(), value->getNextItem()).valueName)
}

void XmlPropertyWriter::writeEnumProperty(TransportableObject* object, qint32 propertyId, qint32 enumId, qint32 value)
{
	PropertyInfo info = model->getPropertyInfo(object->getTypeId(), propertyId);

	writer.writeTextElement(info.name, model->getEnumValueInfo(enumId, value).valueName);
}

void XmlPropertyWriter::writeEmptyElement(QString name, bool isNull)
{
	writer.writeEmptyElement(name);

	if(isNull)
		writer.writeAttribute("isNull", "true");
}

}

#undef Macros_WRITE_TEXT_NULLABLE
#undef Macros_WRITE_TEXT
#undef Macros_WRITE_LIST_INSTRUCTION
#undef Macros_WRITE_LIST