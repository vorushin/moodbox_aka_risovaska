#include "xmlpropertyreader.h"

#include "xmlformats.h"

namespace MoodBox
{

XmlPropertyReader::XmlPropertyReader() : PropertyReader()
{
	model = NULL;
}
XmlPropertyReader::~XmlPropertyReader()
{
}

void XmlPropertyReader::setModel(Model* model)
{
	this->model = model;
}
void XmlPropertyReader::setCurrentValue(const QString &value)
{
	this->value = value;
}
void XmlPropertyReader::resetCurrentValue()
{
	value = QString::null;
}

QString XmlPropertyReader::readString()
{
	if(value.isNull())
		return "";

	return value;
}
QByteArray XmlPropertyReader::readBytes()
{
	return QByteArray::fromBase64(value.toAscii());
}
qint32 XmlPropertyReader::readInt32()
{
	return value.toInt();
}
qint64 XmlPropertyReader::readInt64()
{
	return value.toLongLong();
}
QDateTime XmlPropertyReader::readDateTime()
{
	QDateTime result = QDateTime::fromString(value, XML_DATE_TIME_FORMAT);
	result.setTimeSpec(Qt::UTC);

	return result;
}
QDate XmlPropertyReader::readDate()
{
	return readDateTime().date();
}
bool XmlPropertyReader::readBool()
{
	return value == "true";
}
qreal XmlPropertyReader::readDouble()
{
	return value.toDouble();
}

qint32 XmlPropertyReader::readEnum(qint32 enumId)
{
	return model->getEnumValueInfo(enumId, value).valueId;
}

}