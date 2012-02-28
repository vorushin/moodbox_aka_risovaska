#ifndef XMLPROPERTYREADER_H
#define XMLPROPERTYREADER_H

#include <QXmlDefaultHandler>

#include "propertyreader.h"
#include "transportableobject.h"

namespace MoodBox
{

class XmlPropertyReader : public PropertyReader
{
public:
	XmlPropertyReader();
	virtual ~XmlPropertyReader();

	void setModel(Model* model);
	void setCurrentValue(const QString &value);
	void resetCurrentValue();

	virtual QString readString();
	virtual QByteArray readBytes();
	virtual qint32 readInt32();
	virtual qint64 readInt64();
	virtual QDateTime readDateTime();
	virtual QDate readDate();
	virtual bool readBool();
	virtual qreal readDouble();

	virtual qint32 readEnum(qint32 enumId);

private:
	Model *model;
	QString value;
};

}

#endif // XMLPROPERTYREADER_H
