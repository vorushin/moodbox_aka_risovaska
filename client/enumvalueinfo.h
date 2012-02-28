#ifndef ENUMVALUEINFO_H
#define ENUMVALUEINFO_H

#include <QtGlobal>

namespace MoodBox
{

struct EnumValueInfo
{
	EnumValueInfo()
	{
		enumId = 0;
		valueId = 0;
	}
	EnumValueInfo(qint32 enumId, qint32 valueId, QString valueName)
	{
		this->enumId = enumId;
		this->valueId = valueId;
		this->valueName = valueName;
	}

	qint32 enumId;
	qint32 valueId;
	QString valueName;
};

}

#endif // ENUMVALUEINFO_H
