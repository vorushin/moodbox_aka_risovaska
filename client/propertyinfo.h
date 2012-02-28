#ifndef PROPERTYINFO_H
#define PROPERTYINFO_H

#include <QtGlobal>

namespace MoodBox
{

struct PropertyInfo
{
	PropertyInfo()
	{
		typeId = 0;
		uid = 0;
		isCustomTypeOrList = false;
		isUnion = false;
	}
	PropertyInfo(bool isCustomTypeOrList, bool isUnion)
	{
		typeId = 0;
		uid = 0;
		this->isCustomTypeOrList = isCustomTypeOrList;
		this->isUnion = isUnion;
	}
	PropertyInfo(qint32 typeId, qint32 uid, QString name, bool isCustomTypeOrList, bool isUnion)
	{
		this->typeId = typeId;
		this->uid = uid;
		this->name = name;
		this->isCustomTypeOrList = isCustomTypeOrList;
		this->isUnion = isUnion;
	}

	qint32 typeId;
	qint32 uid;
	QString name;
	bool isCustomTypeOrList;
	bool isUnion;
};

}

#endif // PROPERTYINFO_H
