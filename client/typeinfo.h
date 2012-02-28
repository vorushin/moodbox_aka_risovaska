#ifndef TYPEINFO_H
#define TYPEINFO_H

#include <QtGlobal>

namespace MoodBox
{

struct TypeInfo
{
	TypeInfo()
	{
		uid = 0;
	}
	TypeInfo(qint32 uid, QString name)
	{
		this->uid = uid;
		this->name = name;
	}

	qint32 uid;
	QString name;
};

}

#endif // TYPEINFO_H
