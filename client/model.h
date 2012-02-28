#ifndef MODEL_H
#define MODEL_H

#include <QHash>

#include "typeinfo.h"
#include "propertyinfo.h"
#include "enumvalueinfo.h"

namespace MoodBox
{

template <class T1, class T2>
struct HashPair : public QPair<T1, T2>
{
	HashPair() : QPair<T1, T2>()
	{
	}
	HashPair(const T1 &value1, const T2 &value2) : QPair<T1, T2>(value1, value2)
	{
	}

	uint qHash(const HashPair<T1, T2> &key)
    {
        return qHash(key.first()) ^ qHash(key.second());
    }
};

class Model
{
public:
	Model();
	virtual ~Model();

	void addTypeInfo(TypeInfo info);
	void addPropertyInfo(PropertyInfo info);
	void addEnumValueInfo(EnumValueInfo info);

	TypeInfo getTypeInfo(const QString typeName);
	TypeInfo getTypeInfo(const qint32 typeId);
	PropertyInfo getPropertyInfo(const qint32 typeId, const QString propertyName);
	PropertyInfo getPropertyInfo(const qint32 typeId, const qint32 propertyId);
	EnumValueInfo getEnumValueInfo(const qint32 typeId, const QString valueName);
	EnumValueInfo getEnumValueInfo(const qint32 typeId, const qint32 valueId);

	virtual void fill() = 0;

private:
	QHash<QString, TypeInfo> typeHashByName;
	QHash<qint32, TypeInfo> typeHashById;
	QHash<HashPair<qint32, QString>, PropertyInfo> propertyHashByName;
	QHash<HashPair<qint32, qint32>, PropertyInfo> propertyHashById;
	QHash<HashPair<qint32, QString>, EnumValueInfo> enumHashByName;
	QHash<HashPair<qint32, qint32>, EnumValueInfo> enumHashById;
};

}

#endif // MODEL_H
