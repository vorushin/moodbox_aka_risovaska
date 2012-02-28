#include "model.h"

namespace MoodBox
{

Model::Model()
{
}

Model::~Model()
{
}

void Model::addTypeInfo(const TypeInfo info)
{
	typeHashByName.insert(info.name, info);
	typeHashById.insert(info.uid, info);
}
void Model::addPropertyInfo(const PropertyInfo info)
{
	propertyHashByName.insert(HashPair<qint32, QString>(info.typeId, info.name), info);
	propertyHashById.insert(HashPair<qint32, qint32>(info.typeId, info.uid), info);
}
void Model::addEnumValueInfo(const EnumValueInfo info)
{
	enumHashByName.insert(HashPair<qint32, QString>(info.enumId, info.valueName), info);
	enumHashById.insert(HashPair<qint32, qint32>(info.enumId, info.valueId), info);
}

TypeInfo Model::getTypeInfo(const QString typeName)
{
	return typeHashByName.value(typeName);
}
TypeInfo Model::getTypeInfo(const qint32 typeId)
{
	return typeHashById.value(typeId);
}

PropertyInfo Model::getPropertyInfo(const qint32 typeId, const QString propertyName)
{
	return propertyHashByName.value(HashPair<qint32, QString>(typeId, propertyName));
}
PropertyInfo Model::getPropertyInfo(const qint32 typeId, const qint32 propertyId)
{
	return propertyHashById.value(HashPair<qint32, qint32>(typeId, propertyId));
}

EnumValueInfo Model::getEnumValueInfo(const qint32 enumId, const QString enumValueName)
{
	return enumHashByName.value(HashPair<qint32, QString>(enumId, enumValueName));
}
EnumValueInfo Model::getEnumValueInfo(const qint32 enumId, const qint32 enumValueId)
{
	return enumHashById.value(HashPair<qint32, qint32>(enumId, enumValueId));
}

}