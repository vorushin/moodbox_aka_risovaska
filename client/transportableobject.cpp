#include "transportableobject.h"

namespace MoodBox
{

TransportableObject::~TransportableObject()
{
}

PropertyInfo TransportableObject::getPropertyInfo(Model *model, QString name)
{
	return model->getPropertyInfo(getTypeId(), name);
}
void TransportableObject::writeProperties(PropertyWriter *writer)
{
	Q_UNUSED(writer);
}
PropertyReadResult TransportableObject::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
	Q_UNUSED(propertyId);
	Q_UNUSED(typeId);
	Q_UNUSED(reader);

	return PropertyReadResult(false);
}

}