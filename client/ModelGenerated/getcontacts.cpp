#include "listwrapperobjects.h"
#include "getcontacts.h"

namespace MoodBox
{

GetContactsData::GetContactsData() : QSharedData()
{
}

GetContactsData::~GetContactsData()
{
}

GetContacts::GetContacts() : TransportableObject()
{
}

GetContacts::~GetContacts()
{
}


qint32 GetContacts::getRepresentedTypeId()
{
    return 10011;
}

qint32 GetContacts::getTypeId() const
{
    return 10011;
}
void GetContacts::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

}
PropertyReadResult GetContacts::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    return PropertyReadResult(false);
}

}
