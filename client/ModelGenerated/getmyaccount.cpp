#include "listwrapperobjects.h"
#include "getmyaccount.h"

namespace MoodBox
{

GetMyAccountData::GetMyAccountData() : QSharedData()
{
}

GetMyAccountData::~GetMyAccountData()
{
}

GetMyAccount::GetMyAccount() : TransportableObject()
{
}

GetMyAccount::~GetMyAccount()
{
}


qint32 GetMyAccount::getRepresentedTypeId()
{
    return 10073;
}

qint32 GetMyAccount::getTypeId() const
{
    return 10073;
}
void GetMyAccount::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

}
PropertyReadResult GetMyAccount::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    return PropertyReadResult(false);
}

}
