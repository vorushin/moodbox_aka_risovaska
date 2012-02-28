#include "listwrapperobjects.h"
#include "packageunion.h"

namespace MoodBox
{

PackageUnionData::PackageUnionData() : QSharedData()
{
    this->value = NULL;
}
PackageUnionData::PackageUnionData(TransportableObject* value) : QSharedData()
{
    this->value = value;
}

PackageUnionData::~PackageUnionData()
{
    if(this->value != NULL)
    {
        delete this->value;
        this->value = NULL;
    }
}

PackageUnion::PackageUnion() : TransportableObject()
{
}
PackageUnion::PackageUnion(TransportableObject* value) : TransportableObject()
{
    d = new PackageUnionData(value);
}

PackageUnion::~PackageUnion()
{
}

TransportableObject* PackageUnion::getValue() const
{
    Q_ASSERT_X(!isNull(), "PackageUnion::getValue", "Getter call on object which isNull");
    return this->d->value;
}
void PackageUnion::setValue(TransportableObject* value)
{
    Q_ASSERT_X(!isNull(), "PackageUnion::setValue", "Setter call on object which isNull");
    this->d->value = value;
}

qint32 PackageUnion::getRepresentedTypeId()
{
    return 33;
}

qint32 PackageUnion::getTypeId() const
{
    return 33;
}
void PackageUnion::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->value);
}
PropertyReadResult PackageUnion::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->value = NULL;
            switch(typeId)
            {
                case 34:
                    this->d->value = DeleteMessageCommand::___new_();
                    break;
            }
            return PropertyReadResult(this->d->value);
    }

    return PropertyReadResult(false);
}

}
