#include "listwrapperobjects.h"
#include "fault.h"

namespace MoodBox
{

FaultData::FaultData() : QSharedData()
{
}
FaultData::FaultData(QString code, QString description, QString details) : QSharedData()
{
    this->code = code;
    this->description = description;
    this->details = details;
}

FaultData::~FaultData()
{
}

Fault::Fault() : TransportableObject()
{
}
Fault::Fault(QString code, QString description, QString details) : TransportableObject()
{
    d = new FaultData(code, description, details);
}

Fault::~Fault()
{
}

void Fault::resultFaultCall(ServerProxyBase* server, Callback callback, QVariant state, qint32 resultTypeId)
{
    server->resultFaultCall(callback, state, *this, resultTypeId);
}

QString Fault::getCode() const
{
    Q_ASSERT_X(!isNull(), "Fault::getCode", "Getter call on object which isNull");
    return this->d->code;
}
void Fault::setCode(QString value)
{
    Q_ASSERT_X(!isNull(), "Fault::setCode", "Setter call on object which isNull");
    this->d->code = value;
}
QString Fault::getDescription() const
{
    Q_ASSERT_X(!isNull(), "Fault::getDescription", "Getter call on object which isNull");
    return this->d->description;
}
void Fault::setDescription(QString value)
{
    Q_ASSERT_X(!isNull(), "Fault::setDescription", "Setter call on object which isNull");
    this->d->description = value;
}
QString Fault::getDetails() const
{
    Q_ASSERT_X(!isNull(), "Fault::getDetails", "Getter call on object which isNull");
    return this->d->details;
}
void Fault::setDetails(QString value)
{
    Q_ASSERT_X(!isNull(), "Fault::setDetails", "Setter call on object which isNull");
    this->d->details = value;
}

qint32 Fault::getRepresentedTypeId()
{
    return 3;
}

qint32 Fault::getTypeId() const
{
    return 3;
}
void Fault::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->code);
    writer->writeProperty(this, 2, this->d->description);
    writer->writeProperty(this, 3, this->d->details);
}
PropertyReadResult Fault::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->code = reader->readString();
            return PropertyReadResult(true);
        case 2:
            this->d->description = reader->readString();
            return PropertyReadResult(true);
        case 3:
            this->d->details = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
