#ifndef FAULT_H
#define FAULT_H

#include <QSharedData>
#include <QString>

#include "transportableobject.h"
#include "serverproxybase.h"

namespace MoodBox
{

class FaultData : public QSharedData
{
public:
    FaultData();
    FaultData(QString code, QString description, QString details);
    virtual ~FaultData();

    QString code;
    QString description;
    QString details;
};

class Fault : public TransportableObject
{
public:
    Fault();
    Fault(QString code, QString description, QString details);
    virtual ~Fault();

protected:
    Fault(FaultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static Fault* ___new_()
    {
        return new Fault(new FaultData());
    }
    static Fault empty()
    {
        return Fault(new FaultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual bool isFault() const
    {
        return true;
    }

    virtual void resultFaultCall(ServerProxyBase* server, Callback callback, QVariant state, qint32 resultTypeId);

    QString getCode() const;
    void setCode(QString value);
    QString getDescription() const;
    void setDescription(QString value);
    QString getDetails() const;
    void setDetails(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<FaultData> d;
};

}

#endif // FAULT_H