#ifndef CHECKINVITATION_H
#define CHECKINVITATION_H

#include <QSharedData>
#include <QString>

#include "transportableobject.h"

namespace MoodBox
{

class CheckInvitationData : public QSharedData
{
public:
    CheckInvitationData();
    CheckInvitationData(QString code);
    virtual ~CheckInvitationData();

    QString code;
};

class CheckInvitation : public TransportableObject
{
public:
    CheckInvitation();
    CheckInvitation(QString code);
    virtual ~CheckInvitation();

protected:
    CheckInvitation(CheckInvitationData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static CheckInvitation* ___new_()
    {
        return new CheckInvitation(new CheckInvitationData());
    }
    static CheckInvitation empty()
    {
        return CheckInvitation(new CheckInvitationData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    QString getCode() const;
    void setCode(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<CheckInvitationData> d;
};

}

#endif // CHECKINVITATION_H