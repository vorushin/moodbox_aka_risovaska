#ifndef PROCESSAUTHORIZATIONREQUEST_H
#define PROCESSAUTHORIZATIONREQUEST_H

#include <QSharedData>
#include <QString>

#include "transportableobject.h"

namespace MoodBox
{

class ProcessAuthorizationRequestData : public QSharedData
{
public:
    ProcessAuthorizationRequestData();
    ProcessAuthorizationRequestData(qint32 recipientId, QString authorizationMessage);
    virtual ~ProcessAuthorizationRequestData();

    qint32 recipientId;
    QString authorizationMessage;
};

class ProcessAuthorizationRequest : public TransportableObject
{
public:
    ProcessAuthorizationRequest();
    ProcessAuthorizationRequest(qint32 recipientId, QString authorizationMessage);
    virtual ~ProcessAuthorizationRequest();

protected:
    ProcessAuthorizationRequest(ProcessAuthorizationRequestData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static ProcessAuthorizationRequest* ___new_()
    {
        return new ProcessAuthorizationRequest(new ProcessAuthorizationRequestData());
    }
    static ProcessAuthorizationRequest empty()
    {
        return ProcessAuthorizationRequest(new ProcessAuthorizationRequestData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getRecipientId() const;
    void setRecipientId(qint32 value);
    QString getAuthorizationMessage() const;
    void setAuthorizationMessage(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<ProcessAuthorizationRequestData> d;
};

}

#endif // PROCESSAUTHORIZATIONREQUEST_H