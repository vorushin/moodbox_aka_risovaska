#ifndef PROCESSAUTHORIZATIONRESPONSE_H
#define PROCESSAUTHORIZATIONRESPONSE_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class ProcessAuthorizationResponseData : public QSharedData
{
public:
    ProcessAuthorizationResponseData();
    ProcessAuthorizationResponseData(qint32 recipientId, bool isAccepted);
    virtual ~ProcessAuthorizationResponseData();

    qint32 recipientId;
    bool isAccepted;
};

class ProcessAuthorizationResponse : public TransportableObject
{
public:
    ProcessAuthorizationResponse();
    ProcessAuthorizationResponse(qint32 recipientId, bool isAccepted);
    virtual ~ProcessAuthorizationResponse();

protected:
    ProcessAuthorizationResponse(ProcessAuthorizationResponseData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static ProcessAuthorizationResponse* ___new_()
    {
        return new ProcessAuthorizationResponse(new ProcessAuthorizationResponseData());
    }
    static ProcessAuthorizationResponse empty()
    {
        return ProcessAuthorizationResponse(new ProcessAuthorizationResponseData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getRecipientId() const;
    void setRecipientId(qint32 value);
    bool getIsAccepted() const;
    void setIsAccepted(bool value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<ProcessAuthorizationResponseData> d;
};

}

#endif // PROCESSAUTHORIZATIONRESPONSE_H