#ifndef PROCESSAUTHORIZATIONREQUESTBYLOGIN_H
#define PROCESSAUTHORIZATIONREQUESTBYLOGIN_H

#include <QSharedData>
#include <QString>

#include "transportableobject.h"

namespace MoodBox
{

class ProcessAuthorizationRequestByLoginData : public QSharedData
{
public:
    ProcessAuthorizationRequestByLoginData();
    ProcessAuthorizationRequestByLoginData(QString recipientLogin, QString authorizationMessage);
    virtual ~ProcessAuthorizationRequestByLoginData();

    QString recipientLogin;
    QString authorizationMessage;
};

class ProcessAuthorizationRequestByLogin : public TransportableObject
{
public:
    ProcessAuthorizationRequestByLogin();
    ProcessAuthorizationRequestByLogin(QString recipientLogin, QString authorizationMessage);
    virtual ~ProcessAuthorizationRequestByLogin();

protected:
    ProcessAuthorizationRequestByLogin(ProcessAuthorizationRequestByLoginData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static ProcessAuthorizationRequestByLogin* ___new_()
    {
        return new ProcessAuthorizationRequestByLogin(new ProcessAuthorizationRequestByLoginData());
    }
    static ProcessAuthorizationRequestByLogin empty()
    {
        return ProcessAuthorizationRequestByLogin(new ProcessAuthorizationRequestByLoginData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    QString getRecipientLogin() const;
    void setRecipientLogin(QString value);
    QString getAuthorizationMessage() const;
    void setAuthorizationMessage(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<ProcessAuthorizationRequestByLoginData> d;
};

}

#endif // PROCESSAUTHORIZATIONREQUESTBYLOGIN_H