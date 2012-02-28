#ifndef CONTACTLOGIN_H
#define CONTACTLOGIN_H

#include <QSharedData>
#include <QString>

#include "transportableobject.h"

namespace MoodBox
{

class ContactLoginData : public QSharedData
{
public:
    ContactLoginData();
    ContactLoginData(qint32 contactId, QString login);
    virtual ~ContactLoginData();

    qint32 contactId;
    QString login;
};

class ContactLogin : public TransportableObject
{
public:
    ContactLogin();
    ContactLogin(qint32 contactId, QString login);
    virtual ~ContactLogin();

protected:
    ContactLogin(ContactLoginData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static ContactLogin* ___new_()
    {
        return new ContactLogin(new ContactLoginData());
    }
    static ContactLogin empty()
    {
        return ContactLogin(new ContactLoginData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getContactId() const;
    void setContactId(qint32 value);
    QString getLogin() const;
    void setLogin(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<ContactLoginData> d;
};

}

#endif // CONTACTLOGIN_H