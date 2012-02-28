#ifndef RESETPASSWORD_H
#define RESETPASSWORD_H

#include <QSharedData>
#include <QString>

#include "transportableobject.h"

namespace MoodBox
{

class ResetPasswordData : public QSharedData
{
public:
    ResetPasswordData();
    ResetPasswordData(QString login);
    virtual ~ResetPasswordData();

    QString login;
};

class ResetPassword : public TransportableObject
{
public:
    ResetPassword();
    ResetPassword(QString login);
    virtual ~ResetPassword();

protected:
    ResetPassword(ResetPasswordData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static ResetPassword* ___new_()
    {
        return new ResetPassword(new ResetPasswordData());
    }
    static ResetPassword empty()
    {
        return ResetPassword(new ResetPasswordData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    QString getLogin() const;
    void setLogin(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<ResetPasswordData> d;
};

}

#endif // RESETPASSWORD_H