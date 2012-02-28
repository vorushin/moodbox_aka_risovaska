#ifndef UPDATEPASSWORD_H
#define UPDATEPASSWORD_H

#include <QSharedData>
#include <QString>

#include "transportableobject.h"

namespace MoodBox
{

class UpdatePasswordData : public QSharedData
{
public:
    UpdatePasswordData();
    UpdatePasswordData(QString newPassword, QString oldPassword);
    virtual ~UpdatePasswordData();

    QString newPassword;
    QString oldPassword;
};

class UpdatePassword : public TransportableObject
{
public:
    UpdatePassword();
    UpdatePassword(QString newPassword, QString oldPassword);
    virtual ~UpdatePassword();

protected:
    UpdatePassword(UpdatePasswordData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static UpdatePassword* ___new_()
    {
        return new UpdatePassword(new UpdatePasswordData());
    }
    static UpdatePassword empty()
    {
        return UpdatePassword(new UpdatePasswordData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    QString getNewPassword() const;
    void setNewPassword(QString value);
    QString getOldPassword() const;
    void setOldPassword(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<UpdatePasswordData> d;
};

}

#endif // UPDATEPASSWORD_H