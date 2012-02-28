#ifndef GETCOMMANDS_H
#define GETCOMMANDS_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class GetCommandsData : public QSharedData
{
public:
    GetCommandsData();
    GetCommandsData(qint32 previousPackageId);
    virtual ~GetCommandsData();

    qint32 previousPackageId;
};

class GetCommands : public TransportableObject
{
public:
    GetCommands();
    GetCommands(qint32 previousPackageId);
    virtual ~GetCommands();

protected:
    GetCommands(GetCommandsData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetCommands* ___new_()
    {
        return new GetCommands(new GetCommandsData());
    }
    static GetCommands empty()
    {
        return GetCommands(new GetCommandsData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getPreviousPackageId() const;
    void setPreviousPackageId(qint32 value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetCommandsData> d;
};

}

#endif // GETCOMMANDS_H