#ifndef SERVERINFO_H
#define SERVERINFO_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class ServerInfoData : public QSharedData
{
public:
    ServerInfoData();
    ServerInfoData(bool isInvitationRequired, qint32 maxRequestSize);
    virtual ~ServerInfoData();

    bool isInvitationRequired;
    qint32 maxRequestSize;
};

class ServerInfo : public TransportableObject
{
public:
    ServerInfo();
    ServerInfo(bool isInvitationRequired, qint32 maxRequestSize);
    virtual ~ServerInfo();

protected:
    ServerInfo(ServerInfoData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static ServerInfo* ___new_()
    {
        return new ServerInfo(new ServerInfoData());
    }
    static ServerInfo empty()
    {
        return ServerInfo(new ServerInfoData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    bool getIsInvitationRequired() const;
    void setIsInvitationRequired(bool value);
    qint32 getMaxRequestSize() const;
    void setMaxRequestSize(qint32 value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<ServerInfoData> d;
};

}

#endif // SERVERINFO_H