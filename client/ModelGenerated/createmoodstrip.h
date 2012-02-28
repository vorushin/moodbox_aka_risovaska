#ifndef CREATEMOODSTRIP_H
#define CREATEMOODSTRIP_H

#include <QSharedData>
#include <QString>

#include "transportableobject.h"

namespace MoodBox
{

class CreateMoodstripData : public QSharedData
{
public:
    CreateMoodstripData();
    CreateMoodstripData(QString caption, bool isHidden, qint32 channelId);
    virtual ~CreateMoodstripData();

    QString caption;
    bool isHidden;
    qint32 channelId;
};

class CreateMoodstrip : public TransportableObject
{
public:
    CreateMoodstrip();
    CreateMoodstrip(QString caption, bool isHidden, qint32 channelId);
    virtual ~CreateMoodstrip();

protected:
    CreateMoodstrip(CreateMoodstripData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static CreateMoodstrip* ___new_()
    {
        return new CreateMoodstrip(new CreateMoodstripData());
    }
    static CreateMoodstrip empty()
    {
        return CreateMoodstrip(new CreateMoodstripData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    QString getCaption() const;
    void setCaption(QString value);
    bool getIsHidden() const;
    void setIsHidden(bool value);
    qint32 getChannelId() const;
    void setChannelId(qint32 value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<CreateMoodstripData> d;
};

}

#endif // CREATEMOODSTRIP_H