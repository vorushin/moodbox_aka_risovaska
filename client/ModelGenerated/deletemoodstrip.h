#ifndef DELETEMOODSTRIP_H
#define DELETEMOODSTRIP_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class DeleteMoodstripData : public QSharedData
{
public:
    DeleteMoodstripData();
    DeleteMoodstripData(qint32 moodstripId);
    virtual ~DeleteMoodstripData();

    qint32 moodstripId;
};

class DeleteMoodstrip : public TransportableObject
{
public:
    DeleteMoodstrip();
    DeleteMoodstrip(qint32 moodstripId);
    virtual ~DeleteMoodstrip();

protected:
    DeleteMoodstrip(DeleteMoodstripData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static DeleteMoodstrip* ___new_()
    {
        return new DeleteMoodstrip(new DeleteMoodstripData());
    }
    static DeleteMoodstrip empty()
    {
        return DeleteMoodstrip(new DeleteMoodstripData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getMoodstripId() const;
    void setMoodstripId(qint32 value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<DeleteMoodstripData> d;
};

}

#endif // DELETEMOODSTRIP_H