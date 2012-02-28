#ifndef PUBLISHINGMOODSTRIPRESULT_H
#define PUBLISHINGMOODSTRIPRESULT_H

#include <QSharedData>
#include <QList>

#include "transportableobject.h"
#include "moodstripresultcode.h"
#include "publishingway.h"

namespace MoodBox
{

class PublishingMoodstripResultData : public QSharedData
{
public:
    PublishingMoodstripResultData();
    PublishingMoodstripResultData(MoodstripResultCode::MoodstripResultCodeEnum resultCode, QList<PublishingWay> urls);
    virtual ~PublishingMoodstripResultData();

    MoodstripResultCode::MoodstripResultCodeEnum resultCode;
    QList<PublishingWay> urls;
};

class PublishingMoodstripResult : public TransportableObject
{
public:
    PublishingMoodstripResult();
    PublishingMoodstripResult(MoodstripResultCode::MoodstripResultCodeEnum resultCode, QList<PublishingWay> urls);
    virtual ~PublishingMoodstripResult();

protected:
    PublishingMoodstripResult(PublishingMoodstripResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static PublishingMoodstripResult* ___new_()
    {
        return new PublishingMoodstripResult(new PublishingMoodstripResultData());
    }
    static PublishingMoodstripResult empty()
    {
        return PublishingMoodstripResult(new PublishingMoodstripResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    MoodstripResultCode::MoodstripResultCodeEnum getResultCode() const;
    void setResultCode(MoodstripResultCode::MoodstripResultCodeEnum value);
    QList<PublishingWay> getUrls() const;
    void setUrls(QList<PublishingWay> value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<PublishingMoodstripResultData> d;
};

}

#endif // PUBLISHINGMOODSTRIPRESULT_H