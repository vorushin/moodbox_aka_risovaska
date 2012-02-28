#ifndef GETNEXTARTMESSAGEANDREPORT_H
#define GETNEXTARTMESSAGEANDREPORT_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class GetNextArtmessageAndReportData : public QSharedData
{
public:
    GetNextArtmessageAndReportData();
    GetNextArtmessageAndReportData(qint32 previousMessageId);
    virtual ~GetNextArtmessageAndReportData();

    qint32 previousMessageId;
};

class GetNextArtmessageAndReport : public TransportableObject
{
public:
    GetNextArtmessageAndReport();
    GetNextArtmessageAndReport(qint32 previousMessageId);
    virtual ~GetNextArtmessageAndReport();

protected:
    GetNextArtmessageAndReport(GetNextArtmessageAndReportData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetNextArtmessageAndReport* ___new_()
    {
        return new GetNextArtmessageAndReport(new GetNextArtmessageAndReportData());
    }
    static GetNextArtmessageAndReport empty()
    {
        return GetNextArtmessageAndReport(new GetNextArtmessageAndReportData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getPreviousMessageId() const;
    void setPreviousMessageId(qint32 value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetNextArtmessageAndReportData> d;
};

}

#endif // GETNEXTARTMESSAGEANDREPORT_H