#ifndef MOODSTRIPITEMRESULT_H
#define MOODSTRIPITEMRESULT_H

#include <QSharedData>
#include <QDateTime>
#include <QString>

#include "transportableobject.h"

namespace MoodBox
{

class MoodstripItemResultData : public QSharedData
{
public:
    MoodstripItemResultData();
    MoodstripItemResultData(QString url, QString authorLogin, QDateTime sendDate);
    virtual ~MoodstripItemResultData();

    QString url;
    QString authorLogin;
    QDateTime sendDate;
};

class MoodstripItemResult : public TransportableObject
{
public:
    MoodstripItemResult();
    MoodstripItemResult(QString url, QString authorLogin, QDateTime sendDate);
    virtual ~MoodstripItemResult();

protected:
    MoodstripItemResult(MoodstripItemResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static MoodstripItemResult* ___new_()
    {
        return new MoodstripItemResult(new MoodstripItemResultData());
    }
    static MoodstripItemResult empty()
    {
        return MoodstripItemResult(new MoodstripItemResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    QString getUrl() const;
    void setUrl(QString value);
    QString getAuthorLogin() const;
    void setAuthorLogin(QString value);
    QDateTime getSendDate() const;
    void setSendDate(QDateTime value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<MoodstripItemResultData> d;
};

}

#endif // MOODSTRIPITEMRESULT_H