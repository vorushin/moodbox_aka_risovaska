#ifndef MOODSTRIPRESULT_H
#define MOODSTRIPRESULT_H

#include <QSharedData>
#include <QList>
#include <QDateTime>
#include <QString>

#include "moodstripitemresult.h"
#include "transportableobject.h"

namespace MoodBox
{

class MoodstripResultData : public QSharedData
{
public:
    MoodstripResultData();
    MoodstripResultData(qint32 moodstripId, qint32 authorId, QString authorLogin, QString title, QDateTime sendDate, QString url, QList<MoodstripItemResult> items, qint32 count);
    virtual ~MoodstripResultData();

    qint32 moodstripId;
    qint32 authorId;
    QString authorLogin;
    QString title;
    QDateTime sendDate;
    QString url;
    QList<MoodstripItemResult> items;
    qint32 count;
};

class MoodstripResult : public TransportableObject
{
public:
    MoodstripResult();
    MoodstripResult(qint32 moodstripId, qint32 authorId, QString authorLogin, QString title, QDateTime sendDate, QString url, QList<MoodstripItemResult> items, qint32 count);
    virtual ~MoodstripResult();

protected:
    MoodstripResult(MoodstripResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static MoodstripResult* ___new_()
    {
        return new MoodstripResult(new MoodstripResultData());
    }
    static MoodstripResult empty()
    {
        return MoodstripResult(new MoodstripResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getMoodstripId() const;
    void setMoodstripId(qint32 value);
    qint32 getAuthorId() const;
    void setAuthorId(qint32 value);
    QString getAuthorLogin() const;
    void setAuthorLogin(QString value);
    QString getTitle() const;
    void setTitle(QString value);
    QDateTime getSendDate() const;
    void setSendDate(QDateTime value);
    QString getUrl() const;
    void setUrl(QString value);
    QList<MoodstripItemResult> getItems() const;
    void setItems(QList<MoodstripItemResult> value);
    qint32 getCount() const;
    void setCount(qint32 value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<MoodstripResultData> d;
};

}

#endif // MOODSTRIPRESULT_H