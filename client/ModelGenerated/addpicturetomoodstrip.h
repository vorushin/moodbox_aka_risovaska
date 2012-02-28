#ifndef ADDPICTURETOMOODSTRIP_H
#define ADDPICTURETOMOODSTRIP_H

#include <QSharedData>
#include <QString>
#include <QByteArray>

#include "transportableobject.h"

namespace MoodBox
{

class AddPictureToMoodstripData : public QSharedData
{
public:
    AddPictureToMoodstripData();
    AddPictureToMoodstripData(qint32 moodstripId, qint32 messageId, QString author, QByteArray data, QString contentType, bool isLast);
    virtual ~AddPictureToMoodstripData();

    qint32 moodstripId;
    qint32 messageId;
    QString author;
    QByteArray data;
    QString contentType;
    bool isLast;
};

class AddPictureToMoodstrip : public TransportableObject
{
public:
    AddPictureToMoodstrip();
    AddPictureToMoodstrip(qint32 moodstripId, qint32 messageId, QString author, QByteArray data, QString contentType, bool isLast);
    virtual ~AddPictureToMoodstrip();

protected:
    AddPictureToMoodstrip(AddPictureToMoodstripData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static AddPictureToMoodstrip* ___new_()
    {
        return new AddPictureToMoodstrip(new AddPictureToMoodstripData());
    }
    static AddPictureToMoodstrip empty()
    {
        return AddPictureToMoodstrip(new AddPictureToMoodstripData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getMoodstripId() const;
    void setMoodstripId(qint32 value);
    qint32 getMessageId() const;
    void setMessageId(qint32 value);
    QString getAuthor() const;
    void setAuthor(QString value);
    QByteArray getData() const;
    void setData(QByteArray value);
    QString getContentType() const;
    void setContentType(QString value);
    bool getIsLast() const;
    void setIsLast(bool value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<AddPictureToMoodstripData> d;
};

}

#endif // ADDPICTURETOMOODSTRIP_H