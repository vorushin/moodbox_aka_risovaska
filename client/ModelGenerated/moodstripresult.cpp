#include "listwrapperobjects.h"
#include "moodstripresult.h"

namespace MoodBox
{

MoodstripResultData::MoodstripResultData() : QSharedData()
{
    this->moodstripId = 0;
    this->authorId = 0;
    this->count = 0;
}
MoodstripResultData::MoodstripResultData(qint32 moodstripId, qint32 authorId, QString authorLogin, QString title, QDateTime sendDate, QString url, QList<MoodstripItemResult> items, qint32 count) : QSharedData()
{
    this->moodstripId = moodstripId;
    this->authorId = authorId;
    this->authorLogin = authorLogin;
    this->title = title;
    this->sendDate = sendDate;
    this->url = url;
    this->items = items;
    this->count = count;
}

MoodstripResultData::~MoodstripResultData()
{
}

MoodstripResult::MoodstripResult() : TransportableObject()
{
}
MoodstripResult::MoodstripResult(qint32 moodstripId, qint32 authorId, QString authorLogin, QString title, QDateTime sendDate, QString url, QList<MoodstripItemResult> items, qint32 count) : TransportableObject()
{
    d = new MoodstripResultData(moodstripId, authorId, authorLogin, title, sendDate, url, items, count);
}

MoodstripResult::~MoodstripResult()
{
}

qint32 MoodstripResult::getMoodstripId() const
{
    Q_ASSERT_X(!isNull(), "MoodstripResult::getMoodstripId", "Getter call on object which isNull");
    return this->d->moodstripId;
}
void MoodstripResult::setMoodstripId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "MoodstripResult::setMoodstripId", "Setter call on object which isNull");
    this->d->moodstripId = value;
}
qint32 MoodstripResult::getAuthorId() const
{
    Q_ASSERT_X(!isNull(), "MoodstripResult::getAuthorId", "Getter call on object which isNull");
    return this->d->authorId;
}
void MoodstripResult::setAuthorId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "MoodstripResult::setAuthorId", "Setter call on object which isNull");
    this->d->authorId = value;
}
QString MoodstripResult::getAuthorLogin() const
{
    Q_ASSERT_X(!isNull(), "MoodstripResult::getAuthorLogin", "Getter call on object which isNull");
    return this->d->authorLogin;
}
void MoodstripResult::setAuthorLogin(QString value)
{
    Q_ASSERT_X(!isNull(), "MoodstripResult::setAuthorLogin", "Setter call on object which isNull");
    this->d->authorLogin = value;
}
QString MoodstripResult::getTitle() const
{
    Q_ASSERT_X(!isNull(), "MoodstripResult::getTitle", "Getter call on object which isNull");
    return this->d->title;
}
void MoodstripResult::setTitle(QString value)
{
    Q_ASSERT_X(!isNull(), "MoodstripResult::setTitle", "Setter call on object which isNull");
    this->d->title = value;
}
QDateTime MoodstripResult::getSendDate() const
{
    Q_ASSERT_X(!isNull(), "MoodstripResult::getSendDate", "Getter call on object which isNull");
    return this->d->sendDate;
}
void MoodstripResult::setSendDate(QDateTime value)
{
    Q_ASSERT_X(!isNull(), "MoodstripResult::setSendDate", "Setter call on object which isNull");
    this->d->sendDate = value;
}
QString MoodstripResult::getUrl() const
{
    Q_ASSERT_X(!isNull(), "MoodstripResult::getUrl", "Getter call on object which isNull");
    return this->d->url;
}
void MoodstripResult::setUrl(QString value)
{
    Q_ASSERT_X(!isNull(), "MoodstripResult::setUrl", "Setter call on object which isNull");
    this->d->url = value;
}
QList<MoodstripItemResult> MoodstripResult::getItems() const
{
    Q_ASSERT_X(!isNull(), "MoodstripResult::getItems", "Getter call on object which isNull");
    return this->d->items;
}
void MoodstripResult::setItems(QList<MoodstripItemResult> value)
{
    Q_ASSERT_X(!isNull(), "MoodstripResult::setItems", "Setter call on object which isNull");
    this->d->items = value;
}
qint32 MoodstripResult::getCount() const
{
    Q_ASSERT_X(!isNull(), "MoodstripResult::getCount", "Getter call on object which isNull");
    return this->d->count;
}
void MoodstripResult::setCount(qint32 value)
{
    Q_ASSERT_X(!isNull(), "MoodstripResult::setCount", "Setter call on object which isNull");
    this->d->count = value;
}

qint32 MoodstripResult::getRepresentedTypeId()
{
    return 12;
}

qint32 MoodstripResult::getTypeId() const
{
    return 12;
}
void MoodstripResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->moodstripId);
    writer->writeProperty(this, 2, this->d->authorId);
    writer->writeProperty(this, 3, this->d->authorLogin);
    writer->writeProperty(this, 4, this->d->title);
    writer->writeProperty(this, 5, this->d->sendDate);
    writer->writeProperty(this, 6, this->d->url);
    TransportableListOfSharedWrapper<MoodstripItemResult> items_wrapper(this->d->items);
    writer->writeProperty(this, 7, &items_wrapper);
    writer->writeProperty(this, 8, this->d->count);
}
PropertyReadResult MoodstripResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->moodstripId = reader->readInt32();
            return PropertyReadResult(true);
        case 2:
            this->d->authorId = reader->readInt32();
            return PropertyReadResult(true);
        case 3:
            this->d->authorLogin = reader->readString();
            return PropertyReadResult(true);
        case 4:
            this->d->title = reader->readString();
            return PropertyReadResult(true);
        case 5:
            this->d->sendDate = reader->readDateTime();
            return PropertyReadResult(true);
        case 6:
            this->d->url = reader->readString();
            return PropertyReadResult(true);
        case 7:
            this->d->items = QList<MoodstripItemResult>();
            return PropertyReadResult(new ListOfSharedWrapperObject<MoodstripItemResult>(&this->d->items, PropertyInfo(true, false)));
        case 8:
            this->d->count = reader->readInt32();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
