#include "listwrapperobjects.h"
#include "addpicturetomoodstrip.h"

namespace MoodBox
{

AddPictureToMoodstripData::AddPictureToMoodstripData() : QSharedData()
{
    this->moodstripId = 0;
    this->messageId = 0;
    this->contentType = "image/png";
    this->isLast = false;
}
AddPictureToMoodstripData::AddPictureToMoodstripData(qint32 moodstripId, qint32 messageId, QString author, QByteArray data, QString contentType, bool isLast) : QSharedData()
{
    this->moodstripId = moodstripId;
    this->messageId = messageId;
    this->author = author;
    this->data = data;
    this->contentType = contentType;
    this->isLast = isLast;
}

AddPictureToMoodstripData::~AddPictureToMoodstripData()
{
}

AddPictureToMoodstrip::AddPictureToMoodstrip() : TransportableObject()
{
}
AddPictureToMoodstrip::AddPictureToMoodstrip(qint32 moodstripId, qint32 messageId, QString author, QByteArray data, QString contentType, bool isLast) : TransportableObject()
{
    d = new AddPictureToMoodstripData(moodstripId, messageId, author, data, contentType, isLast);
}

AddPictureToMoodstrip::~AddPictureToMoodstrip()
{
}

qint32 AddPictureToMoodstrip::getMoodstripId() const
{
    Q_ASSERT_X(!isNull(), "AddPictureToMoodstrip::getMoodstripId", "Getter call on object which isNull");
    return this->d->moodstripId;
}
void AddPictureToMoodstrip::setMoodstripId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "AddPictureToMoodstrip::setMoodstripId", "Setter call on object which isNull");
    this->d->moodstripId = value;
}
qint32 AddPictureToMoodstrip::getMessageId() const
{
    Q_ASSERT_X(!isNull(), "AddPictureToMoodstrip::getMessageId", "Getter call on object which isNull");
    return this->d->messageId;
}
void AddPictureToMoodstrip::setMessageId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "AddPictureToMoodstrip::setMessageId", "Setter call on object which isNull");
    this->d->messageId = value;
}
QString AddPictureToMoodstrip::getAuthor() const
{
    Q_ASSERT_X(!isNull(), "AddPictureToMoodstrip::getAuthor", "Getter call on object which isNull");
    return this->d->author;
}
void AddPictureToMoodstrip::setAuthor(QString value)
{
    Q_ASSERT_X(!isNull(), "AddPictureToMoodstrip::setAuthor", "Setter call on object which isNull");
    this->d->author = value;
}
QByteArray AddPictureToMoodstrip::getData() const
{
    Q_ASSERT_X(!isNull(), "AddPictureToMoodstrip::getData", "Getter call on object which isNull");
    return this->d->data;
}
void AddPictureToMoodstrip::setData(QByteArray value)
{
    Q_ASSERT_X(!isNull(), "AddPictureToMoodstrip::setData", "Setter call on object which isNull");
    this->d->data = value;
}
QString AddPictureToMoodstrip::getContentType() const
{
    Q_ASSERT_X(!isNull(), "AddPictureToMoodstrip::getContentType", "Getter call on object which isNull");
    return this->d->contentType;
}
void AddPictureToMoodstrip::setContentType(QString value)
{
    Q_ASSERT_X(!isNull(), "AddPictureToMoodstrip::setContentType", "Setter call on object which isNull");
    this->d->contentType = value;
}
bool AddPictureToMoodstrip::getIsLast() const
{
    Q_ASSERT_X(!isNull(), "AddPictureToMoodstrip::getIsLast", "Getter call on object which isNull");
    return this->d->isLast;
}
void AddPictureToMoodstrip::setIsLast(bool value)
{
    Q_ASSERT_X(!isNull(), "AddPictureToMoodstrip::setIsLast", "Setter call on object which isNull");
    this->d->isLast = value;
}

qint32 AddPictureToMoodstrip::getRepresentedTypeId()
{
    return 10071;
}

qint32 AddPictureToMoodstrip::getTypeId() const
{
    return 10071;
}
void AddPictureToMoodstrip::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->moodstripId);
    writer->writeProperty(this, 2, this->d->messageId);
    writer->writeProperty(this, 3, this->d->author);
    writer->writeProperty(this, 4, this->d->data);
    writer->writeProperty(this, 5, this->d->contentType);
    writer->writeProperty(this, 6, this->d->isLast);
}
PropertyReadResult AddPictureToMoodstrip::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
            this->d->messageId = reader->readInt32();
            return PropertyReadResult(true);
        case 3:
            this->d->author = reader->readString();
            return PropertyReadResult(true);
        case 4:
            this->d->data = reader->readBytes();
            return PropertyReadResult(true);
        case 5:
            this->d->contentType = reader->readString();
            return PropertyReadResult(true);
        case 6:
            this->d->isLast = reader->readBool();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
