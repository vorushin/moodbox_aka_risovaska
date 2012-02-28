#include "listwrapperobjects.h"
#include "moodstripitemresult.h"

namespace MoodBox
{

MoodstripItemResultData::MoodstripItemResultData() : QSharedData()
{
}
MoodstripItemResultData::MoodstripItemResultData(QString url, QString authorLogin, QDateTime sendDate) : QSharedData()
{
    this->url = url;
    this->authorLogin = authorLogin;
    this->sendDate = sendDate;
}

MoodstripItemResultData::~MoodstripItemResultData()
{
}

MoodstripItemResult::MoodstripItemResult() : TransportableObject()
{
}
MoodstripItemResult::MoodstripItemResult(QString url, QString authorLogin, QDateTime sendDate) : TransportableObject()
{
    d = new MoodstripItemResultData(url, authorLogin, sendDate);
}

MoodstripItemResult::~MoodstripItemResult()
{
}

QString MoodstripItemResult::getUrl() const
{
    Q_ASSERT_X(!isNull(), "MoodstripItemResult::getUrl", "Getter call on object which isNull");
    return this->d->url;
}
void MoodstripItemResult::setUrl(QString value)
{
    Q_ASSERT_X(!isNull(), "MoodstripItemResult::setUrl", "Setter call on object which isNull");
    this->d->url = value;
}
QString MoodstripItemResult::getAuthorLogin() const
{
    Q_ASSERT_X(!isNull(), "MoodstripItemResult::getAuthorLogin", "Getter call on object which isNull");
    return this->d->authorLogin;
}
void MoodstripItemResult::setAuthorLogin(QString value)
{
    Q_ASSERT_X(!isNull(), "MoodstripItemResult::setAuthorLogin", "Setter call on object which isNull");
    this->d->authorLogin = value;
}
QDateTime MoodstripItemResult::getSendDate() const
{
    Q_ASSERT_X(!isNull(), "MoodstripItemResult::getSendDate", "Getter call on object which isNull");
    return this->d->sendDate;
}
void MoodstripItemResult::setSendDate(QDateTime value)
{
    Q_ASSERT_X(!isNull(), "MoodstripItemResult::setSendDate", "Setter call on object which isNull");
    this->d->sendDate = value;
}

qint32 MoodstripItemResult::getRepresentedTypeId()
{
    return 13;
}

qint32 MoodstripItemResult::getTypeId() const
{
    return 13;
}
void MoodstripItemResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->url);
    writer->writeProperty(this, 2, this->d->authorLogin);
    writer->writeProperty(this, 3, this->d->sendDate);
}
PropertyReadResult MoodstripItemResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->url = reader->readString();
            return PropertyReadResult(true);
        case 2:
            this->d->authorLogin = reader->readString();
            return PropertyReadResult(true);
        case 3:
            this->d->sendDate = reader->readDateTime();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
