#include "listwrapperobjects.h"
#include "envelope.h"

namespace MoodBox
{

EnvelopeData::EnvelopeData() : QSharedData()
{
    this->body = NULL;
}
EnvelopeData::EnvelopeData(Header header, TransportableObject* body) : QSharedData()
{
    this->header = header;
    this->body = body;
}

EnvelopeData::~EnvelopeData()
{
    if(this->body != NULL)
    {
        delete this->body;
        this->body = NULL;
    }
}

Envelope::Envelope() : TransportableObject()
{
}
Envelope::Envelope(Header header, TransportableObject* body) : TransportableObject()
{
    d = new EnvelopeData(header, body);
}

Envelope::~Envelope()
{
}

Header Envelope::getHeader() const
{
    Q_ASSERT_X(!isNull(), "Envelope::getHeader", "Getter call on object which isNull");
    return this->d->header;
}
void Envelope::setHeader(Header value)
{
    Q_ASSERT_X(!isNull(), "Envelope::setHeader", "Setter call on object which isNull");
    this->d->header = value;
}
TransportableObject* Envelope::getBody() const
{
    Q_ASSERT_X(!isNull(), "Envelope::getBody", "Getter call on object which isNull");
    return this->d->body;
}
void Envelope::setBody(TransportableObject* value)
{
    Q_ASSERT_X(!isNull(), "Envelope::setBody", "Setter call on object which isNull");
    this->d->body = value;
}

qint32 Envelope::getRepresentedTypeId()
{
    return 1;
}

qint32 Envelope::getTypeId() const
{
    return 1;
}
void Envelope::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->header);
    writer->writeProperty(this, 2, this->d->body);
}
PropertyReadResult Envelope::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->header = Header::empty();
            return PropertyReadResult(&this->d->header);
        case 2:
            this->d->body = NULL;
            switch(typeId)
            {
                case 3:
                    this->d->body = Fault::___new_();
                    break;
                case 10093:
                    this->d->body = GetServerInfo::___new_();
                    break;
                case 10094:
                    this->d->body = GetServerInfoResult::___new_();
                    break;
                case 10001:
                    this->d->body = GetAuthTicket::___new_();
                    break;
                case 10002:
                    this->d->body = GetAuthTicketResult::___new_();
                    break;
                case 10003:
                    this->d->body = CreateAccount::___new_();
                    break;
                case 10004:
                    this->d->body = CreateAccountResult::___new_();
                    break;
                case 10005:
                    this->d->body = UpdateAccount::___new_();
                    break;
                case 10006:
                    this->d->body = UpdateAccountResult::___new_();
                    break;
                case 10009:
                    this->d->body = GetUserPicture::___new_();
                    break;
                case 10010:
                    this->d->body = GetUserPictureResult::___new_();
                    break;
                case 10011:
                    this->d->body = GetContacts::___new_();
                    break;
                case 10012:
                    this->d->body = GetContactsResult::___new_();
                    break;
                case 10015:
                    this->d->body = ProcessAuthorizationRequest::___new_();
                    break;
                case 10016:
                    this->d->body = ProcessAuthorizationRequestResult::___new_();
                    break;
                case 10099:
                    this->d->body = ProcessAuthorizationRequestByLogin::___new_();
                    break;
                case 10100:
                    this->d->body = ProcessAuthorizationRequestByLoginResult::___new_();
                    break;
                case 10017:
                    this->d->body = ProcessAuthorizationResponse::___new_();
                    break;
                case 10018:
                    this->d->body = ProcessAuthorizationResponseResult::___new_();
                    break;
                case 10019:
                    this->d->body = RemoveFromContacts::___new_();
                    break;
                case 10020:
                    this->d->body = RemoveFromContactsResult::___new_();
                    break;
                case 10021:
                    this->d->body = SendFriendMessage::___new_();
                    break;
                case 10022:
                    this->d->body = SendFriendMessageResult::___new_();
                    break;
                case 10023:
                    this->d->body = SendPrivateMessage::___new_();
                    break;
                case 10024:
                    this->d->body = SendPrivateMessageResult::___new_();
                    break;
                case 10113:
                    this->d->body = GetNextArtmessageAndReport::___new_();
                    break;
                case 10114:
                    this->d->body = GetNextArtmessageAndReportResult::___new_();
                    break;
                case 10029:
                    this->d->body = BlockContact::___new_();
                    break;
                case 10030:
                    this->d->body = BlockContactResult::___new_();
                    break;
                case 10031:
                    this->d->body = UnblockContact::___new_();
                    break;
                case 10032:
                    this->d->body = UnblockContactResult::___new_();
                    break;
                case 10033:
                    this->d->body = SimpleSearchContacts::___new_();
                    break;
                case 10034:
                    this->d->body = SimpleSearchContactsResult::___new_();
                    break;
                case 10035:
                    this->d->body = AdvancedSearchContacts::___new_();
                    break;
                case 10036:
                    this->d->body = AdvancedSearchContactsResult::___new_();
                    break;
                case 10037:
                    this->d->body = UpdatePassword::___new_();
                    break;
                case 10038:
                    this->d->body = UpdatePasswordResult::___new_();
                    break;
                case 10039:
                    this->d->body = GetUserInfo::___new_();
                    break;
                case 10040:
                    this->d->body = GetUserInfoResult::___new_();
                    break;
                case 10097:
                    this->d->body = GetUserInfoByLogin::___new_();
                    break;
                case 10098:
                    this->d->body = GetUserInfoByLoginResult::___new_();
                    break;
                case 10045:
                    this->d->body = CreateMoodstrip::___new_();
                    break;
                case 10046:
                    this->d->body = CreateMoodstripResult::___new_();
                    break;
                case 10071:
                    this->d->body = AddPictureToMoodstrip::___new_();
                    break;
                case 10072:
                    this->d->body = AddPictureToMoodstripResult::___new_();
                    break;
                case 10051:
                    this->d->body = DeleteMoodstrip::___new_();
                    break;
                case 10052:
                    this->d->body = DeleteMoodstripResult::___new_();
                    break;
                case 10063:
                    this->d->body = ResetPassword::___new_();
                    break;
                case 10064:
                    this->d->body = ResetPasswordResult::___new_();
                    break;
                case 10073:
                    this->d->body = GetMyAccount::___new_();
                    break;
                case 10074:
                    this->d->body = GetMyAccountResult::___new_();
                    break;
                case 10075:
                    this->d->body = NotificationRegister::___new_();
                    break;
                case 10076:
                    this->d->body = NotificationRegisterResult::___new_();
                    break;
                case 10077:
                    this->d->body = NotificationUnregister::___new_();
                    break;
                case 10078:
                    this->d->body = NotificationUnregisterResult::___new_();
                    break;
                case 10079:
                    this->d->body = GetNotifications::___new_();
                    break;
                case 10080:
                    this->d->body = GetNotificationsResult::___new_();
                    break;
                case 10081:
                    this->d->body = GetContact::___new_();
                    break;
                case 10082:
                    this->d->body = GetContactResult::___new_();
                    break;
                case 10083:
                    this->d->body = GetStatus::___new_();
                    break;
                case 10084:
                    this->d->body = GetStatusResult::___new_();
                    break;
                case 10085:
                    this->d->body = GetAuthorization::___new_();
                    break;
                case 10086:
                    this->d->body = GetAuthorizationResult::___new_();
                    break;
                case 10089:
                    this->d->body = GetNotificationTimeout::___new_();
                    break;
                case 10090:
                    this->d->body = GetNotificationTimeoutResult::___new_();
                    break;
                case 10091:
                    this->d->body = CheckInvitation::___new_();
                    break;
                case 10092:
                    this->d->body = CheckInvitationResult::___new_();
                    break;
                case 10101:
                    this->d->body = SearchChannel::___new_();
                    break;
                case 10102:
                    this->d->body = SearchChannelResult::___new_();
                    break;
                case 10103:
                    this->d->body = GetChannelInfo::___new_();
                    break;
                case 10104:
                    this->d->body = GetChannelInfoResult::___new_();
                    break;
                case 10105:
                    this->d->body = SendChannelMessage::___new_();
                    break;
                case 10106:
                    this->d->body = SendChannelMessageResult::___new_();
                    break;
                case 10107:
                    this->d->body = GetNextChannelMessage::___new_();
                    break;
                case 10108:
                    this->d->body = GetNextChannelMessageResult::___new_();
                    break;
                case 10007:
                    this->d->body = GetNextChannelMessageUrl::___new_();
                    break;
                case 10008:
                    this->d->body = GetNextChannelMessageUrlResult::___new_();
                    break;
                case 10109:
                    this->d->body = AddUserToChannel::___new_();
                    break;
                case 10110:
                    this->d->body = AddUserToChannelResult::___new_();
                    break;
                case 10111:
                    this->d->body = DeleteUserFromChannel::___new_();
                    break;
                case 10112:
                    this->d->body = DeleteUserFromChannelResult::___new_();
                    break;
                case 10025:
                    this->d->body = ObsceneChannelMessage::___new_();
                    break;
                case 10026:
                    this->d->body = ObsceneChannelMessageResult::___new_();
                    break;
                case 10047:
                    this->d->body = DeleteMessage::___new_();
                    break;
                case 10048:
                    this->d->body = DeleteMessageResult::___new_();
                    break;
                case 10049:
                    this->d->body = GetCommands::___new_();
                    break;
                case 10050:
                    this->d->body = GetCommandsResult::___new_();
                    break;
            }
            return PropertyReadResult(this->d->body);
    }

    return PropertyReadResult(false);
}

}
