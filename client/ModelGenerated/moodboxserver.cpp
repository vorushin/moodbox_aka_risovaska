#include "moodboxserver.h"


#include "getserverinforesult.h"
#include "getcommandsresult.h"
#include "getauthticketresult.h"
#include "createaccountresult.h"
#include "getuserinforesult.h"
#include "getuserinfobyloginresult.h"
#include "getmyaccountresult.h"
#include "updateaccountresult.h"
#include "getuserpictureresult.h"
#include "updatepasswordresult.h"
#include "resetpasswordresult.h"
#include "getcontactsresult.h"
#include "getcontactresult.h"
#include "getstatusresult.h"
#include "getauthorizationresult.h"
#include "processauthorizationrequestresult.h"
#include "processauthorizationrequestbyloginresult.h"
#include "processauthorizationresponseresult.h"
#include "removefromcontactsresult.h"
#include "blockcontactresult.h"
#include "unblockcontactresult.h"
#include "simplesearchcontactsresult.h"
#include "advancedsearchcontactsresult.h"
#include "sendfriendmessageresult.h"
#include "sendprivatemessageresult.h"
#include "getnextartmessageandreportresult.h"
#include "createmoodstripresult.h"
#include "addpicturetomoodstripresult.h"
#include "deletemoodstripresult.h"
#include "notificationregisterresult.h"
#include "notificationunregisterresult.h"
#include "getnotificationsresult.h"
#include "getnotificationtimeoutresult.h"
#include "searchchannelresult.h"
#include "getchannelinforesult.h"
#include "sendchannelmessageresult.h"
#include "getnextchannelmessageresult.h"
#include "getnextchannelmessageurlresult.h"
#include "addusertochannelresult.h"
#include "deleteuserfromchannelresult.h"
#include "obscenechannelmessageresult.h"
#include "deletemessageresult.h"
#include "checkinvitationresult.h"

namespace MoodBox
{

MoodBoxServer::MoodBoxServer(Model* model, TransportChannelBase* channel) : ServerProxyBase(model, channel)
{
}

void MoodBoxServer::resultFaultCall(Callback callback, QVariant state, Fault fault, qint32 resultTypeId)
{
    switch(resultTypeId)
    {
        case 10094:
            GetServerInfoResultCallbackCaller::call(callback, state, fault);
            break;
        case 10050:
            GetCommandsResultCallbackCaller::call(callback, state, fault);
            break;
        case 10002:
            GetAuthTicketResultCallbackCaller::call(callback, state, fault);
            break;
        case 10004:
            CreateAccountResultCallbackCaller::call(callback, state, fault);
            break;
        case 10040:
            GetUserInfoResultCallbackCaller::call(callback, state, fault);
            break;
        case 10098:
            GetUserInfoByLoginResultCallbackCaller::call(callback, state, fault);
            break;
        case 10074:
            GetMyAccountResultCallbackCaller::call(callback, state, fault);
            break;
        case 10006:
            UpdateAccountResultCallbackCaller::call(callback, state, fault);
            break;
        case 10010:
            GetUserPictureResultCallbackCaller::call(callback, state, fault);
            break;
        case 10038:
            UpdatePasswordResultCallbackCaller::call(callback, state, fault);
            break;
        case 10064:
            ResetPasswordResultCallbackCaller::call(callback, state, fault);
            break;
        case 10012:
            GetContactsResultCallbackCaller::call(callback, state, fault);
            break;
        case 10082:
            GetContactResultCallbackCaller::call(callback, state, fault);
            break;
        case 10084:
            GetStatusResultCallbackCaller::call(callback, state, fault);
            break;
        case 10086:
            GetAuthorizationResultCallbackCaller::call(callback, state, fault);
            break;
        case 10016:
            ProcessAuthorizationRequestResultCallbackCaller::call(callback, state, fault);
            break;
        case 10100:
            ProcessAuthorizationRequestByLoginResultCallbackCaller::call(callback, state, fault);
            break;
        case 10018:
            ProcessAuthorizationResponseResultCallbackCaller::call(callback, state, fault);
            break;
        case 10020:
            RemoveFromContactsResultCallbackCaller::call(callback, state, fault);
            break;
        case 10030:
            BlockContactResultCallbackCaller::call(callback, state, fault);
            break;
        case 10032:
            UnblockContactResultCallbackCaller::call(callback, state, fault);
            break;
        case 10034:
            SimpleSearchContactsResultCallbackCaller::call(callback, state, fault);
            break;
        case 10036:
            AdvancedSearchContactsResultCallbackCaller::call(callback, state, fault);
            break;
        case 10022:
            SendFriendMessageResultCallbackCaller::call(callback, state, fault);
            break;
        case 10024:
            SendPrivateMessageResultCallbackCaller::call(callback, state, fault);
            break;
        case 10114:
            GetNextArtmessageAndReportResultCallbackCaller::call(callback, state, fault);
            break;
        case 10046:
            CreateMoodstripResultCallbackCaller::call(callback, state, fault);
            break;
        case 10072:
            AddPictureToMoodstripResultCallbackCaller::call(callback, state, fault);
            break;
        case 10052:
            DeleteMoodstripResultCallbackCaller::call(callback, state, fault);
            break;
        case 10076:
            NotificationRegisterResultCallbackCaller::call(callback, state, fault);
            break;
        case 10078:
            NotificationUnregisterResultCallbackCaller::call(callback, state, fault);
            break;
        case 10080:
            GetNotificationsResultCallbackCaller::call(callback, state, fault);
            break;
        case 10090:
            GetNotificationTimeoutResultCallbackCaller::call(callback, state, fault);
            break;
        case 10102:
            SearchChannelResultCallbackCaller::call(callback, state, fault);
            break;
        case 10104:
            GetChannelInfoResultCallbackCaller::call(callback, state, fault);
            break;
        case 10106:
            SendChannelMessageResultCallbackCaller::call(callback, state, fault);
            break;
        case 10108:
            GetNextChannelMessageResultCallbackCaller::call(callback, state, fault);
            break;
        case 10008:
            GetNextChannelMessageUrlResultCallbackCaller::call(callback, state, fault);
            break;
        case 10110:
            AddUserToChannelResultCallbackCaller::call(callback, state, fault);
            break;
        case 10112:
            DeleteUserFromChannelResultCallbackCaller::call(callback, state, fault);
            break;
        case 10026:
            ObsceneChannelMessageResultCallbackCaller::call(callback, state, fault);
            break;
        case 10048:
            DeleteMessageResultCallbackCaller::call(callback, state, fault);
            break;
        case 10092:
            CheckInvitationResultCallbackCaller::call(callback, state, fault);
            break;
        default:
            throw "Unknown resultTypeId";
    }
}

void MoodBoxServer::getServerInfo(Callback callback)
{
    getServerInfo(callback, QVariant());
}

void MoodBoxServer::getServerInfo(Callback callback, QVariant state)
{
    send(callback, state, GetServerInfo::___new_());
}

void MoodBoxServer::getCommands(Callback callback, qint32 previousPackageId)
{
    getCommands(callback, QVariant(), previousPackageId);
}

void MoodBoxServer::getCommands(Callback callback, QVariant state, qint32 previousPackageId)
{
    send(callback, state, new GetCommands(previousPackageId));
}

void MoodBoxServer::getAuthTicket(Callback callback, QString login, QString password)
{
    getAuthTicket(callback, QVariant(), login, password);
}

void MoodBoxServer::getAuthTicket(Callback callback, QVariant state, QString login, QString password)
{
    send(callback, state, new GetAuthTicket(login, password));
}

void MoodBoxServer::createAccount(Callback callback, UserAccount userAccount, QString inviteCode)
{
    createAccount(callback, QVariant(), userAccount, inviteCode);
}

void MoodBoxServer::createAccount(Callback callback, QVariant state, UserAccount userAccount, QString inviteCode)
{
    send(callback, state, new CreateAccount(userAccount, inviteCode));
}

void MoodBoxServer::getUserInfo(Callback callback, qint32 userId)
{
    getUserInfo(callback, QVariant(), userId);
}

void MoodBoxServer::getUserInfo(Callback callback, QVariant state, qint32 userId)
{
    send(callback, state, new GetUserInfo(userId));
}

void MoodBoxServer::getUserInfoByLogin(Callback callback, QString login)
{
    getUserInfoByLogin(callback, QVariant(), login);
}

void MoodBoxServer::getUserInfoByLogin(Callback callback, QVariant state, QString login)
{
    send(callback, state, new GetUserInfoByLogin(login));
}

void MoodBoxServer::getMyAccount(Callback callback)
{
    getMyAccount(callback, QVariant());
}

void MoodBoxServer::getMyAccount(Callback callback, QVariant state)
{
    send(callback, state, GetMyAccount::___new_());
}

void MoodBoxServer::updateAccount(Callback callback, UserAccount userAccount, bool hasUserPicture, QByteArray userPicture, QString contentType)
{
    updateAccount(callback, QVariant(), userAccount, hasUserPicture, userPicture, contentType);
}

void MoodBoxServer::updateAccount(Callback callback, QVariant state, UserAccount userAccount, bool hasUserPicture, QByteArray userPicture, QString contentType)
{
    send(callback, state, new UpdateAccount(userAccount, hasUserPicture, userPicture, contentType));
}

void MoodBoxServer::getUserPicture(Callback callback, qint32 userId, QDateTime lastChangedDate)
{
    getUserPicture(callback, QVariant(), userId, lastChangedDate);
}

void MoodBoxServer::getUserPicture(Callback callback, QVariant state, qint32 userId, QDateTime lastChangedDate)
{
    send(callback, state, new GetUserPicture(userId, lastChangedDate));
}

void MoodBoxServer::updatePassword(Callback callback, QString newPassword, QString oldPassword)
{
    updatePassword(callback, QVariant(), newPassword, oldPassword);
}

void MoodBoxServer::updatePassword(Callback callback, QVariant state, QString newPassword, QString oldPassword)
{
    send(callback, state, new UpdatePassword(newPassword, oldPassword));
}

void MoodBoxServer::resetPassword(Callback callback, QString login)
{
    resetPassword(callback, QVariant(), login);
}

void MoodBoxServer::resetPassword(Callback callback, QVariant state, QString login)
{
    send(callback, state, new ResetPassword(login));
}

void MoodBoxServer::getContacts(Callback callback)
{
    getContacts(callback, QVariant());
}

void MoodBoxServer::getContacts(Callback callback, QVariant state)
{
    send(callback, state, GetContacts::___new_());
}

void MoodBoxServer::getContact(Callback callback, qint32 userId)
{
    getContact(callback, QVariant(), userId);
}

void MoodBoxServer::getContact(Callback callback, QVariant state, qint32 userId)
{
    send(callback, state, new GetContact(userId));
}

void MoodBoxServer::getStatus(Callback callback, qint32 userId)
{
    getStatus(callback, QVariant(), userId);
}

void MoodBoxServer::getStatus(Callback callback, QVariant state, qint32 userId)
{
    send(callback, state, new GetStatus(userId));
}

void MoodBoxServer::getAuthorization(Callback callback, qint32 userId)
{
    getAuthorization(callback, QVariant(), userId);
}

void MoodBoxServer::getAuthorization(Callback callback, QVariant state, qint32 userId)
{
    send(callback, state, new GetAuthorization(userId));
}

void MoodBoxServer::processAuthorizationRequest(Callback callback, qint32 recipientId, QString authorizationMessage)
{
    processAuthorizationRequest(callback, QVariant(), recipientId, authorizationMessage);
}

void MoodBoxServer::processAuthorizationRequest(Callback callback, QVariant state, qint32 recipientId, QString authorizationMessage)
{
    send(callback, state, new ProcessAuthorizationRequest(recipientId, authorizationMessage));
}

void MoodBoxServer::processAuthorizationRequestByLogin(Callback callback, QString recipientLogin, QString authorizationMessage)
{
    processAuthorizationRequestByLogin(callback, QVariant(), recipientLogin, authorizationMessage);
}

void MoodBoxServer::processAuthorizationRequestByLogin(Callback callback, QVariant state, QString recipientLogin, QString authorizationMessage)
{
    send(callback, state, new ProcessAuthorizationRequestByLogin(recipientLogin, authorizationMessage));
}

void MoodBoxServer::processAuthorizationResponse(Callback callback, qint32 recipientId, bool isAccepted)
{
    processAuthorizationResponse(callback, QVariant(), recipientId, isAccepted);
}

void MoodBoxServer::processAuthorizationResponse(Callback callback, QVariant state, qint32 recipientId, bool isAccepted)
{
    send(callback, state, new ProcessAuthorizationResponse(recipientId, isAccepted));
}

void MoodBoxServer::removeFromContacts(Callback callback, qint32 contactUserId)
{
    removeFromContacts(callback, QVariant(), contactUserId);
}

void MoodBoxServer::removeFromContacts(Callback callback, QVariant state, qint32 contactUserId)
{
    send(callback, state, new RemoveFromContacts(contactUserId));
}

void MoodBoxServer::blockContact(Callback callback, qint32 contactUserId)
{
    blockContact(callback, QVariant(), contactUserId);
}

void MoodBoxServer::blockContact(Callback callback, QVariant state, qint32 contactUserId)
{
    send(callback, state, new BlockContact(contactUserId));
}

void MoodBoxServer::unblockContact(Callback callback, qint32 contactUserId)
{
    unblockContact(callback, QVariant(), contactUserId);
}

void MoodBoxServer::unblockContact(Callback callback, QVariant state, qint32 contactUserId)
{
    send(callback, state, new UnblockContact(contactUserId));
}

void MoodBoxServer::simpleSearchContacts(Callback callback, qint32 pageNumber, qint32 recordsPerPage, QString value)
{
    simpleSearchContacts(callback, QVariant(), pageNumber, recordsPerPage, value);
}

void MoodBoxServer::simpleSearchContacts(Callback callback, QVariant state, qint32 pageNumber, qint32 recordsPerPage, QString value)
{
    send(callback, state, new SimpleSearchContacts(pageNumber, recordsPerPage, value));
}

void MoodBoxServer::advancedSearchContacts(Callback callback, qint32 pageNumber, qint32 recordsPerPage, QString value, QLocale::Country country, QString city, Sex::SexEnum sex, qint32 minAge, qint32 maxAge)
{
    advancedSearchContacts(callback, QVariant(), pageNumber, recordsPerPage, value, country, city, sex, minAge, maxAge);
}

void MoodBoxServer::advancedSearchContacts(Callback callback, QVariant state, qint32 pageNumber, qint32 recordsPerPage, QString value, QLocale::Country country, QString city, Sex::SexEnum sex, qint32 minAge, qint32 maxAge)
{
    send(callback, state, new AdvancedSearchContacts(pageNumber, recordsPerPage, value, country, city, sex, minAge, maxAge));
}

void MoodBoxServer::sendFriendMessage(Callback callback, bool isPublic, QByteArray message, QString metadata)
{
    sendFriendMessage(callback, QVariant(), isPublic, message, metadata);
}

void MoodBoxServer::sendFriendMessage(Callback callback, QVariant state, bool isPublic, QByteArray message, QString metadata)
{
    send(callback, state, new SendFriendMessage(isPublic, message, metadata));
}

void MoodBoxServer::sendPrivateMessage(Callback callback, qint32 recipientId, QByteArray message, QString metadata)
{
    sendPrivateMessage(callback, QVariant(), recipientId, message, metadata);
}

void MoodBoxServer::sendPrivateMessage(Callback callback, QVariant state, qint32 recipientId, QByteArray message, QString metadata)
{
    send(callback, state, new SendPrivateMessage(recipientId, message, metadata));
}

void MoodBoxServer::getNextArtmessageAndReport(Callback callback, qint32 previousMessageId)
{
    getNextArtmessageAndReport(callback, QVariant(), previousMessageId);
}

void MoodBoxServer::getNextArtmessageAndReport(Callback callback, QVariant state, qint32 previousMessageId)
{
    send(callback, state, new GetNextArtmessageAndReport(previousMessageId));
}

void MoodBoxServer::createMoodstrip(Callback callback, QString caption, bool isHidden, qint32 channelId)
{
    createMoodstrip(callback, QVariant(), caption, isHidden, channelId);
}

void MoodBoxServer::createMoodstrip(Callback callback, QVariant state, QString caption, bool isHidden, qint32 channelId)
{
    send(callback, state, new CreateMoodstrip(caption, isHidden, channelId));
}

void MoodBoxServer::addPictureToMoodstrip(Callback callback, qint32 moodstripId, qint32 messageId, QString author, QByteArray data, QString contentType, bool isLast)
{
    addPictureToMoodstrip(callback, QVariant(), moodstripId, messageId, author, data, contentType, isLast);
}

void MoodBoxServer::addPictureToMoodstrip(Callback callback, QVariant state, qint32 moodstripId, qint32 messageId, QString author, QByteArray data, QString contentType, bool isLast)
{
    send(callback, state, new AddPictureToMoodstrip(moodstripId, messageId, author, data, contentType, isLast));
}

void MoodBoxServer::deleteMoodstrip(Callback callback, qint32 moodstripId)
{
    deleteMoodstrip(callback, QVariant(), moodstripId);
}

void MoodBoxServer::deleteMoodstrip(Callback callback, QVariant state, qint32 moodstripId)
{
    send(callback, state, new DeleteMoodstrip(moodstripId));
}

void MoodBoxServer::notificationRegister(Callback callback)
{
    notificationRegister(callback, QVariant());
}

void MoodBoxServer::notificationRegister(Callback callback, QVariant state)
{
    send(callback, state, NotificationRegister::___new_());
}

void MoodBoxServer::notificationUnregister(Callback callback, QString key)
{
    notificationUnregister(callback, QVariant(), key);
}

void MoodBoxServer::notificationUnregister(Callback callback, QVariant state, QString key)
{
    send(callback, state, new NotificationUnregister(key));
}

void MoodBoxServer::getNotifications(Callback callback, QString key, qint64 packetId)
{
    getNotifications(callback, QVariant(), key, packetId);
}

void MoodBoxServer::getNotifications(Callback callback, QVariant state, QString key, qint64 packetId)
{
    send(callback, state, new GetNotifications(key, packetId));
}

void MoodBoxServer::getNotificationTimeout(Callback callback)
{
    getNotificationTimeout(callback, QVariant());
}

void MoodBoxServer::getNotificationTimeout(Callback callback, QVariant state)
{
    send(callback, state, GetNotificationTimeout::___new_());
}

void MoodBoxServer::searchChannel(Callback callback, qint32 pageNumber, qint32 recordsPerPage, QString value)
{
    searchChannel(callback, QVariant(), pageNumber, recordsPerPage, value);
}

void MoodBoxServer::searchChannel(Callback callback, QVariant state, qint32 pageNumber, qint32 recordsPerPage, QString value)
{
    send(callback, state, new SearchChannel(pageNumber, recordsPerPage, value));
}

void MoodBoxServer::getChannelInfo(Callback callback, qint32 channelId)
{
    getChannelInfo(callback, QVariant(), channelId);
}

void MoodBoxServer::getChannelInfo(Callback callback, QVariant state, qint32 channelId)
{
    send(callback, state, new GetChannelInfo(channelId));
}

void MoodBoxServer::sendChannelMessage(Callback callback, qint32 channelId, QByteArray message, QString metadata)
{
    sendChannelMessage(callback, QVariant(), channelId, message, metadata);
}

void MoodBoxServer::sendChannelMessage(Callback callback, QVariant state, qint32 channelId, QByteArray message, QString metadata)
{
    send(callback, state, new SendChannelMessage(channelId, message, metadata));
}

void MoodBoxServer::getNextChannelMessage(Callback callback, qint32 channelId, qint32 lastMessageId, bool skipMessage)
{
    getNextChannelMessage(callback, QVariant(), channelId, lastMessageId, skipMessage);
}

void MoodBoxServer::getNextChannelMessage(Callback callback, QVariant state, qint32 channelId, qint32 lastMessageId, bool skipMessage)
{
    send(callback, state, new GetNextChannelMessage(channelId, lastMessageId, skipMessage));
}

void MoodBoxServer::getNextChannelMessageUrl(Callback callback, qint32 channelId, qint32 lastMessageId, bool skipMessage)
{
    getNextChannelMessageUrl(callback, QVariant(), channelId, lastMessageId, skipMessage);
}

void MoodBoxServer::getNextChannelMessageUrl(Callback callback, QVariant state, qint32 channelId, qint32 lastMessageId, bool skipMessage)
{
    send(callback, state, new GetNextChannelMessageUrl(channelId, lastMessageId, skipMessage));
}

void MoodBoxServer::addUserToChannel(Callback callback, qint32 channelId)
{
    addUserToChannel(callback, QVariant(), channelId);
}

void MoodBoxServer::addUserToChannel(Callback callback, QVariant state, qint32 channelId)
{
    send(callback, state, new AddUserToChannel(channelId));
}

void MoodBoxServer::deleteUserFromChannel(Callback callback, qint32 channelId)
{
    deleteUserFromChannel(callback, QVariant(), channelId);
}

void MoodBoxServer::deleteUserFromChannel(Callback callback, QVariant state, qint32 channelId)
{
    send(callback, state, new DeleteUserFromChannel(channelId));
}

void MoodBoxServer::obsceneChannelMessage(Callback callback, qint32 channelId, qint32 messageId)
{
    obsceneChannelMessage(callback, QVariant(), channelId, messageId);
}

void MoodBoxServer::obsceneChannelMessage(Callback callback, QVariant state, qint32 channelId, qint32 messageId)
{
    send(callback, state, new ObsceneChannelMessage(channelId, messageId));
}

void MoodBoxServer::deleteMessage(Callback callback, qint32 contactId, MessageType::MessageTypeEnum messageType, qint32 messageId)
{
    deleteMessage(callback, QVariant(), contactId, messageType, messageId);
}

void MoodBoxServer::deleteMessage(Callback callback, QVariant state, qint32 contactId, MessageType::MessageTypeEnum messageType, qint32 messageId)
{
    send(callback, state, new DeleteMessage(contactId, messageType, messageId));
}

void MoodBoxServer::checkInvitation(Callback callback, QString code)
{
    checkInvitation(callback, QVariant(), code);
}

void MoodBoxServer::checkInvitation(Callback callback, QVariant state, QString code)
{
    send(callback, state, new CheckInvitation(code));
}


}
