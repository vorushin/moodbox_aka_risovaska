#ifndef MOODBOXSERVER_H
#define MOODBOXSERVER_H


#include "createmoodstrip.h"
#include "addpicturetomoodstrip.h"
#include "deletemoodstrip.h"
#include "notificationregister.h"
#include "notificationunregister.h"
#include "getcommands.h"
#include "getcontacts.h"
#include "getstatus.h"
#include "removefromcontacts.h"
#include "simplesearchcontacts.h"
#include "advancedsearchcontacts.h"
#include "getnotifications.h"
#include "getauthticket.h"
#include "createaccount.h"
#include "getmyaccount.h"
#include "updateaccount.h"
#include "updatepassword.h"
#include "resetpassword.h"
#include "getcontact.h"
#include "processauthorizationrequest.h"
#include "blockcontact.h"
#include "unblockcontact.h"
#include "getnextartmessageandreport.h"
#include "getnotificationtimeout.h"
#include "transportableobject.h"
#include "fault.h"
#include "getuserpicture.h"
#include "processauthorizationresponse.h"
#include "sendfriendmessage.h"
#include "sendprivatemessage.h"
#include "sendchannelmessage.h"
#include "getnextchannelmessage.h"
#include "obscenechannelmessage.h"
#include "deletemessage.h"
#include "searchchannel.h"
#include "getnextchannelmessageurl.h"
#include "addusertochannel.h"
#include "deleteuserfromchannel.h"
#include "getuserinfobylogin.h"
#include "getauthorization.h"
#include "processauthorizationrequestbylogin.h"
#include "checkinvitation.h"
#include "getserverinfo.h"
#include "getuserinfo.h"
#include "getchannelinfo.h"

#include "serverproxybase.h"

namespace MoodBox
{

class MoodBoxServer : public ServerProxyBase
{
public:
    MoodBoxServer(Model* model, TransportChannelBase* channel);

    virtual void resultFaultCall(Callback callback, QVariant state, Fault fault, qint32 resultTypeId);

    void getServerInfo(Callback callback);
    void getServerInfo(Callback callback, QVariant state);
    void getCommands(Callback callback, qint32 previousPackageId);
    void getCommands(Callback callback, QVariant state, qint32 previousPackageId);
    void getAuthTicket(Callback callback, QString login, QString password);
    void getAuthTicket(Callback callback, QVariant state, QString login, QString password);
    void createAccount(Callback callback, UserAccount userAccount, QString inviteCode);
    void createAccount(Callback callback, QVariant state, UserAccount userAccount, QString inviteCode);
    void getUserInfo(Callback callback, qint32 userId);
    void getUserInfo(Callback callback, QVariant state, qint32 userId);
    void getUserInfoByLogin(Callback callback, QString login);
    void getUserInfoByLogin(Callback callback, QVariant state, QString login);
    void getMyAccount(Callback callback);
    void getMyAccount(Callback callback, QVariant state);
    void updateAccount(Callback callback, UserAccount userAccount, bool hasUserPicture, QByteArray userPicture, QString contentType);
    void updateAccount(Callback callback, QVariant state, UserAccount userAccount, bool hasUserPicture, QByteArray userPicture, QString contentType);
    void getUserPicture(Callback callback, qint32 userId, QDateTime lastChangedDate);
    void getUserPicture(Callback callback, QVariant state, qint32 userId, QDateTime lastChangedDate);
    void updatePassword(Callback callback, QString newPassword, QString oldPassword);
    void updatePassword(Callback callback, QVariant state, QString newPassword, QString oldPassword);
    void resetPassword(Callback callback, QString login);
    void resetPassword(Callback callback, QVariant state, QString login);
    void getContacts(Callback callback);
    void getContacts(Callback callback, QVariant state);
    void getContact(Callback callback, qint32 userId);
    void getContact(Callback callback, QVariant state, qint32 userId);
    void getStatus(Callback callback, qint32 userId);
    void getStatus(Callback callback, QVariant state, qint32 userId);
    void getAuthorization(Callback callback, qint32 userId);
    void getAuthorization(Callback callback, QVariant state, qint32 userId);
    void processAuthorizationRequest(Callback callback, qint32 recipientId, QString authorizationMessage);
    void processAuthorizationRequest(Callback callback, QVariant state, qint32 recipientId, QString authorizationMessage);
    void processAuthorizationRequestByLogin(Callback callback, QString recipientLogin, QString authorizationMessage);
    void processAuthorizationRequestByLogin(Callback callback, QVariant state, QString recipientLogin, QString authorizationMessage);
    void processAuthorizationResponse(Callback callback, qint32 recipientId, bool isAccepted);
    void processAuthorizationResponse(Callback callback, QVariant state, qint32 recipientId, bool isAccepted);
    void removeFromContacts(Callback callback, qint32 contactUserId);
    void removeFromContacts(Callback callback, QVariant state, qint32 contactUserId);
    void blockContact(Callback callback, qint32 contactUserId);
    void blockContact(Callback callback, QVariant state, qint32 contactUserId);
    void unblockContact(Callback callback, qint32 contactUserId);
    void unblockContact(Callback callback, QVariant state, qint32 contactUserId);
    void simpleSearchContacts(Callback callback, qint32 pageNumber, qint32 recordsPerPage, QString value);
    void simpleSearchContacts(Callback callback, QVariant state, qint32 pageNumber, qint32 recordsPerPage, QString value);
    void advancedSearchContacts(Callback callback, qint32 pageNumber, qint32 recordsPerPage, QString value, QLocale::Country country, QString city, Sex::SexEnum sex, qint32 minAge, qint32 maxAge);
    void advancedSearchContacts(Callback callback, QVariant state, qint32 pageNumber, qint32 recordsPerPage, QString value, QLocale::Country country, QString city, Sex::SexEnum sex, qint32 minAge, qint32 maxAge);
    void sendFriendMessage(Callback callback, bool isPublic, QByteArray message, QString metadata);
    void sendFriendMessage(Callback callback, QVariant state, bool isPublic, QByteArray message, QString metadata);
    void sendPrivateMessage(Callback callback, qint32 recipientId, QByteArray message, QString metadata);
    void sendPrivateMessage(Callback callback, QVariant state, qint32 recipientId, QByteArray message, QString metadata);
    void getNextArtmessageAndReport(Callback callback, qint32 previousMessageId);
    void getNextArtmessageAndReport(Callback callback, QVariant state, qint32 previousMessageId);
    void createMoodstrip(Callback callback, QString caption, bool isHidden, qint32 channelId);
    void createMoodstrip(Callback callback, QVariant state, QString caption, bool isHidden, qint32 channelId);
    void addPictureToMoodstrip(Callback callback, qint32 moodstripId, qint32 messageId, QString author, QByteArray data, QString contentType, bool isLast);
    void addPictureToMoodstrip(Callback callback, QVariant state, qint32 moodstripId, qint32 messageId, QString author, QByteArray data, QString contentType, bool isLast);
    void deleteMoodstrip(Callback callback, qint32 moodstripId);
    void deleteMoodstrip(Callback callback, QVariant state, qint32 moodstripId);
    void notificationRegister(Callback callback);
    void notificationRegister(Callback callback, QVariant state);
    void notificationUnregister(Callback callback, QString key);
    void notificationUnregister(Callback callback, QVariant state, QString key);
    void getNotifications(Callback callback, QString key, qint64 packetId);
    void getNotifications(Callback callback, QVariant state, QString key, qint64 packetId);
    void getNotificationTimeout(Callback callback);
    void getNotificationTimeout(Callback callback, QVariant state);
    void searchChannel(Callback callback, qint32 pageNumber, qint32 recordsPerPage, QString value);
    void searchChannel(Callback callback, QVariant state, qint32 pageNumber, qint32 recordsPerPage, QString value);
    void getChannelInfo(Callback callback, qint32 channelId);
    void getChannelInfo(Callback callback, QVariant state, qint32 channelId);
    void sendChannelMessage(Callback callback, qint32 channelId, QByteArray message, QString metadata);
    void sendChannelMessage(Callback callback, QVariant state, qint32 channelId, QByteArray message, QString metadata);
    void getNextChannelMessage(Callback callback, qint32 channelId, qint32 lastMessageId, bool skipMessage);
    void getNextChannelMessage(Callback callback, QVariant state, qint32 channelId, qint32 lastMessageId, bool skipMessage);
    void getNextChannelMessageUrl(Callback callback, qint32 channelId, qint32 lastMessageId, bool skipMessage);
    void getNextChannelMessageUrl(Callback callback, QVariant state, qint32 channelId, qint32 lastMessageId, bool skipMessage);
    void addUserToChannel(Callback callback, qint32 channelId);
    void addUserToChannel(Callback callback, QVariant state, qint32 channelId);
    void deleteUserFromChannel(Callback callback, qint32 channelId);
    void deleteUserFromChannel(Callback callback, QVariant state, qint32 channelId);
    void obsceneChannelMessage(Callback callback, qint32 channelId, qint32 messageId);
    void obsceneChannelMessage(Callback callback, QVariant state, qint32 channelId, qint32 messageId);
    void deleteMessage(Callback callback, qint32 contactId, MessageType::MessageTypeEnum messageType, qint32 messageId);
    void deleteMessage(Callback callback, QVariant state, qint32 contactId, MessageType::MessageTypeEnum messageType, qint32 messageId);
    void checkInvitation(Callback callback, QString code);
    void checkInvitation(Callback callback, QVariant state, QString code);
};

}

#endif // MOODBOXSERVER_H