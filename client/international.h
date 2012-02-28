#ifndef INTERNATIONAL_H
#define INTERNATIONAL_H

#include <QString>

#include "language.h"

#include "userstatus.h"
#include "accountresultcode.h"
#include "authorizationresultcode.h"
#include "contactresultcode.h"
#include "contactresultcode.h"
#include "changeuserchannelresult.h"
#include "userpictureresultcode.h"

namespace MoodBox
{

#ifndef RUSSIAN_VERSION
  #define PUBLISHER_DOMAIN_NAME				"moodbox.com"
#else
  #define PUBLISHER_DOMAIN_NAME				"risovaska.ru"
#endif

#define NO_COUNTRY_TEXT					QT_TRANSLATE_NOOP("@default", "NoCountry")

#define INSTALL_NEW_VERSION_BUTTON		QT_TRANSLATE_NOOP("@default", "InstallNewVersionButton")

// Standart buttons
#define CANCEL_TEXT						QT_TRANSLATE_NOOP("@default", "CancelButton")
#define CLOSE_TEXT						QT_TRANSLATE_NOOP("@default", "CloseButton")
#define OK_TEXT							QT_TRANSLATE_NOOP("@default", "OkButton")

// Links
#define NEXT_TEXT						QT_TRANSLATE_NOOP("@default", "NextButton")
#define BACK_TEXT						QT_TRANSLATE_NOOP("@default", "BackButton")
#define FINISH_TEXT						QT_TRANSLATE_NOOP("@default", "FinishButton")

// Statuses
#define STATUS_OFFLINE_NAME				QT_TRANSLATE_NOOP("MoodBox::PeopleInfoManager", "OfflineStatus")
#define STATUS_ONLINE_NAME				QT_TRANSLATE_NOOP("MoodBox::PeopleInfoManager", "OnlineStatus")
#define STATUS_CONNECTING_NAME			QT_TRANSLATE_NOOP("MoodBox::PeopleInfoManager", "ConnectingStatus")
#define STATUS_UNDEFINED_NAME			QT_TRANSLATE_NOOP("MoodBox::PeopleInfoManager", "UnknownStatus")	

// Gender
#define SEX_FEMALE						QT_TRANSLATE_NOOP("@default", "SexFemale")
#define SEX_MALE						QT_TRANSLATE_NOOP("@default", "SexMale")
#define SEX_UNDEFINED					QT_TRANSLATE_NOOP("@default", "SexUndefined")

// Actions
#define UNDO_ACTION_TEXT				QT_TRANSLATE_NOOP("MoodBox::DrawingWindow", "Undo")
#define REDO_ACTION_TEXT				QT_TRANSLATE_NOOP("MoodBox::DrawingWindow", "Redo")
#define COPY_ACTION_TEXT				QT_TRANSLATE_NOOP("MoodBox::DrawingWindow", "Copy")
#define PASTE_ACTION_TEXT				QT_TRANSLATE_NOOP("MoodBox::DrawingWindow", "Paste")
#define SAVE_ACTION_TEXT				QT_TRANSLATE_NOOP("MoodBox::DrawingWindow", "Save")

#define SAVE_TITLE						QT_TRANSLATE_NOOP("@default", "Save")
#define SAVE_FILTER						QT_TRANSLATE_NOOP("@default", "SaveFilter%1")

// Errors
#define MUST_ACCEPT_TOS_TITLE			QT_TRANSLATE_NOOP("MoodBox::RegistrationWidget", "YouMustAcceptTos")
#define MUST_ACCEPT_TOS_ERROR			QT_TRANSLATE_NOOP("MoodBox::RegistrationWidget", "YouMustAcceptTosAndPP")

#define LOGIN_IS_NOT_AVAILABLE_ERROR	QT_TRANSLATE_NOOP("MoodBox::PromptHelper", "LoginIsNotAvailableError")
#define INVALID_INVITE_CODE_ERROR		QT_TRANSLATE_NOOP("MoodBox::PromptHelper", "InvalidInviteCodeError")
#define AGE_MUST_BE_13_OR_GREATER_ERROR	QT_TRANSLATE_NOOP("MoodBox::PromptHelper", "AgeMustBe13OrGreaterError")

#define ACCOUNT_GET_ERROR				QT_TRANSLATE_NOOP("@default", "AccountGetError%1")
#define ACCOUNT_UPDATE_ERROR_TITLE		QT_TRANSLATE_NOOP("@default", "AccountUpdateErrorTitle")
#define CONTACT_NOT_FOUND_ERROR			QT_TRANSLATE_NOOP("@default", "ContactNotFoundError%1")
#define UNKNOWN_SERVER_ERROR			QT_TRANSLATE_NOOP("@default", "UnknownServerError%1")
#define INFO_GET_ERROR_TITLE			QT_TRANSLATE_NOOP("@default", "InfoGetErrorTitle%1")
#define AUTH_REQUEST_SEND_ERROR_TITLE	QT_TRANSLATE_NOOP("@default", "AuthRequestSendErrorTitle%1")
#define AUTH_RESPONSE_SEND_ERROR_TITLE	QT_TRANSLATE_NOOP("@default", "AuthResponseSendErrorTitle%1")

#define AUTH_REQUEST_ALREADY_ERROR		QT_TRANSLATE_NOOP("@default", "AlreadyAuthorizedError")
#define AUTH_REQUEST_U2MANY_ERROR		QT_TRANSLATE_NOOP("@default", "YouHaveTooManyContactsError")
#define AUTH_REQUEST_NOTWAIT_ERROR		QT_TRANSLATE_NOOP("@default", "NotWaitingAuthorizationError")
#define AUTH_REQUEST_R2MANY_ERROR		QT_TRANSLATE_NOOP("@default", "RecipientHasTooManyContactsError")
#define AUTH_REQUEST_NOACCOUNT_ERROR	QT_TRANSLATE_NOOP("@default", "AccountNotFoundError")
#define AUTH_REQUEST_SELF_ERROR			QT_TRANSLATE_NOOP("@default", "YouCannotAuthorizeYourselfError")

#define USERPIC_SET_ERROR				QT_TRANSLATE_NOOP("@default", "UserpicSetError%1")
#define USERPIC_TOO_BIG_ERROR			QT_TRANSLATE_NOOP("@default", "UserpicTooBigError")

#define MESSAGE_TOO_BIG_TITLE			QT_TRANSLATE_NOOP("MoodBox::DrawingWindow", "MessageTooBigTitle")
#define MESSAGE_TOO_BIG_TEXT			QT_TRANSLATE_NOOP("MoodBox::DrawingWindow", "MessageTooBigText")

class PromptHelper
{
public:
	// Status name by type
	static QString getStatusName(UserStatus::UserStatusEnum status);

	// Account update error by type
	static QString getAccountUpdateErrorName(AccountResultCode::AccountResultCodeEnum error);

	// Authorization error by type
	static QString getAuthorizationErrorName(AuthorizationResultCode::AuthorizationResultCodeEnum error);

	// Remove error by type
	static QString getRemoveFriendErrorName(ContactResultCode::ContactResultCodeEnum error);

	// Change channel error by type
	static QString getChangeUserChannelErrorName(ChangeUserChannelResult::ChangeUserChannelResultEnum error);
	
	// Picture error name by type
	static QString getPictureErrorName(UserPictureResultCode::UserPictureResultCodeEnum error);

};

}

#endif // INTERNATIONAL_H
