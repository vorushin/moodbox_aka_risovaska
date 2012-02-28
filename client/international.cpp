#include "international.h"
#include "verifiers.h"

#include <QObject>

namespace MoodBox
{

QString PromptHelper::getStatusName(UserStatus::UserStatusEnum status)
{
	QString statusString;

	switch (status)
	{
		case UserStatus::Offline: statusString = QObject::tr(STATUS_OFFLINE_NAME); break;
		case UserStatus::Online: statusString = QObject::tr(STATUS_ONLINE_NAME); break;
		case UserStatus::Connecting: statusString = QObject::tr(STATUS_CONNECTING_NAME); break;
		default: statusString = QObject::tr(STATUS_OFFLINE_NAME); break;
	}

	return statusString;
}

QString PromptHelper::getAccountUpdateErrorName(AccountResultCode::AccountResultCodeEnum error)
{
	QString errorString;

	switch (error)
	{
		case AccountResultCode::InvalidEmail: errorString = QObject::tr(CHECK_EMAIL_ERROR_MESSAGE); break;
		case AccountResultCode::InvalidPassword: errorString = QObject::tr(CHECK_PASSWORD_ERROR_MESSAGE); break;
		case AccountResultCode::InvalidLogin: errorString = QObject::tr(CHECK_LOGIN_ERROR_MESSAGE); break;
		case AccountResultCode::LoginIsNotAvailable: errorString = QObject::tr(LOGIN_IS_NOT_AVAILABLE_ERROR); break;
		case AccountResultCode::InvalidInviteCode: errorString = QObject::tr(INVALID_INVITE_CODE_ERROR); break;
		case AccountResultCode::AgeMustBe13OrGreater: errorString = QObject::tr(AGE_MUST_BE_13_OR_GREATER_ERROR); break;
	}

	return errorString;
}

QString PromptHelper::getAuthorizationErrorName(AuthorizationResultCode::AuthorizationResultCodeEnum error)
{
	QString errorString;

	switch (error)
	{
		case AuthorizationResultCode::AlreadyAuthorized: errorString = QObject::tr(AUTH_REQUEST_ALREADY_ERROR); break;
		case AuthorizationResultCode::AuthorHasTooManyContacts: errorString = QObject::tr(AUTH_REQUEST_U2MANY_ERROR); break;
		case AuthorizationResultCode::NotWaitingForAuthorization: errorString = QObject::tr(AUTH_REQUEST_NOTWAIT_ERROR); break;
		case AuthorizationResultCode::RecipientHasTooManyContacts: errorString = QObject::tr(AUTH_REQUEST_R2MANY_ERROR); break;
		case AuthorizationResultCode::AccountNotFound: errorString = QObject::tr(AUTH_REQUEST_NOACCOUNT_ERROR); break;
	}

	return errorString;
}

QString PromptHelper::getRemoveFriendErrorName(ContactResultCode::ContactResultCodeEnum error)
{
	QString errorString;

	switch (error)
	{
		case ContactResultCode::ContactNotFound: errorString = QObject::tr(CONTACT_NOT_FOUND_ERROR); break;
	}

	return errorString;
}

QString PromptHelper::getChangeUserChannelErrorName(ChangeUserChannelResult::ChangeUserChannelResultEnum error)
{
	QString errorString;

	switch (error)
	{
		case ChangeUserChannelResult::Undefined: errorString = QObject::tr(UNKNOWN_SERVER_ERROR); break;
	}

	return errorString;
}

QString PromptHelper::getPictureErrorName(UserPictureResultCode::UserPictureResultCodeEnum error)
{
	QString errorString;

	switch (error)
	{
		case UserPictureResultCode::UserpicSizeTooBig: errorString = QObject::tr(USERPIC_TOO_BIG_ERROR);
			break;
	}

	return errorString;
}

}