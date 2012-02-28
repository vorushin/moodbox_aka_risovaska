#ifndef AUTHORIZATIONRESULTCODE_H
#define AUTHORIZATIONRESULTCODE_H

namespace MoodBox
{

class AuthorizationResultCode
{

public:
enum AuthorizationResultCodeEnum
{
    Ok = 1,
    IncorrectDbData = 2,
    AuthorHasTooManyContacts = 3,
    RecipientHasTooManyContacts = 4,
    AlreadyAuthorized = 5,
    NotWaitingForAuthorization = 6,
    AuthorizedByThisRequest = 7,
    AccountNotFound = 8
};

};

}

#endif // AUTHORIZATIONRESULTCODE_H