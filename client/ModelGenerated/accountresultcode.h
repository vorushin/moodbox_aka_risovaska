#ifndef ACCOUNTRESULTCODE_H
#define ACCOUNTRESULTCODE_H

namespace MoodBox
{

class AccountResultCode
{

public:
enum AccountResultCodeEnum
{
    Ok = 1,
    InvalidLogin = 2,
    LoginIsNotAvailable = 3,
    InvalidPassword = 4,
    InvalidEmail = 5,
    InvalidInviteCode = 6,
    UserpicSizeTooBig = 7,
    AccountNotFound = 8,
    AgeMustBe13OrGreater = 9
};

};

}

#endif // ACCOUNTRESULTCODE_H