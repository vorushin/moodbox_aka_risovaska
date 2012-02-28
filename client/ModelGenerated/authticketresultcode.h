#ifndef AUTHTICKETRESULTCODE_H
#define AUTHTICKETRESULTCODE_H

namespace MoodBox
{

class AuthTicketResultCode
{

public:
enum AuthTicketResultCodeEnum
{
    Ok = 1,
    InvalidCredentials = 2,
    LockedTooManyAttempts = 3
};

};

}

#endif // AUTHTICKETRESULTCODE_H