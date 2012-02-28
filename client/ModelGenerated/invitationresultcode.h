#ifndef INVITATIONRESULTCODE_H
#define INVITATIONRESULTCODE_H

namespace MoodBox
{

class InvitationResultCode
{

public:
enum InvitationResultCodeEnum
{
    Ok = 1,
    InvalidInviteCode = 2,
    LimitWasExceeded = 3,
    InvalidEmail = 4
};

};

}

#endif // INVITATIONRESULTCODE_H