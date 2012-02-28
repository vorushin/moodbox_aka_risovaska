#ifndef AUTHORIZATIONSTATE_H
#define AUTHORIZATIONSTATE_H

namespace MoodBox
{

class AuthorizationState
{

public:
enum AuthorizationStateEnum
{
    Undefined = 0,
    Authorized = 1,
    WaitsAuthorizationFromMe = 2,
    NotAuthorizedMe = 3
};

};

}

#endif // AUTHORIZATIONSTATE_H