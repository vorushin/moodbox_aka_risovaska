#ifndef USERSTATUS_H
#define USERSTATUS_H

namespace MoodBox
{

class UserStatus
{

public:
enum UserStatusEnum
{
    Undefined = 0,
    Offline = 1,
    Online = 2,
    Connecting = 100
};

};

}

#endif // USERSTATUS_H