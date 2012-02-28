#ifndef USERPICTURERESULTCODE_H
#define USERPICTURERESULTCODE_H

namespace MoodBox
{

class UserPictureResultCode
{

public:
enum UserPictureResultCodeEnum
{
    Ok = 1,
    UserpicSizeTooBig = 2,
    NotModified = 3,
    NotFound = 4
};

};

}

#endif // USERPICTURERESULTCODE_H