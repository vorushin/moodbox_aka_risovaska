#ifndef CONTACTRESULTCODE_H
#define CONTACTRESULTCODE_H

namespace MoodBox
{

class ContactResultCode
{

public:
enum ContactResultCodeEnum
{
    Ok = 1,
    ContactNotFound = 2,
    NotAuthorizedMe = 3,
    ClosedContact = 4
};

};

}

#endif // CONTACTRESULTCODE_H