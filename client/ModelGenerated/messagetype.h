#ifndef MESSAGETYPE_H
#define MESSAGETYPE_H

namespace MoodBox
{

class MessageType
{

public:
enum MessageTypeEnum
{
    Undefined = 0,
    Friends = 1,
    Private = 2,
    Group = 3,
    Channel = 4
};

};

}

#endif // MESSAGETYPE_H