#ifndef EVENT_H
#define EVENT_H

namespace MoodBox
{

class Event
{

public:
enum EventEnum
{
    StatusChanged = 1,
    UserpicChanged = 2,
    ContactChanged = 3,
    AuthorizationChanged = 4,
    NewMessage = 5,
    Disconnect = 6,
    Reload = 7,
    NewChannelMessage = 8,
    NewCommand = 9
};

};

}

#endif // EVENT_H