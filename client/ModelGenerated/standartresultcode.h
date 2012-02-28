#ifndef STANDARTRESULTCODE_H
#define STANDARTRESULTCODE_H

namespace MoodBox
{

class StandartResultCode
{

public:
enum StandartResultCodeEnum
{
    Undefined = 0,
    Ok = 1,
    Empty = 2,
    Hidden = 3,
    Forbidden = 4
};

};

}

#endif // STANDARTRESULTCODE_H