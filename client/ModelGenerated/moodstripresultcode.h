#ifndef MOODSTRIPRESULTCODE_H
#define MOODSTRIPRESULTCODE_H

namespace MoodBox
{

class MoodstripResultCode
{

public:
enum MoodstripResultCodeEnum
{
    Ok = 1,
    MoodstripNotFound = 2,
    MoodstripIsPublished = 3,
    MoodstripIsEmpty = 4
};

};

}

#endif // MOODSTRIPRESULTCODE_H