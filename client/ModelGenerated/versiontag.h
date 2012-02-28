#ifndef VERSIONTAG_H
#define VERSIONTAG_H

namespace MoodBox
{

class VersionTag
{

public:
enum VersionTagEnum
{
    Undefined = 0,
    Api = 1,
    DesktopClient = 2,
    WebSite = 3,
    WebWidget = 4,
    MobileClient = 5
};

};

}

#endif // VERSIONTAG_H