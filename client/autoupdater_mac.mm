#include "autoupdater_mac.h"
#import <Cocoa/Cocoa.h>
#import <Sparkle/Sparkle.h>


#ifndef RUSSIAN_VERSION
#define APPCAST     "http://moodbox.com/appcast.xml"
#else
#define APPCAST     "http://risovaska.ru/appcast.xml"
#endif

#ifdef DEBUG
#define APPCAST     "http://localhost/appcast.xml"
#endif

namespace MoodBox {
class SparkleAutoUpdater::Private 
{
public:
    SUUpdater* updater;
};

SparkleAutoUpdater::SparkleAutoUpdater(QWidget *parentWidget):AutoUpdater(parentWidget) 
{
    QString aUrl(APPCAST);
    d = new Private;
    d->updater = [[SUUpdater sharedUpdater] retain];
    NSURL* url = [NSURL URLWithString:
                  [NSString stringWithUTF8String: aUrl.toUtf8().data()]];
    [d->updater setFeedURL: url];
}

SparkleAutoUpdater::~SparkleAutoUpdater() 
{
    [d->updater release];
    delete d;
}

void SparkleAutoUpdater::checkForUpdateInteractive(QWidget *dialogWidget) 
{
    //[d->updater checkForUpdatesInBackground];
    [d->updater checkForUpdates:nil];
}
}