#ifndef STATUSICON_H
#define STATUSICON_H

#include <QObject>
#include <QIcon>
#include <QPixmap>

#include "userstatus.h"

namespace MoodBox
{

#define STATUS_OFFLINE_ICON			":/MoodBox/Resources/offline.png"
#define STATUS_ONLINE_ICON			":/MoodBox/Resources/online.png"
#define STATUS_CONNECTING_ICON		":/MoodBox/Resources/connecting1.png"
#define STATUS_NOTAUTHORIZED_ICON	":/MoodBox/Resources/not_auth.png"

class StatusIcon : public QObject
{
	Q_OBJECT

public:

	static const QIcon& getStatusIcon(const UserStatus::UserStatusEnum status);
	static const QPixmap getStatusPixmap(const UserStatus::UserStatusEnum status);

	static void resetConnectingStatusIcon();
	static QIcon& getNextConnectingIcon();

private:
	static int currentConnectingIcon;
};

}

#endif // STATUSICON_H
