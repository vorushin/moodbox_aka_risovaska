#include "statusicon.h"

namespace MoodBox
{

int StatusIcon::currentConnectingIcon = 0;

const QIcon& StatusIcon::getStatusIcon(const UserStatus::UserStatusEnum status)
{
	static const QIcon offlineIcon(STATUS_OFFLINE_ICON);
	static const QIcon connectingIcon(STATUS_CONNECTING_ICON);
	static const QIcon onlineIcon(STATUS_ONLINE_ICON);
	static const QIcon empty;

	switch (status)
	{
		case UserStatus::Offline: return offlineIcon;
		case UserStatus::Connecting: return connectingIcon;
		case UserStatus::Online: return onlineIcon;
		case UserStatus::Undefined: return offlineIcon;
	}

	return empty;
}

const QPixmap StatusIcon::getStatusPixmap(const UserStatus::UserStatusEnum status)
{
	return StatusIcon::getStatusIcon(status).pixmap(QSize(1000, 1000));
}

void StatusIcon::resetConnectingStatusIcon()
{
	currentConnectingIcon = 0;
}

QIcon& StatusIcon::getNextConnectingIcon()
{
	static QIcon icons[] = {QIcon(":/MoodBox/Resources/offline.png"), 
						    QIcon(":/MoodBox/Resources/connecting1.png"),
							QIcon(":/MoodBox/Resources/connecting2.png"),
							QIcon(":/MoodBox/Resources/connecting3.png"),
							QIcon(":/MoodBox/Resources/connecting4.png"),
							QIcon(":/MoodBox/Resources/connecting5.png"),
							QIcon(":/MoodBox/Resources/connecting6.png"),
							QIcon(":/MoodBox/Resources/online.png")
	};

	currentConnectingIcon++;
	if (currentConnectingIcon >= (int) (sizeof(icons) / sizeof(icons[0])))
			currentConnectingIcon = 0;

	return icons[currentConnectingIcon];
}

}