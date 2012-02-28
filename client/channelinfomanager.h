#ifndef CHANNELINFOMANAGER_H
#define CHANNELINFOMANAGER_H

#include <QSettings>
#include <QHash>

namespace MoodBox
{

// Handy macro to access channel info manager singleton
#define CHANNELMANAGER ChannelInfoManager::getInstance()

struct Channel 
{
     int messageId;
	 bool isNotify;
};

class ChannelInfoManager
{
public:
	static ChannelInfoManager* getInstance();

public:
	ChannelInfoManager();

	void load();
	 
	void removeChannel(int id); 

	void setChannelNotifications(int id, bool isNotify);
	bool getChannelNotifications(int id);

	void setLastChannelMessage(int id, int messageId);
	int getLastChannelMessage(int id);

protected:
	void addChannel(int id);
	void save() const;

private:
	static ChannelInfoManager* instance;

	QHash<int, Channel> channels;

};

}

#endif // CHANNELINFOMANAGER_H
