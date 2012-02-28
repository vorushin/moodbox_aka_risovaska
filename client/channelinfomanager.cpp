#include "channelinfomanager.h"
#include "peopleinfomanager.h"

namespace MoodBox
{

// Singleton
ChannelInfoManager* ChannelInfoManager::instance = NULL;

ChannelInfoManager* ChannelInfoManager::getInstance()
{
	if (instance == NULL)
		instance = new ChannelInfoManager();
	
	return instance;
}

ChannelInfoManager::ChannelInfoManager()
{
}

void ChannelInfoManager::load()
{
	channels.clear();

	QSettings settings(INFOMANAGER->getUserSettingsFolder() + SETTINGS_INI_FILE, QSettings::IniFormat);
	int size = settings.beginReadArray("channels");

	int channelId;
	for (int i = 0; i < size; ++i) 
	{
		settings.setArrayIndex(i);

		Channel channel;
		channelId = settings.value("id").toInt();
		channel.messageId = settings.value("messageId").toInt();
		channel.isNotify = settings.value("isNotify").toBool();
		channels.insert(channelId, channel);
	}
	settings.endArray();
}

void ChannelInfoManager::removeChannel(int id)
{
	channels.remove(id);

	save();
}

void ChannelInfoManager::setChannelNotifications(int id, bool isNotify)
{
	if (!channels.contains(id))
	{
		addChannel(id);
	}
	channels[id].isNotify = isNotify;

	save();
}

bool ChannelInfoManager::getChannelNotifications(int id)
{
	if (!channels.contains(id))
	{
		addChannel(id);
		save();
	}
	return channels[id].isNotify;
}

void ChannelInfoManager::setLastChannelMessage(int id, int messageId)
{
	if (!channels.contains(id))
	{
		addChannel(id);
	}
	channels[id].messageId = messageId;

	save();
}

int ChannelInfoManager::getLastChannelMessage(int id)
{
	if (!channels.contains(id))
	{
		addChannel(id);
		save();
	}
	return channels.value(id).messageId;
}

void ChannelInfoManager::addChannel(int id)
{
	Channel channel = Channel();
	channel.messageId = 0;
	channel.isNotify = true;
	channels[id] = channel;
}

void ChannelInfoManager::save() const
{
	QSettings settings(INFOMANAGER->getUserSettingsFolder() + SETTINGS_INI_FILE, QSettings::IniFormat);
	settings.beginWriteArray("channels");

	QHash<int, Channel>::const_iterator channelIterator = channels.begin();
	int i = 0;
	while (channelIterator != channels.end()) 
	{
		settings.setArrayIndex(i);
		settings.setValue("id", channelIterator.key());
		settings.setValue("messageId", channelIterator.value().messageId);
		settings.setValue("isNotify", channelIterator.value().isNotify);

		++channelIterator;
		i++;
	}
	settings.endArray();
}

}