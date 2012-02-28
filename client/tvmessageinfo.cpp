#include "tvmessageinfo.h"

#include "messagefile.h"
#include "messageorganizer.h"

using namespace Velasquez;

namespace MoodBox
{

TVMessageInfo TVMessageInfo::getMessageInfo(const MessageKey &key, MessageType::MessageTypeEnum messageType, qint32 contactId)
{
	MessageFileBase messageFile;
	QString fileName = MessageOrganizer::getFilePath(key, messageType, contactId);
	messageFile.setFileName(fileName);
	XmlSerializable::SerializationResult result = messageFile.load();

	if(result != XmlSerializable::Ok)
	{
		return TVMessageInfo();
	}

	QString previewFileName = MessageOrganizer::getPreviewFileName(fileName);

	return TVMessageInfo(messageFile.getAuthor(), messageFile.getAuthorLogin(), previewFileName, MessageOrganizer::getThumbnailFileName(previewFileName), messageFile.getSent());
}

}