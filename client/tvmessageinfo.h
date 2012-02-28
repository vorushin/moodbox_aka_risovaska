#ifndef TVMESSAGEINFO_H
#define TVMESSAGEINFO_H

#include "messagekey.h"
#include "messagetype.h"

namespace MoodBox
{

// Message info for TV widget messages list
struct TVMessageInfo
{
public:
	bool isNull;

	qint32 authorId;
	QString authorLogin;

	QString previewFileName;
	QString thumbnailFileName;

	bool sent;
	
	TVMessageInfo(): isNull(true), authorId(0), sent(false) {};

	TVMessageInfo(qint32 authorId, const QString &authorLogin, const QString &previewFileName, const QString &thumbnailFileName, bool sent): isNull(false)
	{
		this->authorId = authorId;
		this->authorLogin = authorLogin;

		this->previewFileName = previewFileName;
		this->thumbnailFileName = thumbnailFileName;

		this->sent = sent;
	};

	static TVMessageInfo getMessageInfo(const MessageKey &key, MessageType::MessageTypeEnum messageType, qint32 contactId);
};

}

#endif // TVMESSAGEINFO_H