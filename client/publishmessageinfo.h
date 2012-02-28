#ifndef PUBLISHMESSAGEINFO_H
#define PUBLISHMESSAGEINFO_H

#include <QString>

#include "varianthash.h"

namespace MoodBox
{

class PublishMessageInfo : public VariantHash
{
public:

	bool isSent;
	qint32 messageId;
	qint32 authorId;
	QString authorLogin;
	QString imagePath;

	PublishMessageInfo(bool isSent, qint32 messageId, qint32 authorId, const QString authorLogin, const QString &imagePath) : VariantHash()
	{
		this->isSent = isSent;
		this->messageId = messageId;
		this->authorId = authorId;
		this->authorLogin = authorLogin;
		this->imagePath = imagePath;
	};
	
	PublishMessageInfo() : VariantHash(), isSent(false), messageId(-1), authorId(-1)
	{
	};
};

}

#endif // PUBLISHMESSAGEINFO_H