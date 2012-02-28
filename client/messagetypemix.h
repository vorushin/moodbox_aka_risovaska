#ifndef MESSAGETYPEMIX_H
#define MESSAGETYPEMIX_H

#include "messagetype.h"

namespace MoodBox
{

// Simple mix class for classes working with messages
class MessageTypeMix
{
public:
	MessageTypeMix() : messageType(MessageType::Undefined), recipientId(-1), messageTypeInitialized(false) {};
	virtual ~MessageTypeMix() {};

	inline MessageType::MessageTypeEnum getMessageType() const { return messageType; };

	inline virtual void setRecipient(MessageType::MessageTypeEnum type, qint32 recipientId = -1) { this->recipientId = recipientId; this->messageType = type; messageTypeInitialized = true; };
	inline qint32 getRecipient() const { return recipientId; };

protected:
	MessageType::MessageTypeEnum messageType;
	qint32 recipientId;	

	inline bool isMessageTypeInitialized() const { return messageTypeInitialized; };
	inline void unInitializeMessageType() { messageTypeInitialized = false; };

private:
	bool messageTypeInitialized;

};

}

#endif // MESSAGETYPEMIX_H