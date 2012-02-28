#ifndef MESSAGEFILE_H
#define MESSAGEFILE_H

#include "xmlserializable.h"

#include <QList>
#include <QByteArray>
#include <QImage>

#include "messagekey.h"
#include "messagetypemix.h"

namespace Velasquez
{
	class DrawingElement;
}

namespace MoodBox
{

#define MESSAGE_FILE_EXTENSION				".mbm"
#define MESSAGE_TIMESTAMP					"dd.MM.yyyy hh-mm-ss-zzz"

#define MESSAGE_FILE_XML_TAGNAME			"Message"
#define MESSAGE_FILE_ID_ATTRIBUTE			"Id"
#define MESSAGE_TYPE_XML_ATTRIBUTE			"Type"
#define MESSAGE_SENTDATE_XML_ATTRIBUTE		"SentDate"
#define MESSAGE_RECIPIENT_XML_ATTRIBUTE		"Recipient"
#define MESSAGE_AUTHOR_XML_ATTRIBUTE		"Author"
#define MESSAGE_AUTHORLOGIN_XML_ATTRIBUTE	"AuthorLogin"
#define MESSAGE_SENT_XML_ATTRIBUTE			"Sent"
#define MESSAGE_ISPUBLIC_XML_ATTRIBUTE		"IsPublic"
#define MESSAGE_ONBEHALF_XML_TAGNAME		"OnBehalf"

#define MESSAGE_TYPE_PRIVATE_VALUE			"Private"
#define MESSAGE_TYPE_FRIENDS_VALUE			"Friends"
#define MESSAGE_TYPE_CHANNEL_VALUE			"Channel"
#define MESSAGE_YES_VALUE					"Yes"
#define MESSAGE_NO_VALUE					"No"

#define METAINFO_IMAGEFORMAT_TITLE			"Image"
#define METAINFO_IMAGEFORMAT_TAGNAME		"Type"
#define METAINFO_IMAGEFORMAT_VALUEPREFIX	"image/"

// Drawing Message file base info
class MessageFileBase : public MessageTypeMix, protected XmlSerializable
{
public:
	MessageFileBase();
	MessageFileBase(MessageType::MessageTypeEnum type, qint32 authorId);
	MessageFileBase(qint32 recipientId, qint32 authorId);

	inline void setId(qint32 id) { key.setId(id); };
	inline qint32 getId() const { return key.getId(); };

	inline void setSentDate(const QDateTime &date) { key.setDate(date); };
	inline QDateTime getSentDate() const { return key.getDate(); };

	inline void setAuthor(qint32 authorId) { this->authorId = authorId; };
	inline qint32 getAuthor() const { return authorId; };

	inline void setAuthorLogin(const QString &authorLogin) { this->authorLogin = authorLogin; };
	inline QString getAuthorLogin() const { return authorLogin; };

	inline void setSent(bool sent) { this->sent = sent; };
	inline bool getSent() const { return sent; };

	inline void setPublic(bool isPublic) { this->isPublic = isPublic; };
	inline bool getPublic() const { return isPublic; };

	inline void setFileName(const QString &fileName) { this->fileName = fileName; };
	inline QString getFileName() const { return this->fileName; };

	inline MessageKey getMessageKey() const { return key; };

	SerializationResult save();
	SerializationResult load();

	static QString messageTypeToString(MessageType::MessageTypeEnum type);
	static MessageType::MessageTypeEnum messageTypeFromString(const QString &string, bool *ok = NULL);

	static QString messageBoolToString(bool value);
	static bool messageBoolFromString(const QString &string);

protected:
	virtual SerializationResult saveToXml(QXmlStreamWriter* writer) const;
	virtual SerializationResult loadFromXml(QXmlStreamReader* reader);

	virtual SerializationResult saveContentToXml(QXmlStreamWriter* writer) const;
	virtual SerializationResult loadContentFromXml(QXmlStreamReader* reader);

private:
	MessageKey key;

	qint32 authorId;
	QString authorLogin;
	
	bool sent;
	bool isPublic;

	QString fileName;
};

// Drawing Message file with elements buffer
class MessageFile : public MessageFileBase
{
public:
	MessageFile();
	MessageFile(MessageType::MessageTypeEnum type, qint32 authorId);
	MessageFile(qint32 recipientId, qint32 authorId);

	// Information
	inline void setInfo(const QString &info) { this->info = info; };
	QString getInfo() const { return info; };

	// Image format info
	QString getImageFormatFromInfo() const;

	// Preview works
	void setPreview(const QImage &preview);
	void setPreview(const QByteArray &previewBytes);

	inline QImage getPreview() const { return preview; };
	inline QByteArray getPreviewBytes() const { return previewBytes; };

protected:
	virtual SerializationResult saveContentToXml(QXmlStreamWriter* writer) const;
	virtual SerializationResult loadContentFromXml(QXmlStreamReader* reader);

private:
	QString info;
	QImage preview;
	QByteArray previewBytes;

	void updateBytesFromPreview();
	void updatePreviewFromBytes();
};

}

#endif // MESSAGEFILE_H