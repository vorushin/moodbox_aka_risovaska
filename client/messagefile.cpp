#include "messagefile.h"

#include <QFile>
#include <QBuffer>

#include "drawingelement.h"
#include "editorscene.h"
#include "imageelement.h"
#include "messageorganizer.h"
#include "debug.h"

#include "imagetools.h"
#include "common.h"

namespace MoodBox
{

using namespace Velasquez;

// MessageFileBase class
MessageFileBase::MessageFileBase()
	: MessageTypeMix(), XmlSerializable(), authorId(-1), sent(false), isPublic(false)
{
}

MessageFileBase::MessageFileBase(MessageType::MessageTypeEnum type, qint32 authorId)
	: MessageTypeMix(), XmlSerializable(), sent(false), isPublic(false)
{
	this->messageType = type;
	this->authorId = authorId;
}

MessageFileBase::MessageFileBase(qint32 recipientId, qint32 authorId)
	: MessageTypeMix(), XmlSerializable(), sent(false), isPublic(false)
{
	this->messageType = MessageType::Private;
	this->recipientId = recipientId;
	this->authorId = authorId;
}

MessageFileBase::SerializationResult MessageFileBase::save()
{
	QFile file(fileName);

	if (!file.open(QIODevice::WriteOnly | QIODevice::Truncate))
		return BadDevice;

	QXmlStreamWriter writer(&file);

	SerializationResult result = saveToXml(&writer);

	file.close();

	return result;
}

MessageFileBase::SerializationResult MessageFileBase::load()
{
	QFile file(fileName);

	if (!file.open(QIODevice::ReadOnly))
		return BadDevice;

	QXmlStreamReader reader(&file);

	return loadFromXml(&reader);
}

MessageFileBase::SerializationResult MessageFileBase::saveToXml(QXmlStreamWriter* writer) const
{
	if (writer == NULL || writer->device() == NULL)
		return BadDevice;

	writer->writeStartElement(MESSAGE_FILE_XML_TAGNAME);

	writer->writeAttribute(MESSAGE_FILE_ID_ATTRIBUTE, QString::number(key.getId()));
	writer->writeAttribute(MESSAGE_TYPE_XML_ATTRIBUTE, messageTypeToString(getMessageType()));
	writer->writeAttribute(MESSAGE_SENTDATE_XML_ATTRIBUTE, key.getDate().toString(MESSAGE_TIMESTAMP));

	if (getMessageType() == MessageType::Private || getMessageType() == MessageType::Channel)
		writer->writeAttribute(MESSAGE_RECIPIENT_XML_ATTRIBUTE, QString::number(recipientId));

	writer->writeAttribute(MESSAGE_AUTHOR_XML_ATTRIBUTE, QString::number(authorId));

	if (!authorLogin.isEmpty())
		writer->writeAttribute(MESSAGE_AUTHORLOGIN_XML_ATTRIBUTE, authorLogin);

	writer->writeAttribute(MESSAGE_SENT_XML_ATTRIBUTE, messageBoolToString(getSent()));
	writer->writeAttribute(MESSAGE_ISPUBLIC_XML_ATTRIBUTE, messageBoolToString(getPublic()));

	SerializationResult result = saveContentToXml(writer);

	if (result != Ok)
		return result;

	writer->writeEndElement();

	return Ok;
}

MessageFileBase::SerializationResult MessageFileBase::loadFromXml(QXmlStreamReader* reader)
{
	if (reader == NULL || reader->device() == NULL)
		return BadDevice;

	if (!moveToElement(reader, MESSAGE_FILE_XML_TAGNAME))
		return BadFormat;

	QXmlStreamAttributes attributes = reader->attributes();
	
	bool ok;

	// Load id
	QString idString = attributes.value(MESSAGE_FILE_ID_ATTRIBUTE).toString();
	qint32 readedId = idString.toInt(&ok);

	setId( (ok) ? readedId : -1);

	// Load message type
	QString typeString = attributes.value(MESSAGE_TYPE_XML_ATTRIBUTE).toString();
	
	MessageType::MessageTypeEnum readedType = messageTypeFromString(typeString, &ok);
	
	if (!ok)
		return BadFormat;

	// Load sent date
	QString dateString = attributes.value(MESSAGE_SENTDATE_XML_ATTRIBUTE).toString();
	QDateTime readedDt = QDateTime::fromString(dateString, MESSAGE_TIMESTAMP);

	if (!readedDt.isValid())
		return BadFormat;
	else
	{
		readedDt.setTimeSpec(Qt::UTC);
		setSentDate(readedDt);
	}

	int readedRecipient = -1;

	// Load recipientId, if needed
	if (readedType == MessageType::Private || readedType == MessageType::Channel)
	{
		QString recipientString = attributes.value(MESSAGE_RECIPIENT_XML_ATTRIBUTE).toString();
		readedRecipient = recipientString.toInt(&ok);

		if (!ok)
			return BadFormat;
	}
	
	setRecipient(readedType, readedRecipient);

	// Load author
	QString authorString = attributes.value(MESSAGE_AUTHOR_XML_ATTRIBUTE).toString();
	int readedAuthor = authorString.toInt(&ok);

	if (!ok)
		return BadFormat;
	else
		setAuthor(readedAuthor);

	authorString = attributes.value(MESSAGE_AUTHORLOGIN_XML_ATTRIBUTE).toString();
	setAuthorLogin(authorString);

	// Load sent & public
	setSent(messageBoolFromString(attributes.value(MESSAGE_SENT_XML_ATTRIBUTE).toString()));
	setPublic(messageBoolFromString(attributes.value(MESSAGE_ISPUBLIC_XML_ATTRIBUTE).toString()));

	SerializationResult result = loadContentFromXml(reader);

	if (result != Ok)
		return result;

	return Ok;
}

MessageFileBase::SerializationResult MessageFileBase::saveContentToXml(QXmlStreamWriter* writer) const
{
	Q_UNUSED(writer)

	return Ok;
}

MessageFileBase::SerializationResult MessageFileBase::loadContentFromXml(QXmlStreamReader* reader)
{
	Q_UNUSED(reader)

	return Ok;
}

QString MessageFileBase::messageTypeToString(MessageType::MessageTypeEnum type)
{
	switch (type)
	{
		case MessageType::Private: return MESSAGE_TYPE_PRIVATE_VALUE;
		case MessageType::Friends: return MESSAGE_TYPE_FRIENDS_VALUE;
		case MessageType::Channel: return MESSAGE_TYPE_CHANNEL_VALUE;

		default: return QString();
	}
}

MessageType::MessageTypeEnum MessageFileBase::messageTypeFromString(const QString &string, bool *ok)
{
	if (string == MESSAGE_TYPE_PRIVATE_VALUE)
		return MessageType::Private;

	if (string == MESSAGE_TYPE_FRIENDS_VALUE)
		return MessageType::Friends;

	if (string == MESSAGE_TYPE_CHANNEL_VALUE)
		return MessageType::Channel;

	if (ok != NULL)
		ok = false;

	return MessageType::Undefined;
}

QString MessageFileBase::messageBoolToString(bool value)
{
	return (value) ? MESSAGE_YES_VALUE : MESSAGE_NO_VALUE;
}

bool MessageFileBase::messageBoolFromString(const QString &string)
{
	return (string == MESSAGE_YES_VALUE);
}

// MessageFile class
MessageFile::MessageFile()
	: MessageFileBase()
{
}

MessageFile::MessageFile(MessageType::MessageTypeEnum type, qint32 authorId)
	: MessageFileBase(type, authorId)
{
}

MessageFile::MessageFile(qint32 recipientId, qint32 authorId)
	: MessageFileBase(recipientId, authorId)
{
}

QString MessageFile::getImageFormatFromInfo() const
{
	static const int prefixLength = strlen(METAINFO_IMAGEFORMAT_VALUEPREFIX);

	if (info.isEmpty())
		return DEFAULT_IMAGE_FORMAT;

	int i = info.lastIndexOf(METAINFO_IMAGEFORMAT_VALUEPREFIX);

	if (i == -1)
		return DEFAULT_IMAGE_FORMAT;
	else
		i += prefixLength;

	int j = info.indexOf("\"", i);

	if (j == -1)
		return DEFAULT_IMAGE_FORMAT;

	QString format = info.mid(i, j - i);

	return format;
}

void MessageFile::setPreview(const QImage &preview)
{
	this->preview = preview;

	updateBytesFromPreview();
}

void MessageFile::setPreview(const QByteArray &previewBytes)
{
	this->previewBytes = previewBytes;

	updatePreviewFromBytes();
}

MessageFile::SerializationResult MessageFile::saveContentToXml(QXmlStreamWriter* writer) const
{
	// Call empty writeCharacters("") to push Qt to write ">" close tag
	writer->writeCharacters("");

	// Write buffer content directly
	QByteArray bytes;
	bytes.append(info);
	writer->device()->write(bytes);

	return Ok;
}

MessageFile::SerializationResult MessageFile::loadContentFromXml(QXmlStreamReader* reader)
{
	// Load meta info manually
	qint64 mediaStartPos = reader->characterOffset();
	qint64 mediaEndPos = mediaStartPos;

	// Need to do it 
	while (!reader->atEnd())
	{
		if (reader->isEndElement() && reader->name() == MESSAGE_FILE_XML_TAGNAME)
			break;

		mediaEndPos = reader->characterOffset();
		reader->readNext();
	}

	// Now we know the size of content
	qint64 currentDevicePos = reader->device()->pos();

	// Read the content
	reader->device()->seek(mediaStartPos);
	QByteArray bytes = reader->device()->read(mediaEndPos - mediaStartPos);
	this->info = QString(bytes);

	// Return device to current position 
	reader->device()->seek(currentDevicePos);

	return Ok;
}

void MessageFile::updateBytesFromPreview()
{	
	QByteArray latin1 = getImageFormatFromInfo().toLatin1();
	const char *format = latin1.constData();

	if (getImageFormatFromInfo() == ALTERNATE_IMAGE_FORMAT)
	{
		previewBytes = ImageTools::saveToBytesAlternative(preview);
	}
	else
	{
		QBuffer buffer(&previewBytes);

		buffer.open(QIODevice::WriteOnly);
		
		preview.save(&buffer, format);

		buffer.close();

		Q_ASSERT_X(!previewBytes.isEmpty(), "MessageFile::updateBytesFromPreview()", "Empty message data");
	}
}

void MessageFile::updatePreviewFromBytes()
{
	QBuffer buffer(&previewBytes);

	buffer.open(QIODevice::ReadOnly);

	QByteArray latin1 = getImageFormatFromInfo().toLatin1();
	const char *format = latin1.constData();

	preview.load(&buffer, format);

	buffer.close();
}

}