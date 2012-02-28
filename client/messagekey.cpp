#include "messagekey.h"

namespace MoodBox
{

MessageKey::MessageKey()
{
	first = QDateTime::currentDateTime().toUTC();
	second = 0;
}

MessageKey::MessageKey(const QDateTime& date, qint32 id)
{
	first = date;
	second = id;
}

QString MessageKey::toString() const
{
	return QString(MESSAGE_KEY_TEMPLATE).arg(first.toString(MESSAGE_KEY_TIMESTAMP)).arg(QString::number(second));
}

QVariant MessageKey::toVariant() const
{
	QVariant v;
	MessageKey copy(first, second);
	v.setValue(copy);

	return v;
}

MessageKey MessageKey::fromString(const QString &string)
{
	qint32 lastPos = string.lastIndexOf(MESSAGE_KEY_SEPARATOR);
	MessageKey key(QDateTime(), -1);

	if (lastPos <= 0)
		return key;

	// Parse id
	QString idString = string.mid(lastPos + 1);

	bool goodId;

	qint32 id = idString.toInt(&goodId);

	if (!goodId)
		id = -1;

	// Parse date
	QString dateString = string;
	dateString.truncate(lastPos);
	QDateTime date = QDateTime::fromString(dateString, MESSAGE_KEY_TIMESTAMP);
	date.setTimeSpec(Qt::UTC);

	key.first = date;
	key.second = id;

	return key;
}

}