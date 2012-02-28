#ifndef MESSAGEKEY_H
#define MESSAGEKEY_H

#include <QPair>
#include <QDateTime>
#include <QString>

#include <QMetaType>
#include <QVariant>

namespace MoodBox
{

#define MESSAGE_KEY_SEPARATOR			"_"
#define MESSAGE_KEY_TIMESTAMP			"yyyy.MM.dd"MESSAGE_KEY_SEPARATOR"hh-mm-ss-zzz"
#define MESSAGE_KEY_TEMPLATE			"%1"MESSAGE_KEY_SEPARATOR"%2"

// Universal message key
class MessageKey : public QPair<QDateTime, qint32>
{
public:
	MessageKey();
	MessageKey(const QDateTime &date, qint32 id);

	inline void setDate(const QDateTime &date) { first = date; };
	inline QDateTime getDate() const { return first; };

	inline void setId(qint32 id) { second = id; };
	inline qint32 getId() const { return second; };

	inline bool isValid() const { return first.isValid() && (second >= 0); };

	QString toString() const;
	QVariant toVariant() const;

	static MessageKey fromString(const QString &string);
};

}

Q_DECLARE_METATYPE(MoodBox::MessageKey)

#endif // MESSAGEKEY_H