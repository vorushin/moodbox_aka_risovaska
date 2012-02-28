#include "encryption.h"

#include <QString>
#include <QByteArray>
#include <QDir>

#include "blowfish.h"

namespace MoodBox
{

// ключ шифрования
QString key;

// шифровщик
BlowFishEnc encoder(NULL);

// размер буфера шифрования
#define ENC_BUFFER_SIZE	1024

void generateKey()
{
	QString keyname = QDir::home().dirName();

	key = QString();

	while (key.length() < KEY_LENGTH)
		key += keyname;
	
	key.chop(key.length() - KEY_LENGTH);

	encoder.setPassword(key.toLocal8Bit().constData());
}

void Encryptor::encrypt(const QByteArray &plain, QByteArray &cipher)
{
	if (key.isEmpty())
		generateKey();

	char buff[ENC_BUFFER_SIZE];
	cipher.clear();

	for(int i = 0; i < plain.size(); i += ENC_BUFFER_SIZE)
	{
		QByteArray slice = plain.mid(i, ENC_BUFFER_SIZE);
		const char* plainData = slice.constData();

		int encsize = encoder.encryptStream(plainData, slice.size(), buff);
		cipher.append(QByteArray(buff, encsize));
	}
}

void Encryptor::decrypt(const QByteArray &cipher, QByteArray &plain)
{
	if (key.isEmpty())
		generateKey();

	char buff[ENC_BUFFER_SIZE];

	for(int i = 0; i < cipher.size(); i += ENC_BUFFER_SIZE)
	{
		QByteArray slice = cipher.mid(i, ENC_BUFFER_SIZE);
		const char* cipherData = slice.constData();

		int encsize = encoder.decryptStream(cipherData, slice.size(), buff);
		plain.append(QByteArray(buff, encsize));
	}
}

}
