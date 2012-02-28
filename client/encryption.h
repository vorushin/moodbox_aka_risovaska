#ifndef ENCRYPTION_H
#define ENCRYPTION_H

class QByteArray;

namespace MoodBox
{

// длина ключа шифрования
#define KEY_LENGTH		16

// статический класс для шифровки-дешифровки массивов данных
class Encryptor
{
public:
	static void encrypt(const QByteArray &plain, QByteArray &cipher);
	static void decrypt(const QByteArray &cipher, QByteArray &plain);
};

}

#endif // ENCRYPTION_H