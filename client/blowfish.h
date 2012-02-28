// _THE BlowFishEnc ENCRYPTION ALGORITHM_
// by Bruce Schneier
// Revised code--3/20/94
// Converted to C++ class 5/96, Jim Conger
// Updated to support FileEncryption Utility by Nir Dremer, 9/02

#ifndef BLOWFISH_H
#define BLOWFISH_H

#include <QtGlobal>

#define MAX_ENC_KEY_LENGTH	100

class BlowFishEnc
{
public:
	BlowFishEnc(const char* pwd = NULL);
	~BlowFishEnc();

	void setPassword(const char* pwd);

	quint32 encryptStream(const char *plain, const quint32 size, char *cipher);
	quint32 decryptStream(const char *cipher, const quint32 size, char *plain);
	quint32 GetOutputLength(quint32 lInputLong);

protected:
    char _encryptionKey[MAX_ENC_KEY_LENGTH];

	void clear();

private:
	quint32 	*PArray;
	quint32	(*SBoxes)[256];

	void 	BlowFishEnc_encipher(quint32 *xl, quint32 *xr);
	void 	BlowFishEnc_decipher(quint32 *xl, quint32 *xr);
};

union aword {
	quint32 num;
	unsigned char bytes [4];
	struct {
	unsigned int byte3:8;
	unsigned int byte2:8;
	unsigned int byte1:8;
	unsigned int byte0:8;
	} w;
};

#endif // BLOWFISH_H