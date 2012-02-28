#ifndef FLAGSCOLLECTION_H
#define FLAGSCOLLECTION_H

#include <QMap>

namespace MoodBox
{

// Collection of key and flags
class FlagsCollection : protected QMap<qint32, qint32> 
{
public:
	FlagsCollection() : QMap<qint32, qint32>() {};
	~FlagsCollection() {};

	bool getFlag(qint32 id, qint32 flag) const;
	void setFlag(qint32 id, qint32 flag);
	void clearFlag(qint32 id, qint32 flag);

	void clear();	
};

}

#endif // FLAGSCOLLECTION_H
