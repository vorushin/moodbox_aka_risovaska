#ifndef MTRANDOM_H
#define MTRANDOM_H

#include <QtGlobal>

// Mersenne Twister random generator
class MTRandom
{
public:
	MTRandom();
	MTRandom(quint32 seed);
	virtual ~MTRandom();

	quint32 getNextInt();
	qint32 getNextIntRanged(qint32 min, qint32 max);

	qreal getNextReal();
	qreal getNextRealRanged(qreal min, qreal max);

private:
	static const int stateLength = 624, m = 397;

	int statePos;
	quint32 state[stateLength];

	void init();
	void setSeed(quint32 seed);

	void generateState();
	quint32 twiddle(quint32 u, quint32 v);
};

#endif // MTRANDOM_H