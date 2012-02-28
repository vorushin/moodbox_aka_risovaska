#include "mtrandom.h"

#include <QDateTime>

MTRandom::MTRandom()
{
	QDateTime time = QDateTime::currentDateTime();
	quint32 seed = (quint32)(time.toTime_t() * 1000 + time.time().msec());

	setSeed(seed);
}

MTRandom::MTRandom(quint32 seed)
{
	setSeed(seed);
}

MTRandom::~MTRandom()
{
}

quint32 MTRandom::getNextInt()
{
	if (statePos == stateLength)
		generateState();

	quint32 tmp = state[statePos];
	statePos++;

	tmp ^= (tmp >> 11);
	tmp ^= (tmp << 7) & 0x9D2C5680;
	tmp ^= (tmp << 15) & 0xEFC60000;

	return tmp ^ (tmp >> 18);
}

qint32 MTRandom::getNextIntRanged(qint32 min, qint32 max)
{
	return min + qAbs(getNextReal()) * (max-min);
}

qreal MTRandom::getNextReal()
{
	return ((qreal)getNextInt()) / ((qreal)4294967295.0); // divide by (2^32 - 1)
}

qreal MTRandom::getNextRealRanged(qreal min, qreal max)
{
	return min + getNextReal() * (max-min);
}

void MTRandom::setSeed(quint32 seed)
{
	state[0] = seed;
	
	for (int i = 1; i < stateLength; i++)
		state[i] = 1812433253 * (state[i - 1] ^ (state[i - 1] >> 30)) + i;

	// force generateState() on next getNextInt()
	statePos = stateLength;
}

void MTRandom::generateState()
{
	for (int i = 0; i < (stateLength - m); i++)
		state[i] = state[i + m] ^ twiddle(state[i], state[i + 1]);

	for (int i = stateLength - m; i < (stateLength - 1); i++)
		state[i] = state[i + m - stateLength] ^ twiddle(state[i], state[i + 1]);

	state[stateLength - 1] = state[m - 1] ^ twiddle(state[stateLength - 1], state[0]);

	// reset state position
	statePos = 0;
}

quint32 MTRandom::twiddle(quint32 u, quint32 v)
{
	return (((u & 0x80000000) | (v & 0x7FFFFFFF)) >> 1) ^ ((v & 1) ? 0x9908B0DF : 0);
}