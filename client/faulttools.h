#ifndef FAULTTOLS_H
#define FAULTTOLS_H

#include "fault.h"

namespace MoodBox
{

#define FAULT_FILE_EXTENSION		".fault"

// Utility class to work with faults
class FaultTools
{
public:
	enum FaultReaction
	{
		RetryIfPossible,
		LongRetryIfPossible,
		StopRequests,
		RequestCannotBeSent
	};

	static FaultReaction getFaultReaction(const Fault &fault);

	static bool isNetworkError(const Fault &fault);
	static bool saveFault(Fault *fault, const QString &filePath);
};

}

#endif // FAULTTOLS_H