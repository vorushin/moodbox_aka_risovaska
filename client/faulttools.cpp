#include "faulttools.h"

#include <QFile>

#include "serverproxysingleton.h"
#include "moodboxmodel.h"
#include "xmlpropertywriter.h"

namespace MoodBox
{

FaultTools::FaultReaction FaultTools::getFaultReaction(const Fault &fault)
{
	QString code = fault.getCode();

	if (code == FAULT_TRANSPORT_ERROR || code == FAULT_REQUEST_TIMED_OUT)
	{
		return RetryIfPossible;
	}
	else if(code == FAULT_REQUEST_TOO_LARGE)
	{
		return RequestCannotBeSent;
	}
	else if (code == FAULT_REQUEST_CANCELLED || code == FAULT_NOT_AUTHENTICATED || code == FAULT_UNSUPPORTED_CLIENT_VERSION)
	{
		return StopRequests;
	}
	else
	{
		return LongRetryIfPossible;
	}
}

bool FaultTools::isNetworkError(const Fault &fault)
{
	if (fault.isNull())
		return false;

	return fault.getCode() == FAULT_TRANSPORT_ERROR;
}

bool FaultTools::saveFault(Fault *fault, const QString &filePath)
{
	QFile file(filePath);

	if (!file.open(QIODevice::WriteOnly))
		return false;

	XmlPropertyWriter::write(SERVER->getModel(), &file, fault);

	return true;
}

}