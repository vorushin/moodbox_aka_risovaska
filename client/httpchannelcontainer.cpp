#include "httpchannelcontainer.h"

#include <QFile>

namespace MoodBox
{

HttpChannelContainer::HttpChannelContainer(ServerResponseHandler *handler, Callback &callback, 
										   QVariant state, qint32 resultTypeId, QBuffer *buffer) :
	handler(handler), callback(callback), state(state), resultTypeId(resultTypeId), buffer(buffer), isCancelled(false) {
	outBuffer = new QBuffer();
}

HttpChannelContainer::~HttpChannelContainer()
{
	if (buffer)
	{
		buffer->close();
		buffer->deleteLater();
		buffer = NULL;
	}
	if (outBuffer)
	{
		outBuffer->close();
		outBuffer->deleteLater();
		outBuffer = NULL;
	}
}

void HttpChannelContainer::handleResponse()
{
	outBuffer->open(QIODevice::ReadWrite);
	outBuffer->seek(0);
	handler->handleResponse(callback, state, resultTypeId, *outBuffer);
	deleteLater();
}

void HttpChannelContainer::handleFault()
{
	handler->resultFaultCall(callback, state, fault, resultTypeId);

#ifndef QT_NO_DEBUG
	QFile file(QString("Logs/response_errors" + QDateTime::currentDateTime().toString("yyyyMMdd_hhmmss") + ".txt"));
	file.open(QIODevice::WriteOnly);
	file.write(fault.getCode().toAscii() + " " + fault.getDescription().toAscii() + " " + fault.getDetails().toAscii());
	file.close();
#endif

	deleteLater();
}

}