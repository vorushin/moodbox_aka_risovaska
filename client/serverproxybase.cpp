#include "serverproxybase.h"

#include "envelope.h"
#include "fault.h"
#include "header.h"
#include "transportchannelbase.h"
#include "xmlparser.h"

namespace MoodBox
{

ServerProxyBase::ServerProxyBase(Model* model, TransportChannelBase* channel)
{
	this->model = model;
	this->channel = channel;
}

ServerProxyBase::~ServerProxyBase()
{

}

void ServerProxyBase::send(Callback callback, QVariant state, TransportableObject* object)
{
	Envelope* envelope = new Envelope(getHeader(), object);
	channel->send(model, this, callback, state, envelope, object->getTypeId() + 1);
}

void ServerProxyBase::handleResponse(Callback callback, QVariant state, qint32 resultTypeId, QIODevice &device)
{
	Envelope* envelope = XmlParser<Envelope>::parse(model, &device);

	bool hasParserError = true;

	if(envelope != NULL)
	{
		TransportableObject *result = envelope->getBody();
		if(result != NULL)
		{
			if(result->isFault())
				result->resultFaultCall(this, callback, state, resultTypeId);
			else
				result->resultCall(callback, state);
			hasParserError = false;
		}

		delete envelope;
	}
	if (hasParserError)
	{ // Additional failure notification
		Fault fault(FAULT_PARSER_ERROR, QString(), QString());
		this->resultFaultCall(callback, state, fault, resultTypeId);
	}
}

Header ServerProxyBase::getHeader()
{
	return Header();
}

}