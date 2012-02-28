#include "propertyreader.h"

namespace MoodBox
{

PropertyReadResult::PropertyReadResult(bool isPropertyFound)
{
	this->isPropertyFound = isPropertyFound;
	this->resultObject = 0;
}
PropertyReadResult::PropertyReadResult(TransportableObject *resultObject)
{
	this->isPropertyFound = true;
	this->resultObject = resultObject;
}

bool PropertyReadResult::getIsPropertyFound()
{
	return isPropertyFound;
}
TransportableObject *PropertyReadResult::getResultObject()
{
	return resultObject;
}

}