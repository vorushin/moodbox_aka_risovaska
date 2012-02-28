#include "serverrequest.h"
#include "peopleinfomanager.h"

namespace MoodBox
{

ServerRequest::ServerRequest()
	: QObject(INFOMANAGER), active(true)
{
}

}
