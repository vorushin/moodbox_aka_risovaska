#ifndef SERVERREQUEST_H
#define SERVERREQUEST_H

#include <QObject>

namespace MoodBox
{

// Base class for server request callbacks processing
class ServerRequest : public QObject
{
	Q_OBJECT

public:
	ServerRequest();

	void detach() { active = false; };

protected:
	bool active;
};

}

#endif // SERVERREQUEST_H
