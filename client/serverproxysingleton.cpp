#include "serverproxysingleton.h"

#include <QUrl>

#include "httpchannel2.h"
#include "moodboxmodel.h"
#include "programsettings.h"

namespace MoodBox
{

MoodBoxModel* ServerProxySingleton::model = NULL;
MoodBoxCustomServer* ServerProxySingleton::singleton = NULL;
HttpChannel2* ServerProxySingleton::channel = NULL;

MoodBoxCustomServer* ServerProxySingleton::getInstance()
{
	if (singleton == NULL)
	{
		model = new MoodBoxModel();
		model->fill();

		// Fetch server URL
		ProgramSettings settings;
		settings.readSettings();
		QUrl url(settings.serverUrl);

		channel = new HttpChannel2(url);
		singleton = new MoodBoxCustomServer(model, channel);
	}

	return singleton;
}

}