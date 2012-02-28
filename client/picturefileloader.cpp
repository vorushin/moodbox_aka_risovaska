#include "picturefileloader.h"

#include <QNetworkReply>
#include <QByteArray>

#include "picturefilecache.h"
#include "httptools.h"

namespace MoodBox
{

// Singleton
PictureFileLoader* PictureFileLoader::instance = NULL;

// Common access manager
PictureFileLoader* PictureFileLoader::getInstance()
{
	if (instance == NULL)
		instance = new PictureFileLoader();
	
	return instance;
}

PictureFileLoader::PictureFileLoader()
	: QObject(), reply(NULL)
{
	http = new HttpLoader(this);

	connect(http, SIGNAL(finished(QNetworkReply *)), this, SLOT(onReply(QNetworkReply *)));
}

bool PictureFileLoader::find(const QString &key, QPixmap &picture, const QString &url)
{
	bool found = PictureFileCache::find(key, picture);
	
	if (!url.isEmpty())
		addRequest(key, url);

	return found;
}

void PictureFileLoader::addRequest(const QString &key, const QString &url)
{
	QDateTime moddate;

	if (PictureFileCache::getLastModifyDate(key, moddate))
		moddate = moddate.toUTC();
	
	bool isNewRequest = requests.isEmpty();

	PictureFileLoaderRequest newRequest(url, key, moddate);

	bool isUpdatedRequest = false;

	for (int i = 0; i < requests.size(); i++)
	{
		if (requests.at(i) == newRequest)
		{
			requests.replace(i, newRequest);

			isUpdatedRequest = true;

			break;
		}
	}

	if (!isUpdatedRequest)
		requests.append(newRequest);
	
	if (isNewRequest)
		queryNextPicture();
}

void PictureFileLoader::queryNextPicture()
{
	const PictureFileLoaderRequest &currentRequest = requests.first();

	QNetworkRequest request(currentRequest.url);

	QDateTime dt = currentRequest.lastModifyDate;

	if (dt.isValid())
	{
		QByteArray date = dt.toString(QLatin1String("ddd, dd MMM yyyy hh:mm:ss 'GMT'")).toLatin1();

		request.setRawHeader("If-Modified-Since", date);
	}

	reply = http->get(request);
}

void PictureFileLoader::onReply(QNetworkReply *reply)
{
	if (requests.isEmpty())
		return;

	const PictureFileLoaderRequest &currentRequest = requests.first();

	if (reply != NULL && reply->error() == QNetworkReply::NoError)
	{
		QByteArray data = reply->readAll();

		if (!data.isEmpty())
		{
			QPixmap newPixmap;
			newPixmap.loadFromData(data);

			if (!newPixmap.isNull())
			{
				PictureFileCache::insert(currentRequest.key, newPixmap, QDateTime::currentDateTime().toUTC());

				emit newPictureLoaded(currentRequest.key);
			}
		}
	}

	if (reply != NULL)
	{
		reply->deleteLater();
		reply = NULL;
	}

	requests.pop_front();

	if (!requests.isEmpty())
		queryNextPicture();
}

}