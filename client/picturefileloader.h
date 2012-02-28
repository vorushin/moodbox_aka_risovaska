#ifndef PICTUREFILELOADER_H
#define PICTUREFILELOADER_H

#include <QObject>
#include <QList>

#include <QPixmap>
#include <QDateTime>

class QNetworkReply;

namespace MoodBox
{

// Macro to access info Picture loader singleton
#define PICTURELOADER PictureFileLoader::getInstance()

struct PictureFileLoaderRequest
{
public:
	QString url;
	QString key;
	QDateTime lastModifyDate;

	PictureFileLoaderRequest(const QString &url, const QString &key, const QDateTime &lastModifyDate)
		: url(url), key(key), lastModifyDate(lastModifyDate) 
	{};
	
	PictureFileLoaderRequest() 
	{};

	bool operator == (const PictureFileLoaderRequest &other) const { return url == other.url; };
};

class HttpLoader;

// Loader of pictures via URLs and storing them to common picture file cache
class PictureFileLoader : public QObject
{
	Q_OBJECT

public:
	static PictureFileLoader* getInstance();

public:
	// Gets picture from cache and runs update if url specified
	bool find(const QString &key, QPixmap &picture, const QString &url = QString());

signals:
	void newPictureLoaded(const QString &key);

private:
	QList <PictureFileLoaderRequest> requests;
	HttpLoader *http;
	
	QNetworkReply *reply;

	PictureFileLoader();

	void addRequest(const QString &key, const QString &url);
	void queryNextPicture();

	static PictureFileLoader* instance;

private slots:
	void onReply(QNetworkReply *reply);
};

}

#endif // PICTUREFILELOADER_H
