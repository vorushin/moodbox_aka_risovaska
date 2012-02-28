#ifndef BLOGTOOLS_H
#define BLOGTOOLS_H

#include <QObject>

class QNetworkAccessManager;
class QNetworkReply;

namespace MoodBox
{

#define BLOG_POSTER_POST_TAG			QT_TRANSLATE_NOOP("@default", "MoodBoxPostTag")

#define LJ_POSTER_INTERFACE_URL			"http://www.livejournal.com/interface/flat"
#define LJ_POSTER_SUCCESS_TAG			"success"
#define LJ_POSTER_ERRORMESSAGE_TAG		"errmsg"

#define LI_POSTER_LOGIN_INTERFACE_URL	"http://www.liveinternet.ru/member.php"
#define LI_POSTER_POST_INTERFACE_URL	"http://www.liveinternet.ru/offline_addpost.php"
#define LI_POSTER_POST_URL				"http://www.liveinternet.ru/users/"
#define LI_POSTER_USER_ID_TAG			"bbuserid="
#define LI_CONTENT						"--7d239f19102c8\nContent-Disposition: form-data; name=\"%1\"\n\n%2\n"
#define LI_BOUNDARY						"--7d239f19102c8--\n"
#define LI_POSTER_ERRORMESSAGE_TAG		"ERROR:"

#define DIARY_POSTER_INTERFACE_URL		"http://www.diary.ru/client/mwa.php"
#define DIARY_POSTER_GET_BLOGS_COMMAND	"<?xml version=\"1.0\"?><methodCall><methodName>blogger.getUsersBlogs</methodName><params><param><value><string>C6CE3FFB3174106584CBB250C0B0519BF4E294</string></value></param><param><value><string>%1</string></value></param><param><value><string>%2</string></value></param></params></methodCall>"
#define DIARY_POSTER_POST_COMMAND		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><methodCall><methodName>metaWeblog.newPost</methodName><params><param><value><string>%1</string></value></param><param><value><string>%2</string></value></param><param><value><string>%3</string></value></param><param><value><struct><member><name>title</name><value><string>%4</string></value></member><member><name>description</name><value><string>%5</string></value></member><member><name>categories</name><value><array><data><value>%6</value></data></array></value></member><member><name>publish</name><value><boolean>%7</boolean></value></member></struct></value></param><param><value><boolean>True</boolean></value></param></params></methodCall>"
#define DIARY_POSTER_BLOG_URL_TAG		"url</name>\n<value><string>"
#define DIARY_POSTER_BLOG_ID_TAG		"blogid</name>\n<value><string>"
#define DIARY_POSTER_POST_ID_TAG		".post"
#define DIARY_POSTER_ERRORMESSAGE_TAG	"faultString</name>\n<value><string>"

// Class for posting messages to blogs
class BlogPoster : public QObject
{
	Q_OBJECT

public:
	BlogPoster(QObject *parent = 0);

	virtual void post(const QString &userName, const QString &userPassword, const QString &caption, const QString &body, bool isPublic = true) = 0;

	static QString getEscapedString(const QString &s);
	static QString getHtmlString(const QString &s);

signals:
	void postCompleted(bool success, const QString &info);

protected:
	QNetworkAccessManager *http;

	static QString getPasswordHash(const QString &password);

protected slots:
	virtual void onNetworkReply(QNetworkReply *reply) { Q_UNUSED(reply) };
};

// LJ poster class
class LJPoster : public BlogPoster
{
	Q_OBJECT

public:
	LJPoster(QObject *parent = 0);

	void checkLoginAndPassword(const QString &userName, const QString &userPassword);

	virtual void post(const QString &userName, const QString &userPassword, const QString &caption, const QString &body, bool isPublic = true);

signals:
	void loginChecked(bool success, const QString &errorMessage);

protected slots:
	virtual void onNetworkReply(QNetworkReply *reply);

private:
	bool loginCheck;
};

// LI poster class
class LIPoster : public BlogPoster
{
	Q_OBJECT

public:
	LIPoster(QObject *parent = 0);

	void checkLoginAndPassword(const QString &userName, const QString &userPassword);

	virtual void post(const QString &userName, const QString &userPassword, const QString &caption, const QString &body, bool isPublic = true);

signals:
	void loginChecked(bool success, const QString &errorMessage);

protected slots:
	virtual void onNetworkReply(QNetworkReply *reply);

private:
	bool loginCheck;
	QString userId;
	QTextCodec *codec;
};

// Dairy poster class
class DiaryPoster : public BlogPoster
{
	Q_OBJECT

public:
	DiaryPoster(QObject *parent = 0);

	void checkLoginAndPassword(const QString &userName, const QString &userPassword);

	virtual void post(const QString &userName, const QString &userPassword, const QString &caption, const QString &body, bool isPublic = true);

signals:
	void loginChecked(bool success, const QString &errorMessage);

protected slots:
	virtual void onNetworkReply(QNetworkReply *reply);

private:
	bool loginCheck;
	QString blogUrl;
	QString blogId;

};

}

#endif // BLOGTOOLS_H