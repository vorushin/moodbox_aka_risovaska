#include "blogtools.h"

#include <QNetworkAccessManager>
#include <QNetworkRequest>
#include <QNetworkReply>
#include <QByteArray>
#include <QDateTime>
#include <QCryptographicHash>
#include <QTextCodec>

namespace MoodBox
{

// BlogPoster class
BlogPoster::BlogPoster(QObject *parent) : QObject(parent)
{
	http = new QNetworkAccessManager(this);

	connect(http, SIGNAL(finished(QNetworkReply *)), this, SLOT(onNetworkReply(QNetworkReply *)));
}

QString BlogPoster::getEscapedString(const QString &s)
{
	QByteArray a = s.toUtf8();

	a = a.toPercentEncoding();
	
	QString escaped(a);

	return escaped;
}

QString BlogPoster::getHtmlString(const QString &s)
{
	QString htmlString(s);

	htmlString.replace("&", "&amp;");
	htmlString.replace("<", "&lt;");
	htmlString.replace(">", "&gt;");
	htmlString.replace("\"", "&quot;");

	return htmlString;
}

QString BlogPoster::getPasswordHash(const QString &password)
{
	QCryptographicHash hash(QCryptographicHash::Md5);
	hash.addData(password.toAscii());

	return QString(hash.result().toHex());
}

// LJPoster class
LJPoster::LJPoster(QObject *parent) : BlogPoster(parent), loginCheck(false)
{
}

void LJPoster::checkLoginAndPassword(const QString &userName, const QString &userPassword)
{
	loginCheck = true;

	QString requestString = QString("mode=checkfriends&user=%1&hpassword=%2&auth_method=clear").arg(userName).arg(getPasswordHash(userPassword));

	// Make request
	QNetworkRequest request(QUrl(LJ_POSTER_INTERFACE_URL));
		
	// Post request
	http->post(request, requestString.toAscii());
}

void LJPoster::post(const QString &userName, const QString &userPassword, const QString &caption, const QString &body, bool isPublic)
{	
	// Fill in date
	QDate currentDate = QDateTime::currentDateTime().date();
	QTime currentTime = QDateTime::currentDateTime().time();
	QString dateString = QString("year=%1&mon=%2&day=%3&hour=%4&min=%5").arg(currentDate.year()).arg(currentDate.month()).arg(currentDate.day()).arg(currentTime.hour()).arg(currentTime.minute());

	// Prepare password
	QString passwordString = getPasswordHash(userPassword);

	// Prepare visibility
	QString visibilityString = (isPublic) ? "public" : "private";
	QString requestString1 = QString("mode=postevent&user=%1&hpassword=%2&auth_method=clear&ver=1").arg(userName).arg(passwordString);
	QString requestString2 = QString("&event=") + body;
	QString requestString3 = QString("&lineendings=unix&subject=%1&security=%2&%3&prop_taglist=%4,").arg(caption).arg(visibilityString).arg(dateString).arg(tr(BLOG_POSTER_POST_TAG));

	QString requestString = requestString1 + requestString2 + requestString3;

	// Make request
	loginCheck = false;
	QNetworkRequest request(QUrl(LJ_POSTER_INTERFACE_URL));
	
	request.setHeader(QNetworkRequest::ContentTypeHeader, "application/x-www-form-urlencoded; charset=UTF-8");
	
	// Post request
	http->post(request, requestString.toUtf8());
}

void LJPoster::onNetworkReply(QNetworkReply *reply)
{
	QNetworkReply::NetworkError error = reply->error();
	QByteArray data = reply->readAll();

	bool success = (error == QNetworkReply::NoError) && data.contains("OK");
	QString info;

	// Extract error message, if any
	if (!success)
	{
		static const int errMsgLen = strlen(LJ_POSTER_ERRORMESSAGE_TAG);

		int errorMsgPos = data.indexOf(LJ_POSTER_ERRORMESSAGE_TAG);

		if (errorMsgPos >= 0)
		{
			int successPos = data.indexOf(LJ_POSTER_SUCCESS_TAG);

			errorMsgPos += errMsgLen;

			int count = (errorMsgPos < successPos) ? successPos - errorMsgPos : -1;

			info = QString(data.mid(errorMsgPos, count));
		}
	}

	if (loginCheck)
	{
		emit loginChecked(success, info);
	}
	else
	{
		if (success)
		{
			int i = data.indexOf("http");
			QByteArray url = data.mid(i, data.size() - i - 1);

			info = QString(url);
		}

		emit postCompleted(success, info);
	}
}

// LI Poster class
LIPoster::LIPoster(QObject *parent) : BlogPoster(parent), loginCheck(false)
{
	codec = QTextCodec::codecForName("Windows-1251");
}

void LIPoster::checkLoginAndPassword(const QString &userName, const QString &userPassword)
{
	loginCheck = true;

	QByteArray requestString = "username=" + codec->fromUnicode(userName).toPercentEncoding() + "&password=" + 
		                    codec->fromUnicode(userPassword).toPercentEncoding() + 
							"&s=&url=http%3A%2F%2Fwww.liveinternet.ru%2Ftop%2F&action=login";
	
	// Make request
	QNetworkRequest request(QUrl(LI_POSTER_LOGIN_INTERFACE_URL));
	request.setHeader(QNetworkRequest::ContentTypeHeader, "application/x-www-form-urlencoded");
	
	// Post request
	http->post(request, requestString);
}

void LIPoster::post(const QString &userName, const QString &userPassword, const QString &caption, const QString &body, bool isPublic)
{	
	QString requestString1 = QString(LI_CONTENT).arg("commentsubscribe").arg("1");
	QString requestString2 = QString(LI_CONTENT).arg("offline").arg("1");
	QString requestString3 = QString(LI_CONTENT).arg("username").arg(userName);
	QString requestString4 = QString(LI_CONTENT).arg("password").arg(userPassword);
	QString requestString5 = QString(LI_CONTENT).arg("message").arg(body);
	QString requestString6 = QString(LI_CONTENT).arg("headerofpost").arg(caption);
	QString requestString7 = QString(LI_CONTENT).arg("tags").arg(tr(BLOG_POSTER_POST_TAG));
	if (!isPublic)
		QString requestString7 = QString(LI_CONTENT).arg("privatepost").arg("1");
	QString requestString8 = QString(LI_BOUNDARY);
	
	QString requestString = requestString1 + requestString2 + requestString3 + requestString4 + requestString5 + requestString6 + requestString7
						    + requestString8;

	// Make request
	loginCheck = false;
	QNetworkRequest request(QUrl(LI_POSTER_POST_INTERFACE_URL));
	
	request.setHeader(QNetworkRequest::ContentTypeHeader, "multipart/form-data; boundary=7d239f19102c8");
	
	// Post request
	http->post(request, codec->fromUnicode(requestString));
}

void LIPoster::onNetworkReply(QNetworkReply *reply)
{
	QNetworkReply::NetworkError error = reply->error();
	QByteArray data = reply->readAll();

	QString info;
	bool success = false;
	int start = 0;
	int length;
	int end;

	if (loginCheck)
	{
		bool isCookie = reply->hasRawHeader("set-cookie");
		if (isCookie)
		{
			QByteArray cookies = reply->rawHeader("set-cookie");
			start = cookies.indexOf(LI_POSTER_USER_ID_TAG);
			length = QString(LI_POSTER_USER_ID_TAG).length();
			end = cookies.indexOf(";", start + length);
			userId = cookies.mid(start + length, end - start - length);
		}

		success = (error == QNetworkReply::NoError) && isCookie && (start >= 0);

		emit loginChecked(success, info);
	}
	else
	{
		success = (error == QNetworkReply::NoError) && data.contains("OK");

		if (success)
		{
			QByteArray url = LI_POSTER_POST_URL + userId.toAscii() + "/post" + data.right(data.size() - QString("OK\n").length()) + "/";

			info = QString(url);
		}
		else
		{
			start = data.indexOf(LI_POSTER_ERRORMESSAGE_TAG);
			length = QString(LI_POSTER_ERRORMESSAGE_TAG).length();
			if (start >= 0)
			{
				info = QString(data.right(data.count() - length));
			}
		}

		emit postCompleted(success, info);
	}
}

// Diary Poster class
DiaryPoster::DiaryPoster(QObject *parent) : BlogPoster(parent), loginCheck(false)
{
}

void DiaryPoster::checkLoginAndPassword(const QString &userName, const QString &userPassword)
{
	QString requestString = QString(DIARY_POSTER_GET_BLOGS_COMMAND).arg(userName).arg(userPassword);

	loginCheck = true;
	QNetworkRequest request(QUrl(DIARY_POSTER_INTERFACE_URL));
	
	request.setHeader(QNetworkRequest::ContentTypeHeader, "application/xml");
	
	// Post request
	http->post(request, requestString.toUtf8());
}

void DiaryPoster::post(const QString &userName, const QString &userPassword, const QString &caption, const QString &body, bool isPublic)
{	
	QString requestString = QString(DIARY_POSTER_POST_COMMAND).arg(blogId).arg(userName).arg(userPassword).arg(caption).arg(body).arg(tr(BLOG_POSTER_POST_TAG)).arg((isPublic ? "True" : "False"));
	
	// Make request
	loginCheck = false;
	QNetworkRequest request(QUrl(DIARY_POSTER_INTERFACE_URL));
	
	request.setHeader(QNetworkRequest::ContentTypeHeader, "application/xml");
	
	// Post request
	http->post(request, requestString.toUtf8());
}

void DiaryPoster::onNetworkReply(QNetworkReply *reply)
{
	QNetworkReply::NetworkError error = reply->error();
	QByteArray data = reply->readAll();

	bool success = false;
	int start;
	int length;
	int end;
	QString info;

	if (loginCheck)
	{
		success = (error == QNetworkReply::NoError) && data.contains(DIARY_POSTER_BLOG_ID_TAG);

		if (success)
		{
			start = data.indexOf(DIARY_POSTER_BLOG_ID_TAG);
			length = QString(DIARY_POSTER_BLOG_ID_TAG).length();
			end = data.indexOf("<", start + length);
			blogId = QString(data.mid(start + length, end - start - length));

			start = data.indexOf(DIARY_POSTER_BLOG_URL_TAG);
			length = QString(DIARY_POSTER_BLOG_URL_TAG).length();
			end = data.indexOf("<", start + length);
			blogUrl = QString(data.mid(start + length, end - start - length));

			emit loginChecked(success, info);
		}
	}
	else
	{
		success = (error == QNetworkReply::NoError) && data.contains(DIARY_POSTER_POST_ID_TAG);

		if (success)
		{
			start = data.indexOf(DIARY_POSTER_POST_ID_TAG);
			length = QString(DIARY_POSTER_POST_ID_TAG).length();
			end = data.indexOf("<", start);
			
			QByteArray url = blogUrl.toUtf8() + "/p" + data.mid(start + length, end - start - length);

			info = QString(url);

			emit postCompleted(success, info);
		}
	}

	// Extract error message, if any
	if (!success)
	{
		start = data.indexOf(DIARY_POSTER_ERRORMESSAGE_TAG);
		length = QString(DIARY_POSTER_ERRORMESSAGE_TAG).length();
		end = data.indexOf("<", start);

		if (start >= 0)
		{
			info = QString(data.mid(start + length, end - start - length));
		}

		emit postCompleted(success, info);
	}
}

}