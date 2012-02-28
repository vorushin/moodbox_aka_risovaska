#ifndef VERSION_H
#define VERSION_H

#include <QtGlobal>

namespace MoodBox
{

#define APP_VERSION				(Version(1,5,0,0))

class Version 
{
public:
	Version();
	Version(QString versionString);
	Version(qint32 major, qint32 minor, qint32 build, qint32 revision);

	qint32 getMajor();
	qint32 getMinor();
	qint32 getBuild();
	qint32 getRevision();

	bool isEmpty();

	bool operator ==(const Version &other) const;
	bool operator !=(const Version &other) const;
	bool operator >(const Version &other) const;
	bool operator <(const Version &other) const;
	bool operator >=(const Version &other) const;
	bool operator <=(const Version &other) const;

	QString toString();
	QString toStringShort();

private:
	qint32 major;
	qint32 minor;
	qint32 build;
	qint32 revision;

	void init(qint32 major = 0, qint32 minor = 0, qint32 build = 0, qint32 revision = 0);
};

}

#endif // VERSION_H
