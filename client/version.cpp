#include "version.h"

#include <QStringList>

namespace MoodBox
{

Version::Version()
{
	init();
}
Version::Version(QString versionString)
{
	qint32 major = 0;
	qint32 minor = 0;
	qint32 build = 0;
	qint32 revision = 0;

	bool allOk = true;
	QStringList list = versionString.split(QChar('.'));
	int count = list.count();
	if(count > 0)
	{
		bool ok;
		major = list.at(0).toInt(&ok);
		allOk = allOk && ok;
		if(ok && count > 1)
		{
			minor = list.at(1).toInt(&ok);
			allOk = allOk && ok;
			if(ok && count > 2)
			{
				build = list.at(2).toInt(&ok);
				allOk = allOk && ok;
				if(ok && count > 3)
				{
					revision = list.at(3).toInt(&ok);
					allOk = allOk && ok;
				}
			}
		}
	}

	if(allOk)
		init(major, minor, build, revision);
	else
		init(0, 0, 0, 0);
}
Version::Version(qint32 major, qint32 minor, qint32 build, qint32 revision)
{
	init(major, minor, build, revision);
}

qint32 Version::getMajor()
{
	return major;
}
qint32 Version::getMinor()
{
	return minor;
}
qint32 Version::getBuild()
{
	return build;
}
qint32 Version::getRevision()
{
	return revision;
}

bool Version::isEmpty()
{
	return major == 0 && minor == 0 && build == 0 && revision == 0;
}

bool Version::operator ==(const Version &other) const
{
	return major == other.major && minor == other.minor && build == other.build && revision == other.revision;
}
bool Version::operator !=(const Version &other) const
{
	return !((*this) == other);
}
bool Version::operator >(const Version &other) const
{
	if(major > other.major)
		return true;
	else if(major == other.major)
	{
		if(minor > other.minor)
			return true;
		else if( minor == other.minor)
		{
			if(build > other.build)
				return true;
			else if(build == other.build)
			{
				if(revision > other.revision)
					return true;
			}
		}
	}

	return false;
}
bool Version::operator <(const Version &other) const
{
	return (*this) != other && !((*this) > other);
}
bool Version::operator >=(const Version &other) const
{
	return (*this) == other || (*this) > other;
}
bool Version::operator <=(const Version &other) const
{
	return (*this) == other || (*this) < other;
}

QString Version::toString()
{
	return QString::number(major) + "." + QString::number(minor) + "." + QString::number(build) + "." + QString::number(revision);
}

QString Version::toStringShort()
{
	QString result = QString::number(major) + "." + QString::number(minor);
	
	if(build != 0 || revision != 0)
	{
		result += "." + QString::number(build);

		if(revision != 0)
			result += "." + QString::number(revision);
	}

	return result;
}

void Version::init(qint32 major, qint32 minor, qint32 build, qint32 revision)
{
	this->major = major;
	this->minor = minor;
	this->build = build;
	this->revision = revision;
}

}