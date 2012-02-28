#ifndef XMLUTILS_H
#define XMLUTILS_H

#include <QString>
#include <QList>
#include <QPointF>
#include <QColor>

namespace MoodBox
{

#define M_PATH_STRING		"M %1,%2"
#define L_PATH_STRING		" l %1,%2"

// Helper utils for xml parsing
class XmlUtils
{
public:
	// Convert from QList<QPointF> ---> to string like: "M 100,50 l 10,10 l 5.5,6"
	static QString pathToString(const QList<QPointF> &path);
	static QList<QPointF> stringToPath(const QString &pathString);

	// Convert QColor <-> hex string #RRGGBBAA
	static QString colorToHexString(const QColor &color);
	static QColor hexStringToColor(const QString &string);

	// Convert QList<qreal> to string "132.4 345 45"
	static QString realListToString(const QList<qreal> &list);
	static QList<qreal> stringToRealList(const QString &listString);
};

}

#endif // XMLUTILS_H