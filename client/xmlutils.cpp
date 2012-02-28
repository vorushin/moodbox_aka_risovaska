#include "xmlutils.h"

#include <QStringList>

namespace MoodBox
{

QString XmlUtils::pathToString(const QList<QPointF> &path)
{
	QString pathString;
	
	if (path.isEmpty())
		return QString();
	else
	{
		qreal x = path.at(0).x();
		qreal y = path.at(0).y();
		pathString = QString(M_PATH_STRING).arg(x).arg(y);

		for (int i = 1; i < path.size(); i++)
		{
			x = path.at(i).x();
			y = path.at(i).y();
			pathString += QString(L_PATH_STRING).arg(x).arg(y);
		}
	}

	return pathString;
}

QList<QPointF> XmlUtils::stringToPath(const QString &pathString)
{
	QString pathSpacedString = pathString;
	pathSpacedString.replace("M", "m ", Qt::CaseInsensitive);
	pathSpacedString.replace("l", "l ", Qt::CaseInsensitive);

	// split string: "M 321,60.1 l 200,12.5 l 59,43"
	// to: "m", "321,60.1", "l"... and so on
	QStringList pathStringList = pathSpacedString.split(" ", QString::SkipEmptyParts);
	
	QList<QPointF> path;

	// we need to parse every second element: "321,60.1"
	for (int i = 1; i < pathStringList.size(); i += 2)
	{
		// parse string: "321,60.1" 
		// to: "321", "60.1"
		QStringList pathXYList = pathStringList.at(i).split(",");
		
		if (pathXYList.size() < 2)
			return QList<QPointF>();
		else
		{
			qreal x = pathXYList.at(0).toDouble();
			qreal y = pathXYList.at(1).toDouble();
			path.append(QPointF(x, y));
		}
	}

	return path;
}

QString XmlUtils::colorToHexString(const QColor &color)
{
	QString colorHexString = color.name();
	int alpha = color.alpha();
	QString alphaHex = QString::number(alpha, 16);
	colorHexString.append(alphaHex);

	return colorHexString;
}

QColor XmlUtils::hexStringToColor(const QString &string)
{
	bool convertIsOk;

	QString colorHexString = string;

	int colorAlpha = colorHexString.mid(7, 2).toInt(&convertIsOk, 16);
	
	if (!convertIsOk)
		return QColor();
	
	colorHexString.remove(7, 2);
	
	QColor color;
	color.setNamedColor(colorHexString);
	color.setAlpha(colorAlpha);
	
	return color;
}

QString XmlUtils::realListToString(const QList<qreal> &list)
{
	QString result;

	foreach(qreal item, list)
		result += QString::number(item) + " ";

	result.truncate(result.length() - 1);
	
	return result;
}

QList<qreal> XmlUtils::stringToRealList(const QString &listString)
{
	QList<qreal> result;

	QStringList list = listString.split(" ", QString::SkipEmptyParts);

	bool ok;

	foreach(QString item, list)
	{
		qreal d = item.toDouble(&ok);

		if (ok)
			result.append(d);
	}

	return result;
}

}