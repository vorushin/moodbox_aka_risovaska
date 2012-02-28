#include "logger.h"

#include <QDateTime>
#include <QFile>

void logMessage(const QString &msg)
{
	QFile file("moodbox.log");
	file.open(QIODevice::Append);
	file.write((QDateTime::currentDateTime().toString() + ": " + msg + "\n").toLocal8Bit());
	file.close();
}