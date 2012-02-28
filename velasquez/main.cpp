#include <QtGui/QApplication>
#include <QtPlugin>

#include "vtest.h"

using namespace Velasquez;

Q_IMPORT_PLUGIN(qgif);
Q_IMPORT_PLUGIN(qjpeg);
Q_IMPORT_PLUGIN(qmng);
Q_IMPORT_PLUGIN(qsvg);
Q_IMPORT_PLUGIN(qtiff);

int main(int argc, char *argv[])
{
	QApplication a(argc, argv);
	VTest w;
	w.show();
	return a.exec();
}
