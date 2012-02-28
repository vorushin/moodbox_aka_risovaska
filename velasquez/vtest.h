#ifndef VTEST_H
#define VTEST_H

#include <QtGui/QMainWindow>

#include "ui_vtest.h"

namespace Velasquez
{

using namespace Ui;

class VTest : public QMainWindow, public VTestClass
{
	Q_OBJECT

public:
	VTest(QWidget *parent = 0, Qt::WFlags flags = 0);
	~VTest();

protected slots:
	void on_editorFormAction_triggered();

	void on_exitAction_triggered();
};

}

#endif // VTEST_H
