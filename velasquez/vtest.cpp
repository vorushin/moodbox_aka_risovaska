#include "vtest.h"

#include "editorform.h"

namespace Velasquez
{

VTest::VTest(QWidget *parent, Qt::WFlags flags)
	: QMainWindow(parent, flags)
{
	setupUi(this);
}

VTest::~VTest()
{
}

void VTest::on_editorFormAction_triggered()
{
	EditorForm *f = new EditorForm();
	f->show();
}

void VTest::on_exitAction_triggered()
{
	qApp->quit();
}

}