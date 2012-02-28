#ifndef COLOREDITDIALOG_H
#define COLOREDITDIALOG_H

#include <QDialog>

#include "ui_coloreditdialog.h"

using namespace Ui;

class ColorEditDialog : public QDialog, public ColorEditDialogClass
{
	Q_OBJECT

public:
	ColorEditDialog(QWidget *parent = 0);
	~ColorEditDialog();

	void attachToColorWidget(ColorWidget *widget);
	QColor getColor() const;

	void setColorVolumeMode(ColorVolumeWidget::Mode mode);
	ColorVolumeWidget::Mode getColorVolumeMode() const;

protected:
	virtual void resizeEvent(QResizeEvent *event);

private:
	ColorWidget *attachedWidget;
	ColorVolumeWidget::Mode mode;

};

#endif // COLOREDITDIALOG_H
