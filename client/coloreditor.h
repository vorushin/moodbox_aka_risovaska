#ifndef COLOREDITOR_H
#define COLOREDITOR_H

#include <QWidget>

#include "ui_coloreditor.h"
#include "color.h"

namespace MoodBox
{

using namespace Ui;

// Advanced color selection/edit window
class ColorEditor : public QWidget, public ColorEditorClass
{
	Q_OBJECT

public:
	ColorEditor(QWidget *parent = 0);

	void attachToColorWidget(ColorWidget *widget);
	void setColorAndMove(const Color &color, const QPoint &point);
	QColor getColor() const;

	void setColorVolumeMode(ColorVolumeWidget::Mode mode);
	ColorVolumeWidget::Mode getColorVolumeMode() const;

signals:
	void colorSelected(const Color &color);

protected:
	virtual void resizeEvent(QResizeEvent *event);

	virtual void mousePressEvent(QMouseEvent *event);

private:
	ColorWidget *attachedWidget;
	ColorVolumeWidget::Mode mode;
	QRegion maskRegion;

private slots:
	void on_okButton_clicked();

	void showHelp();
};

}

#endif // COLOREDITOR_H
