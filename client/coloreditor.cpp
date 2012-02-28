#include "coloreditor.h"

#include <QMouseEvent>
#include <QShortcut>

#include "uitools.h"
#include "testtools.h"

namespace MoodBox
{

ColorEditor::ColorEditor(QWidget *parent)
: QWidget(parent, Qt::Popup), attachedWidget(NULL), mode(ColorVolumeWidget::DarkOnly)
{
	TimeMeasure t("ColorEditor");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	setAttribute(Qt::WA_DeleteOnClose);

	colorWidget->setReadOnly(true);

	connect(colorWheel, SIGNAL(colorSelected(const QColor &)), colorVolume, SLOT(setColor(const QColor &)));
	connect(colorVolume, SIGNAL(colorSelected(const QColor &)), colorWidget, SLOT(setColor(const QColor &)));
	connect(colorVolume, SIGNAL(colorSelected(const QColor &)), colorWheel, SLOT(setColor(const QColor &)));

	connect(closeToolButton, SIGNAL(pressed()), this, SLOT(close()));

	// show help by pressing F1
	QShortcut * showHelpShortCut = new QShortcut(this);
	showHelpShortCut->setKey(Qt::Key_F1);
	connect(showHelpShortCut, SIGNAL(activated()), this, SLOT(showHelp()));
}

void ColorEditor::attachToColorWidget(ColorWidget *widget)
{
	if (this->attachedWidget == widget)
		return;

	this->attachedWidget = widget;

	if (widget == NULL)
		return;

	colorWidget->resize(widget->width(), widget->height());

	colorWheel->setColor(widget->getColor());

	QWidget *mapper = (widget->parentWidget() == NULL) ? widget : widget->parentWidget();

	QPoint to = mapper->mapToGlobal(QPoint(widget->x(), widget->y()));

	move(to.x() - colorFrame->geometry().x() - colorWidget->x(), to.y() - colorFrame->geometry().y() - colorWidget->y());
}

void ColorEditor::setColorAndMove(const Color &color, const QPoint &point)
{
	colorWheel->setColor(color);
	move(point.x() - colorFrame->geometry().x() - colorWidget->x(), point.y() - colorFrame->geometry().y() - colorWidget->y());
}

QColor ColorEditor::getColor() const
{
	return colorWidget->getColor();
}

void ColorEditor::setColorVolumeMode(ColorVolumeWidget::Mode mode)
{
	if (mode == this->mode)
		return;

	this->mode = mode;

	colorVolume->setVisible(mode == ColorVolumeWidget::DarkOnly);
}

ColorVolumeWidget::Mode ColorEditor::getColorVolumeMode() const
{
	return this->mode;
}

void ColorEditor::resizeEvent(QResizeEvent *event)
{
	QWidget::resizeEvent(event);

	QRegion colorWidgetRegion(WidgetMaskCreator::createMask(WidgetMaskCreator::WidgetMaskSmall,
		colorFrame->width(), colorFrame->height()));
	colorWidgetRegion.translate(colorFrame->x(), colorFrame->y());

	QRegion windowRegion(WidgetMaskCreator::createMask(WidgetMaskCreator::WidgetMaskSmall, 
		styledFrame->width(), styledFrame->height()));
	windowRegion.translate(styledFrame->x(), styledFrame->y());

	maskRegion = windowRegion.unite(colorWidgetRegion);

	setMask(maskRegion);
}

void ColorEditor::mousePressEvent(QMouseEvent *event)
{
	if (!maskRegion.contains(event->pos()))
	{
		event->accept();
		close();

		return;
	}

	QWidget::mousePressEvent(event);
}

void ColorEditor::on_okButton_clicked()
{
	emit colorSelected(getColor());

	close();
}

void ColorEditor::showHelp()
{
	UiTools::showHelp();
}

}