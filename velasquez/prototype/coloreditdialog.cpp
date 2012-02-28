#include "coloreditdialog.h"

ColorEditDialog::ColorEditDialog(QWidget *parent)
	: QDialog(parent, Qt::FramelessWindowHint), attachedWidget(NULL), mode(ColorVolumeWidget::DarkOnly)
{
	setupUi(this);

	colorVolumeLight->setMode(ColorVolumeWidget::DarkToLight);
	colorVolumeLight->setColor(QColor(Qt::red));
	colorVolumeLight->hide();

	colorVolumeDark->setColor(QColor(Qt::red));

	connect(colorWheel, SIGNAL(colorSelected(const QColor &)), colorVolumeDark, SLOT(setColor(const QColor &)));
	connect(colorVolumeDark, SIGNAL(colorSelected(const QColor &)), colorWidget, SLOT(setColor(const QColor &)));
	connect(colorVolumeDark, SIGNAL(colorSelected(const QColor &)), colorWheel, SLOT(setColor(const QColor &)));

	connect(colorWheel, SIGNAL(colorSelected(const QColor &)), colorVolumeLight, SLOT(setColor(const QColor &)));
	connect(colorVolumeLight, SIGNAL(colorSelected(const QColor &)), colorWheel, SLOT(setColor(const QColor &)));

	connect(closeButton, SIGNAL(pressed()), this, SLOT(reject()));
	connect(okButton, SIGNAL(pressed()), this, SLOT(accept()));
}

ColorEditDialog::~ColorEditDialog()
{
}

void ColorEditDialog::attachToColorWidget(ColorWidget *colorVolumeLight)
{
	this->attachedWidget = colorVolumeLight;

	if (colorVolumeLight == NULL)
		return;

	colorWidget->resize(colorVolumeLight->width(), colorVolumeLight->height());

	colorWheel->setColor(colorVolumeLight->getColor());

	QWidget *mapper = (colorVolumeLight->parentWidget() == NULL) ? colorVolumeLight : colorVolumeLight->parentWidget();

	QPoint to = mapper->mapToGlobal(QPoint(colorVolumeLight->x(), colorVolumeLight->y()));

	move(to.x() - colorWidget->x(), to.y() - colorWidget->y());
}

QColor ColorEditDialog::getColor() const
{
	return colorWidget->getColor();
}

void ColorEditDialog::setColorVolumeMode(ColorVolumeWidget::Mode mode)
{
	if (mode == this->mode)
		return;

	this->mode = mode;

	colorVolumeDark->setVisible(mode == ColorVolumeWidget::DarkOnly);
	colorVolumeLight->setVisible(mode == ColorVolumeWidget::DarkToLight);
}

ColorVolumeWidget::Mode ColorEditDialog::getColorVolumeMode() const
{
	return this->mode;
}

void ColorEditDialog::resizeEvent(QResizeEvent *event)
{
	QDialog::resizeEvent(event);

	QRegion colorWidgetRegion(0, colorWidget->y() - colorWidget->height() / 2, colorWidget->width() * 3, colorWidget->height() * 2);
	QRegion windowRegion(colorWidget->height() * 2, 0, width(), height());

	setMask(windowRegion.unite(colorWidgetRegion));
}
