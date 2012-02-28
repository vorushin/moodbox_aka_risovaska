#include "editorform.h"

#include <QUndoStack>
#include <QUndoView>
#include <QGraphicsRectItem>
#include <QXmlStreamReader>
#include <QXmlStreamWriter>
#include <QTextStream>
#include <QIODevice>
#include <QBuffer>
#include <QFile>
#include <QVariant>

#include "editorscene.h"
#include "toolbox.h"
#include "settingsprovider.h"
#include "penelement.h"
#include "pentool.h"
#include "svgelement.h"
#include "svgtool.h"
#include "imagetool.h"
#include "imageelement.h"
#include "texttool.h"
#include "textelement.h"
#include "oilbrushtool.h"
#include "oilbrushelement.h"
#include "erasertool.h"
#include "eraserelement.h"
#include "sprayelement.h"
#include "spraytool.h"
#include "simplebrushelement.h"
#include "simplebrushtool.h"
#include "backgroundtool.h"
#include "eyedroppertool.h"

#include "palettemanager.h"

#include "vcommon.h"
#include "debug.h"

namespace Velasquez
{

EditorForm::EditorForm(QWidget *parent)
	: QWidget(parent), undoView(NULL), elementCache(NULL)
{
	setupUi(this);

	colorPalette->setColumnCount(2);

	connect(colorPalette, SIGNAL(colorSelected(const QColor &)), this, SLOT(onColorSelected(const QColor&)));

	connect(penSizeButton, SIGNAL(sizeSelected(qreal)), this, SLOT(onSizeSelected(qreal)));
	connect(penSizeButton, SIGNAL(sizeSelected(qreal)), penAlphaButton, SLOT(setSize(qreal)));
	connect(penAlphaButton, SIGNAL(alphaSelected(qreal)), this, SLOT(onAlphaSelected(qreal)));

	// Scene
	scene = new EditorScene(this);
	editorView->setScene(scene);

	// Stack
	undoStack = new QUndoStack(this);

	undoButton->setDefaultAction(undoStack->createUndoAction(this));
	redoButton->setDefaultAction(undoStack->createRedoAction(this));

	// Comment to hide debug stack
	//undoView = new QUndoView(undoStack);

	// Settings
	settings = new SettingsProvider(this);
	settings->setSetting(PenElement::PenWidth, DEFAULT_PEN_WIDTH);
	settings->setSetting(PenElement::PenColor, QColor(DEFAULT_PEN_COLOR));

	oilBrushSettings = new SettingsProvider(this);
	oilBrushSettings->setSetting(PenElement::PenWidth, DEFAULT_OILBRUSH_WIDTH);
	oilBrushSettings->setSetting(PenElement::PenColor, QColor(DEFAULT_OILBRUSH_COLOR));
	oilBrushSettings->includeExternalSetting(DEFAULT_OILBRUSH_COLOR, settings);

	spraySettings = new SettingsProvider(this);
	spraySettings->setSetting(PenElement::PenWidth, DEFAULT_SPRAY_WIDTH);
	spraySettings->setSetting(PenElement::PenColor, QColor(DEFAULT_SPRAY_COLOR));
	spraySettings->includeExternalSetting(DEFAULT_OILBRUSH_COLOR, settings);

	simpleBrushSettings = new SettingsProvider(this);
	simpleBrushSettings->setSetting(PenElement::PenWidth, DEFAULT_SPRAY_WIDTH);
	simpleBrushSettings->setSetting(PenElement::PenColor, QColor(DEFAULT_SPRAY_COLOR));
	simpleBrushSettings->includeExternalSetting(DEFAULT_OILBRUSH_COLOR, settings);

	// Toolbox
	toolBox = new ToolBox(this);
	
	DrawingTool *tool = new PenTool(this);
	tool->setSettingsProvider(settings);
	toolBox->addTool(tool);
	connect(tool, SIGNAL(elementSelected(DrawingElement *)), this, SLOT(onElementSelected(DrawingElement *)));

	tool = new SvgTool(this);
	toolBox->addTool(tool);
	connect(tool, SIGNAL(elementSelected(DrawingElement *)), this, SLOT(onElementSelected(DrawingElement *)));

	tool = new ImageTool(this);
	toolBox->addTool(tool);
	connect(tool, SIGNAL(elementSelected(DrawingElement *)), this, SLOT(onElementSelected(DrawingElement *)));

	tool = new TextTool(this);
	tool->setSettingsProvider(settings);
	toolBox->addTool(tool);
	connect(tool, SIGNAL(elementSelected(DrawingElement *)), this, SLOT(onElementSelected(DrawingElement *)));

	tool = new OilBrushTool(this);
	tool->setSettingsProvider(oilBrushSettings);
	toolBox->addTool(tool);
	connect(tool, SIGNAL(elementSelected(DrawingElement *)), this, SLOT(onElementSelected(DrawingElement *)));

	tool = new EraserTool(this);
	tool->setSettingsProvider(settings);
	toolBox->addTool(tool);
	connect(tool, SIGNAL(elementSelected(DrawingElement *)), this, SLOT(onElementSelected(DrawingElement *)));

	tool = new SprayTool(this);
	tool->setSettingsProvider(spraySettings);
	toolBox->addTool(tool);
	connect(tool, SIGNAL(elementSelected(DrawingElement *)), this, SLOT(onElementSelected(DrawingElement *)));

	tool = new SimpleBrushTool(this);
	tool->setSettingsProvider(simpleBrushSettings);
	toolBox->addTool(tool);
	connect(tool, SIGNAL(elementSelected(DrawingElement *)), this, SLOT(onElementSelected(DrawingElement *)));

	tool = new BackgroundTool(this);
	tool->setSettingsProvider(settings);
	toolBox->addTool(tool);

	tool = new EyedropperTool(this);
	toolBox->addTool(tool);

	toolBox->setScene(scene);
	toolBox->setUndoStack(undoStack);

	connect(toolBox, SIGNAL(toolActivated(DrawingTool *)), this, SLOT(onToolActivated(DrawingTool *)));

	toolBox->setCurrentTool(PenElement::Type);
	toolBox->addAlternativeTool(PenElement::Type, EyedropperTool::Type);
	toolBox->getCurrentTool()->setSettingsProvider(settings);	
	
	connect(pasteButton, SIGNAL(released()), toolBox, SLOT(pasteFromClipboard()));

	// Scene again
	scene->setToolBox(toolBox);

	/*/ test
	QGraphicsRectItem *i = new QGraphicsRectItem(QRectF(0, 0, 30, 30));
	i->setFlags(QGraphicsItem::ItemIsSelectable | QGraphicsItem::ItemIsMovable);
	scene->addItem(i);
	i->setBrush(QBrush(Qt::green));

	QGraphicsRectItem *i2 = new QGraphicsRectItem(QRectF(20, 20, 40, 40), i);
	i2->setBrush(QBrush(Qt::blue));*/
}

EditorForm::~EditorForm()
{
	if (undoView != NULL)
		delete undoView;
}

void EditorForm::on_widthEdit_valueChanged(int width)
{
	SettingsProvider *sp = toolBox->getCurrentTool()->getSettingsProvider();

	if (sp == NULL)
		return;

	sp->setSetting(PenElement::PenWidth, (qreal) width);
}

void EditorForm::on_undoViewButton_released()
{
	if (undoView != NULL)
		undoView->show();
}

void EditorForm::on_penToolButton_released()
{
	toolBox->setCurrentTool(PenElement::Type);
}

void EditorForm::on_svgToolButton_released()
{
	toolBox->setCurrentTool(SvgElement::Type);
}

void EditorForm::on_pixmapToolButton_released()
{
	toolBox->setCurrentTool(ImageElement::Type);
}

void EditorForm::on_textToolButton_released()
{
	toolBox->setCurrentTool(TextElement::Type);
}

void EditorForm::on_oilBrushToolButton_released()
{
	toolBox->setCurrentTool(OilBrushElement::Type);
}

void EditorForm::on_eraserToolButton_released()
{
	toolBox->setCurrentTool(EraserElement::Type);
}

void EditorForm::on_sprayToolButton_released()
{
	toolBox->setCurrentTool(SprayElement::Type);
}

void EditorForm::on_simpleBrushToolButton_released()
{
	toolBox->setCurrentTool(SimpleBrushElement::Type);
}

void EditorForm::on_saveMetaButton_released()
{
	QFile file(metaFilenameEdit->text());
	file.open(QIODevice::ReadWrite | QIODevice::Truncate);
	
	QString sceneInfo = scene->getInfoContentAsXml();

	QTextStream out(&file);

	out << sceneInfo;

	file.close();
}
	
void EditorForm::on_savePNGButton_released()
{
	QImage image = scene->renderToImage();
	image.save(filenameEdit->text());	
}

void EditorForm::on_clearButton_released()
{
	scene->clear();
}

void EditorForm::on_fontSizeBox_valueChanged(double value)
{
	settings->setSetting(TextElement::FontSize, value);
}

void EditorForm::on_fontComboBox_currentFontChanged(const QFont &font)
{
	settings->setSetting(TextElement::FontName, font.family());
}

void EditorForm::on_boldButton_toggled(bool checked)
{
	if (checked)
		settings->setSetting(TextElement::FontStyle, 1);
	else
		settings->setSetting(TextElement::FontStyle, 0);
}

void EditorForm::on_paletteManagerButton_clicked()
{
	PaletteManager m(this);

	m.setPalette(colorPalette->getPalette());

	if (m.exec() == QDialog::Accepted)
		colorPalette->setPalette(m.getPalette());
}

void EditorForm::on_bgButton_clicked()
{
	QColor bgColor = settings->getSetting(PenElement::PenColor).value<QColor>();

	settings->setSetting(BACKGROUND_COLOR_SETTING, bgColor);
}
	
void EditorForm::onElementSelected(DrawingElement *element)
{
	elementCache = element;
	cacheLabel->setText(QString::number((long)elementCache));
}

void EditorForm::onToolActivated(DrawingTool *tool)
{
	SettingsProvider *ps = tool->getSettingsProvider();

	if (ps == NULL)
		return;

	qreal width = ps->getSetting(PenElement::PenWidth).value<qreal>();
	widthEdit->setValue(width);
	qreal alpha = ps->getSetting(PenElement::PenTransparency).toDouble();
	QColor color = ps->getSetting(PenElement::PenColor).value<QColor>();

	penSizeButton->setSize(width);
	penAlphaButton->setSize(width);
	penAlphaButton->setAlpha(alpha);
}

void EditorForm::onColorSelected(const QColor &color)
{
	SettingsProvider *ps = toolBox->getCurrentTool()->getSettingsProvider();

	if (ps == NULL)
		return;

	ps->setSetting(PenElement::PenColor, color);
}

void EditorForm::onSizeSelected(qreal size)
{
	SettingsProvider *ps = toolBox->getCurrentTool()->getSettingsProvider();

	if (ps == NULL)
		return;

	ps->setSetting(PenElement::PenWidth, size);
}

void EditorForm::onAlphaSelected(qreal alpha)
{
	SettingsProvider *ps = toolBox->getCurrentTool()->getSettingsProvider();

	if (ps == NULL)
		return;

	ps->setSetting(PenElement::PenTransparency, alpha);
}

}