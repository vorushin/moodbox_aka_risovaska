#include "drawingwindow.h"

#include <QMenu>
#include <QAction>
#include <QKeyEvent>
#include <QUndoStack>
#include <QMessageBox>
#include <QFileDialog>
#include <QFileInfo>
#include <QBuffer>
#include <QByteArray>

#include "peopleinfomanager.h"
#include "editorscene.h"
#include "toolbox.h"

#include "pentool.h"
#include "oilbrushtool.h"
#include "simplebrushtool.h"
#include "spraytool.h"
#include "erasertool.h"
#include "svgtool.h"
#include "imagetool.h"
#include "texttool.h"
#include "backgroundtool.h"
#include "eyedroppertool.h"

#include "penelement.h"
#include "oilbrushelement.h"
#include "simplebrushelement.h"
#include "sprayelement.h"
#include "eraserelement.h"
#include "svgelement.h"
#include "imageelement.h"
#include "textelement.h"
#include "backgroundelement.h"

#include "vcommon.h"
#include "common.h"

#include "settingsprovider.h"
#include "erasersettingsbar.h"
#include "textsettingsbar.h"

#include "messagemanager.h"
#include "messagefile.h"

#include "international.h"
#include "imagetools.h"

#include "testtools.h"
#include "debug.h"

#ifdef Q_WS_MAC
#include "mactools.h"
#endif
#ifdef Q_WS_X11
#include "linuxtools.h"
#endif


using namespace Velasquez;

namespace MoodBox
{

// EditorSceneInfo class
EditorSceneInfo::EditorSceneInfo() : editorScene(NULL), undoStack(NULL), worthSendItems(0)
{
}

EditorSceneInfo::EditorSceneInfo(QWidget *parent)
{
	editorScene = new EditorScene(parent);

	undoStack = new QUndoStack(parent);
	undoStack->setUndoLimit(DRAWING_UNDO_STACK_LIMIT);

	worthSendItems = 0;
}

void EditorSceneInfo::clear()
{
	if (undoStack != NULL)
		delete undoStack;

	if (editorScene != NULL)
		delete editorScene;
}

// DrawingWindow class
DrawingWindow::DrawingWindow(QWidget *parent)
	: QWidget(parent, Qt::FramelessWindowHint | Qt::Tool), toolBox(NULL),
	  sendableRecipient(false), isDrawingWindowUndocked(false), addingReply(false), replyMode(NoReply), defaultTool(-1), 
	  contextMenu(NULL),  undoAction(NULL), redoAction(NULL)
	  
{
#ifdef Q_WS_MAC
    MacTools::addWindow(this);
#endif
#ifdef Q_WS_X11
    LinuxTools::addWindow(this);
#endif

	TimeMeasure t("DrawingWindow");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	updateToolbarFrameStyle();

	createTools();
	createActions();

	sendButton->setEnabled(false);

	connect(INFOMANAGER, SIGNAL(contactListChanged()), this, SLOT(onContactListChanged()));
	connect(INFOMANAGER, SIGNAL(contactAuthorizationChanged(qint32, AuthorizationState::AuthorizationStateEnum)), this, SLOT(onContactAuthorizationChanged(qint32)));
	connect(INFOMANAGER, SIGNAL(isOnlineChanged()), this, SLOT(onUserOnlineChanged()));
}

DrawingWindow::~DrawingWindow()
{
	clearScenes();
}

void DrawingWindow::setRecipient(MessageType::MessageTypeEnum type, qint32 recipientId)
{
	if (getMessageType() == type && getRecipient() == recipientId && isMessageTypeInitialized())
		return;

	scenesInfo[getRecipient()] = currentSceneInfo;

	MessageTypeMix::setRecipient(type, recipientId);
	
	setCurrentScene(recipientId);
}

bool DrawingWindow::hasDrawing() const
{
	bool hasNonWhiteBackground = false;

	if (currentSceneInfo.editorScene != NULL && currentSceneInfo.editorScene->hasBackground())
		hasNonWhiteBackground = currentSceneInfo.editorScene->getBackgroundColor() != QColor(DEFAULT_BACKGROUND_COLOR);

	return currentSceneInfo.worthSendItems > 0 || hasNonWhiteBackground;
}

bool DrawingWindow::canSend() const
{
	if (!hasDrawing())
		return false;

	// Check sandbox
	if (getRecipient() == INFOMANAGER->getUserAccount().getId())
		return true;

	if (!INFOMANAGER->isUserOnline())
		return false;

	return sendableRecipient;
}

qint32 DrawingWindow::getMaxMessageSize() const
{
	return SERVER->getMaxRequestSize() - MESSAGE_HEADERS_SIZE;
}

void DrawingWindow::updatePosition(const QWidget *topWidget)
{
	if (!isDrawingWindowUndocked)
	{
		QPoint newPos(topWidget->geometry().x(), topWidget->geometry().y() + topWidget->geometry().height());

		if (pos() == newPos)
			return;
		else
			move(newPos);
	}
}

void DrawingWindow::clear()
{	
	toolBox->deleteAllElements();
}

void DrawingWindow::reset()
{
	replyMode = NoReply;

	foreach (ToolSettingsBar *toolBar, toolSettingsBars.values())
		if (toolBar != NULL)
			toolBar->reset();

	clearScenes();

	selectTool(defaultTool);

	switch (defaultTool)
	{
		case PenElement::Type:
			penToolButton->setChecked(true);
			break;
	
		case SimpleBrushElement::Type:
			simplebrushToolButton->setChecked(true);
			break;

		case OilBrushElement::Type:
			oilbrushToolButton->setChecked(true);
			break;

		case SprayElement::Type:
			sprayToolButton->setChecked(true);
			break;

		case TextElement::Type:
			textToolButton->setChecked(true);
			break;
	}

	toolBox->getTool(BackgroundElement::Type)->getSettingsProvider()->setSetting(BACKGROUND_COLOR_SETTING, DEFAULT_BACKGROUND_COLOR);
	
	unInitializeMessageType();
}

void DrawingWindow::addImage(const QImage &image)
{
	ImageTool *imageTool = (ImageTool *)toolBox->getTool(ImageElement::Type);
	imageTool->createElement(image, QPointF(0, 0));
}

void DrawingWindow::addReply(const QImage &image)
{
	// Make sure we will not emit any strange signals
	if (replyMode == NewAdded)
		replyMode = NoReply;

	clear();
	
	addingReply = true;
	addImage(image);
	replyMode = NewAdded;
}

void DrawingWindow::createTools()
{
	// Make toolbox first
	toolBox = new ToolBox(this);
	connect(toolBox, SIGNAL(copyRequest()), toolBox, SLOT(copyImageToClipboard()));
	connect(toolBox, SIGNAL(pasteRequest()), toolBox, SLOT(pasteFromClipboard()));

	// Setup tools
	BrushSettingsBar *brushSettings = new BrushSettingsBar(toolSettingsHost);
	setupTool(new PenTool(this), brushSettings);
	setupTool(new SimpleBrushTool(this), new BrushSettingsBar(toolSettingsHost));
	setupTool(new OilBrushTool(this), new BrushSettingsBar(toolSettingsHost));
	setupTool(new SprayTool(this), new BrushSettingsBar(toolSettingsHost));
	setupTool(new EraserTool(this), new EraserSettingsBar(toolSettingsHost));
	
	setupTool(new TextTool(this), new TextSettingsBar(toolSettingsHost));

	setupTool(new SvgTool(this), NULL);
	setupTool(new ImageTool(this), NULL);
	setupTool(new BackgroundTool(this), NULL);

	EyedropperTool *eyedropper = new EyedropperTool(this);
	setupTool(eyedropper, NULL);

	// Alternatives
	toolBox->addAlternativeTool(PenElement::Type, EyedropperTool::Type);
	toolBox->addAlternativeTool(OilBrushElement::Type, EyedropperTool::Type);
	toolBox->addAlternativeTool(SimpleBrushElement::Type, EyedropperTool::Type);
	toolBox->addAlternativeTool(SprayElement::Type, EyedropperTool::Type);
	toolBox->addAlternativeTool(EraserElement::Type, EyedropperTool::Type);

	// Connect color settings
	SettingsProvider *commonColorProvider = toolBox->getTool(PenElement::Type)->getSettingsProvider();
	toolBox->getTool(SimpleBrushElement::Type)->getSettingsProvider()->includeExternalSetting(SHARED_COLORINDEX_SETTING, commonColorProvider);
	toolBox->getTool(OilBrushElement::Type)->getSettingsProvider()->includeExternalSetting(SHARED_COLORINDEX_SETTING, commonColorProvider);
	toolBox->getTool(SprayElement::Type)->getSettingsProvider()->includeExternalSetting(SHARED_COLORINDEX_SETTING, commonColorProvider);
	toolBox->getTool(TextElement::Type)->getSettingsProvider()->includeExternalSetting(SHARED_COLORINDEX_SETTING, commonColorProvider);

	toolBox->getTool(EraserElement::Type)->getSettingsProvider()->includeExternalSetting(BACKGROUND_COLOR_SETTING, toolBox->getTool(BackgroundElement::Type)->getSettingsProvider());

	// Eyedropper
	// Needs to be connected only once, because the changes will be propagated
	connect(eyedropper, SIGNAL(colorPicked(const QColor &)), brushSettings, SLOT(changeSelectedColor(const QColor &)));
}

void DrawingWindow::createActions()
{
	clearAction = new QAction(clearButton->text(), this);
	connect(clearAction, SIGNAL(triggered()), this, SLOT(clear()));
	clearAction->setToolTip(clearButton->toolTip());
	clearButton->setDefaultAction(clearAction);

	copyAction = new QAction(tr(COPY_ACTION_TEXT), this);
	connect(copyAction, SIGNAL(triggered()), toolBox, SLOT(copyImageToClipboard()));
	copyAction->setShortcut(COPY_KEY_SEQUENCE);

	pasteAction = new QAction(tr(PASTE_ACTION_TEXT), this);
	pasteAction->setShortcut(QKeySequence::Paste);
	connect(pasteAction, SIGNAL(triggered()), toolBox, SLOT(pasteFromClipboard()));

	saveAction = new QAction(tr(SAVE_ACTION_TEXT), this);
	connect(saveAction, SIGNAL(triggered()), this, SLOT(onSaveScene()));

	sendAction = new QAction(sendButton->text(), this);
	connect(sendAction, SIGNAL(triggered()), this, SLOT(on_sendButton_clicked()));
	sendAction->setShortcut(SEND_KEY_SEQUENCE);

	addAction(sendAction);
}

void DrawingWindow::setupTool(DrawingTool *tool, ToolSettingsBar *settingsBar)
{
	toolBox->addTool(tool);

	toolSettingsBars.insert(tool->getElementType(), settingsBar);

	if (settingsBar != NULL)
	{
		settingsBar->hide();
		tool->setSettingsProvider(settingsBar->getSettingsProvider());
	}
	else
		tool->setSettingsProvider(new SettingsProvider(tool));
}

void DrawingWindow::selectTool(qint32 elementType, bool reinit)
{
	switchToolSettingBar(elementType);

	toolBox->setCurrentTool(elementType, reinit);

	if (defaultTool < 0)
		defaultTool = elementType;
}

void DrawingWindow::switchToolSettingBar(qint32 newElementType)
{
	Velasquez::DrawingTool *tool = toolBox->getCurrentTool();
	ToolSettingsBar *currentSettingsBar = NULL;

	if (tool != NULL)
		currentSettingsBar = toolSettingsBars[tool->getElementType()];

	if (currentSettingsBar != NULL)
		currentSettingsBar->hide();

	currentSettingsBar = toolSettingsBars[newElementType];

	if (currentSettingsBar != NULL)
		currentSettingsBar->show();
}

void DrawingWindow::selectPicturesTool(bool showClipart, bool reinit)
{
	if (!picturesToolButton->isChecked())
		picturesToolButton->setChecked(true);

	selectTool(ImageElement::Type, reinit);

	if (showClipart)
		emit clipartWindowRequest();
}

bool DrawingWindow::isSendableItem(const QGraphicsItem *item) const
{
	if (toolBox->getTool(item->type()) == NULL)
		return false;

	if (item->type() == BackgroundElement::Type)
		return false;

	return true;
}

void DrawingWindow::updateSendableRecipient()
{
	bool newSendableRecipient;

	// Enable send messages to all in contact list (and for not authorized contacts)
	newSendableRecipient = true;

	// Old code (enable sending messages only for "all friends" and for authorized contacts)
	//if (getMessageType() == MessageType::Friends)
	//{
	//	newSendableRecipient = true;
	//}
	//else
	//{
	//	newSendableRecipient = (INFOMANAGER->getContactAuthorization(getRecipient()) == AuthorizationState::Authorized);
	//}

	bool wasChanged = newSendableRecipient != sendableRecipient;

	if (!wasChanged)
		return;

	sendableRecipient = newSendableRecipient;
	updateSendAndClearButtons();
}

void DrawingWindow::updateSendAndClearButtons()
{
	clearButton->setEnabled(hasDrawing());
	sendButton->setEnabled(canSend());	

	// Uncomment to debug
	//QDEBUG("DrawingWindow::updateSendAndClearButtons, can send -" << canSend() << " has drawing -" << hasDrawing() << " sendable recipient -" << sendableRecipient);
}

void DrawingWindow::sendMessage()
{
	if (!canSend())
		return;

	QImage image = currentSceneInfo.editorScene->renderToImage();
	
	// TODO: send normal metadata
	//QString info = currentSceneInfo.editorScene->getInfoContentAsXml();
	QByteArray content;
	QString format;
	
	ImageTools::getSmallestImageContent(image, content, format);

	// Ignore the rest of metadata
	QString info = format;

	MessageFile *file = new MessageFile();

	file->setRecipient(getMessageType(), getRecipient());
	file->setInfo(info);
	file->setPublic(INFOMANAGER->getUserAccount().getAllowPublishing());
	file->setPreview(content);

	emit messageSent();

	MESSAGEMANAGER->sendMessage(file);
	
	currentSceneInfo.editorScene->clear();
}

void DrawingWindow::contextMenuEvent(QContextMenuEvent *event)
{
	QRect viewRect = drawingView->rect();

	if (!viewRect.contains(event->pos()))
		return;

	if (contextMenu == NULL)
	{
		contextMenu = new QMenu(this);

		contextMenu->addAction(copyAction);
		contextMenu->addAction(pasteAction);
		contextMenu->addSeparator();
		contextMenu->addAction(saveAction);
	}

	copyAction->setEnabled(hasDrawing());
	pasteAction->setEnabled(toolBox->canPasteFromClipboard());
	saveAction->setEnabled(hasDrawing());

	contextMenu->exec(mapToGlobal(event->pos()));	
}

EditorSceneInfo DrawingWindow::addScene(qint32 id)
{
	EditorSceneInfo currentSceneInfo = EditorSceneInfo(this);
	
	scenesInfo.insert(id, currentSceneInfo);

	return currentSceneInfo;
}

void DrawingWindow::setCurrentScene(qint32 id)
{
	// Disconnect currrent scene
	if (currentSceneInfo.editorScene != NULL)
	{
		disconnect(currentSceneInfo.editorScene, SIGNAL(itemAdded(QGraphicsItem *)), this, SLOT(onItemAdded(QGraphicsItem *)));
		disconnect(currentSceneInfo.editorScene, SIGNAL(itemRemoved(QGraphicsItem *)), this, SLOT(onItemRemoved(QGraphicsItem *)));
		disconnect(currentSceneInfo.editorScene, SIGNAL(cleared()), this, SLOT(onSceneCleared()));
		disconnect(currentSceneInfo.editorScene, SIGNAL(backgroundColorChanged(const QColor &)), this, SLOT(onSceneColorChanged()));
	}

	if (currentSceneInfo.undoStack != NULL)
	{
		disconnect(currentSceneInfo.undoStack, SIGNAL(indexChanged(int)), this, SLOT(onUndoChanged()));	
	}

	// Set new current scene
	currentSceneInfo = scenesInfo.value(id);

	if (currentSceneInfo.editorScene == NULL)
		currentSceneInfo = addScene(id);

	drawingView->setScene(currentSceneInfo.editorScene);

	// Connect new scene and undo stack
	connect(currentSceneInfo.editorScene, SIGNAL(itemAdded(QGraphicsItem *)), this, SLOT(onItemAdded(QGraphicsItem *)));
	connect(currentSceneInfo.editorScene, SIGNAL(itemRemoved(QGraphicsItem *)), this, SLOT(onItemRemoved(QGraphicsItem *)));
	connect(currentSceneInfo.editorScene, SIGNAL(cleared()), this, SLOT(onSceneCleared()));
	connect(currentSceneInfo.editorScene, SIGNAL(backgroundColorChanged(const QColor &)), this, SLOT(onSceneColorChanged()));

	connect(currentSceneInfo.undoStack, SIGNAL(indexChanged(int)), this, SLOT(onUndoChanged()));

	// Update undo actions
	QAction *oldUndoAction = undoAction, *oldRedoAction = redoAction;

	undoAction = currentSceneInfo.undoStack->createUndoAction(this);
	undoAction->setToolTip(undoButton->toolTip());
	undoAction->setIcon(QIcon(EMPTY_ICON));
    undoAction->setShortcut(UNDO_KEY_SEQUENCE);

	redoAction = currentSceneInfo.undoStack->createRedoAction(this);
	redoAction->setToolTip(redoButton->toolTip());
	redoAction->setIcon(QIcon(EMPTY_ICON));
    redoAction->setShortcut(REDO_KEY_SEQUENCE);

	if (oldUndoAction != NULL)
	{
		undoButton->removeAction(oldUndoAction);
		oldUndoAction->deleteLater();
	}
	if (oldRedoAction != NULL)
	{
		redoButton->removeAction(oldRedoAction);
		oldRedoAction->deleteLater();
	}

	undoButton->setDefaultAction(undoAction);
	redoButton->setDefaultAction(redoAction);

	if (toolBox != NULL)
		bindToolBoxWithScene();

	updateSendableRecipient();
	updateSendAndClearButtons();
}

void DrawingWindow::bindToolBoxWithScene()
{
	toolBox->setScene(currentSceneInfo.editorScene);
	toolBox->setUndoStack(currentSceneInfo.undoStack);

	currentSceneInfo.editorScene->setToolBox(toolBox);
	
	// Select current tool
	if (penToolButton->isChecked())
		selectTool(PenElement::Type, true);
	
	if (simplebrushToolButton->isChecked())
		selectTool(SimpleBrushElement::Type, true);

	if (oilbrushToolButton->isChecked())
		selectTool(OilBrushElement::Type, true);

	if (sprayToolButton->isChecked())
		selectTool(SprayElement::Type, true);

	if (textToolButton->isChecked())
		selectTool(TextElement::Type, true);

	if (picturesToolButton->isChecked())
		selectPicturesTool(false, true);
}

void DrawingWindow::clearScenes()
{
	drawingView->setScene(NULL);
	toolBox->setScene(NULL);
	toolBox->setUndoStack(NULL);

	currentSceneInfo = EditorSceneInfo();

	foreach (EditorSceneInfo item, scenesInfo.values())
		item.clear();

	scenesInfo.clear();
}

void DrawingWindow::updateToolbarFrameStyle()
{
	if (isDrawingWindowUndocked)
	{
		toolbarFrame->setStyleSheet(QString());
	}
	else
	{
		toolbarFrame->setStyleSheet("#toolbarFrame{border-width: 0px; border-image: url(:/MoodBox/Resources/shadow_drawing_window.png) 0 0 0 0 stretch, stretch;}");
	}
}

void DrawingWindow::mousePressEvent(QMouseEvent *event)
{
	if (event->button() == Qt::LeftButton)
	{
		dragPosition = event->globalPos() - frameGeometry().topLeft();

		// Only if we press on toolbar frame
		if (toolbarFrame->rect().contains(dragPosition))
		{
			isMousePressed = true;
			event->accept();

			return;
		}
	}

	QWidget::mousePressEvent(event);
}

void DrawingWindow::mouseMoveEvent(QMouseEvent *event)
{
	if (event->buttons() & Qt::LeftButton && isMousePressed)
	{
		if (isDrawingWindowUndocked && 
			abs(parentWidget()->geometry().x() - geometry().x()) <= MIN_LENGTH_X_TO_DOCK &&
			abs(parentWidget()->geometry().y() + parentWidget()->geometry().height() - geometry().y()) <= MIN_LENGTH_Y_TO_DOCK)
		{
			isDrawingWindowUndocked = false;

			move(parentWidget()->geometry().x(), parentWidget()->geometry().y() + parentWidget()->geometry().height());
			updateToolbarFrameStyle();

			dragPosition = event->globalPos() - frameGeometry().topLeft();

			event->accept();
			return;
		}
		else
		{
			if (isDrawingWindowUndocked ||
			 (abs(dragPosition.x() - event->globalPos().x() + frameGeometry().topLeft().x()) >= MIN_LENGTH_X_TO_UNDOCK ||
			  abs(dragPosition.y() - event->globalPos().y() + frameGeometry().topLeft().y()) >= MIN_LENGTH_Y_TO_UNDOCK))
			{	
				isDrawingWindowUndocked = true;

				move(event->globalPos() - dragPosition);
				updateToolbarFrameStyle();

				event->accept();
				return;
			}
		}
	}

	QWidget::mouseMoveEvent(event);
}

void DrawingWindow::mouseReleaseEvent(QMouseEvent *event)
{
	if (event->button() == Qt::LeftButton)
	{
		isMousePressed = false;
	}

	QWidget::mouseReleaseEvent(event);
}

void DrawingWindow::resizeEvent(QResizeEvent *event)
{
	setMask(WidgetMaskCreator::createMask(WidgetMaskCreator::WidgetMaskSmall, width(), height()));

	QWidget::resizeEvent(event);
}

void DrawingWindow::on_penToolButton_clicked()
{
	selectTool(PenElement::Type);
}

void DrawingWindow::on_simplebrushToolButton_clicked()
{
	selectTool(SimpleBrushElement::Type);
}

void DrawingWindow::on_oilbrushToolButton_clicked()
{
	selectTool(OilBrushElement::Type);
}

void DrawingWindow::on_sprayToolButton_clicked()
{
	selectTool(SprayElement::Type);
}

void DrawingWindow::on_eraserToolButton_clicked()
{
	selectTool(EraserElement::Type);
}

void DrawingWindow::on_textToolButton_clicked()
{
	selectTool(TextElement::Type);
}

void DrawingWindow::on_picturesToolButton_clicked()
{
	selectPicturesTool(true);
}

void DrawingWindow::on_sendButton_clicked()
{
	sendMessage();
}

void DrawingWindow::onItemAdded(QGraphicsItem *item)
{
	if (isSendableItem(item))
	{
		currentSceneInfo.worthSendItems++;

		if (currentSceneInfo.worthSendItems == 1)
			updateSendAndClearButtons();

		// Check do we need to activate the clipart
		bool switchToPictures = false;

		if (item->type() == SvgElement::Type)
			switchToPictures = true;
		else
			if (item->type() == ImageElement::Type)
			{
				if (addingReply)
					addingReply = false;
				else
					switchToPictures = true;
			}
		
		if (switchToPictures)
			selectPicturesTool(false);
	}
}

void DrawingWindow::onItemRemoved(QGraphicsItem *item)
{
	if (isSendableItem(item))
	{
		currentSceneInfo.worthSendItems--;

		if (currentSceneInfo.worthSendItems == 0)
			updateSendAndClearButtons();
	}
}

void DrawingWindow::onSceneCleared()
{
	currentSceneInfo.worthSendItems = 0;
	updateSendAndClearButtons();
}

void DrawingWindow::onSaveScene()
{
	QImage image = currentSceneInfo.editorScene->renderToImage();

	ImageTools::saveImageToFile(image, this);
}

void DrawingWindow::onSceneColorChanged()
{
	updateSendAndClearButtons();
}

void DrawingWindow::onUndoChanged()
{
	if (replyMode == NewAdded)
	{
		emit replyChanged();

		replyMode = Changed;
	}
}

void DrawingWindow::onContactListChanged()
{
	updateSendableRecipient();
}

void DrawingWindow::onContactAuthorizationChanged(qint32 id)
{
	if (getMessageType() == MessageType::Private && getRecipient() == id)
		updateSendableRecipient();
}

void DrawingWindow::onUserOnlineChanged()
{
	updateSendAndClearButtons();
}

}
