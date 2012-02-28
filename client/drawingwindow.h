#ifndef DRAWINGWINDOW_H
#define DRAWINGWINDOW_H

#include "uitools.h"

#include <QHash>

#include "ui_drawingwindow.h"

#include "messagetypemix.h"

class QUndoStack;
class QMenu;
class QAction;
class QGraphicsItem;

namespace Velasquez
{
	class DrawingTool;
	class SettingsProvider;
	class EditorScene;
	class ToolBox;
}

namespace MoodBox
{

using namespace Ui;

#define UNDO_KEY_SEQUENCE			Qt::CTRL + Qt::Key_Z
#define REDO_KEY_SEQUENCE			Qt::CTRL + Qt::Key_Y
#define COPY_KEY_SEQUENCE			Qt::CTRL + Qt::Key_C
#define SEND_KEY_SEQUENCE			Qt::CTRL + Qt::Key_Return

#define EMPTY_ICON					":/MoodBox/Resources/transparent.png"

#define MESSAGE_HEADERS_SIZE		10240
#define DRAWING_UNDO_STACK_LIMIT	1000

#define MIN_LENGTH_X_TO_UNDOCK  40
#define MIN_LENGTH_X_TO_DOCK    39
#define MIN_LENGTH_Y_TO_UNDOCK  9
#define MIN_LENGTH_Y_TO_DOCK    8



class ToolSettingsBar;
class MessageFile;

// Scene collection item
class EditorSceneInfo
{
public:
	EditorSceneInfo();
	EditorSceneInfo(QWidget *parent);

	void clear();

	Velasquez::EditorScene *editorScene;
	QUndoStack *undoStack;
	qint32 worthSendItems;
};

// Chat/MoodBox/Group chat drawing window
class DrawingWindow : public QWidget, public MessageTypeMix, public DrawingWindowClass
{
	Q_OBJECT

public:
	DrawingWindow(QWidget *parent = 0);
	virtual ~DrawingWindow();

	virtual void setRecipient(MessageType::MessageTypeEnum type, qint32 recipientId = -1);

	bool getUnDockedState() const { return isDrawingWindowUndocked; };
	void setUnDockedState(bool isUndocked) { isDrawingWindowUndocked = isUndocked; updateToolbarFrameStyle(); };

	bool hasDrawing() const;
	bool canSend() const;

	virtual qint32 getMaxMessageSize() const;

	virtual void updatePosition(const QWidget *topWidget);

signals:
	void clipartWindowRequest();
	void messageSent();
	void replyChanged();

public slots:
	void clear();
	void reset();

	void addImage(const QImage &image);
	void addReply(const QImage &image);

protected:
	enum ReplyMode { NoReply, NewAdded, Changed };	

	QHash<qint32, EditorSceneInfo> scenesInfo;
	EditorSceneInfo currentSceneInfo;

	Velasquez::ToolBox *toolBox;

	bool sendableRecipient;

	bool isDrawingWindowUndocked;
	bool isMousePressed;
	// Dragging & position
	QPoint dragPosition;
	
	bool addingReply;
	ReplyMode replyMode;

	QHash <qint32, ToolSettingsBar *> toolSettingsBars;
	qint32 defaultTool;

	QMenu *contextMenu;

	QAction *undoAction, *redoAction, *clearAction, *sendAction, *copyAction, *pasteAction, *saveAction;

	void createTools();
	void createActions();
	
	void setupTool(Velasquez::DrawingTool *tool, ToolSettingsBar *settingsBar);

	void selectTool(qint32 elementType, bool reinit = false);
	void switchToolSettingBar(qint32 newElementType);

	void selectPicturesTool(bool showClipart, bool reinit = false);

	virtual bool isSendableItem(const QGraphicsItem *item) const;
	virtual void updateSendableRecipient();

	virtual void updateSendAndClearButtons();

	virtual void sendMessage();

	virtual void contextMenuEvent(QContextMenuEvent *event);

	EditorSceneInfo addScene(qint32 id);
	void setCurrentScene(qint32 id);
	void bindToolBoxWithScene();
	void clearScenes();

	void updateToolbarFrameStyle();

	virtual void mousePressEvent(QMouseEvent *event);
	virtual void mouseMoveEvent(QMouseEvent *event);
	virtual void mouseReleaseEvent(QMouseEvent *event);

	virtual void resizeEvent(QResizeEvent *event);

private slots:
	void on_penToolButton_clicked();
	void on_simplebrushToolButton_clicked();
	void on_oilbrushToolButton_clicked();
	void on_sprayToolButton_clicked();
	void on_eraserToolButton_clicked();
	void on_textToolButton_clicked();
	void on_picturesToolButton_clicked();

	void on_sendButton_clicked();
	
	void onItemAdded(QGraphicsItem *item);
	void onItemRemoved(QGraphicsItem *item);

	void onSceneCleared();
	void onSaveScene();
	void onSceneColorChanged();

	void onUndoChanged();

	void onContactListChanged();
	void onContactAuthorizationChanged(qint32 id);
	void onUserOnlineChanged();
};

}

#endif // DRAWINGWINDOW_H
