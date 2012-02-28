#ifndef TEXTELEMENT_H
#define TEXTELEMENT_H

#include "transformableelement.h"

#include <QColor>
#include <QFont>
#include <QFlags>

class QTextControl;
class QTextCharFormat;
class QAction;

namespace Velasquez
{

// Additional zone to move text via frame
#define TEXT_ELEMENT_CONVENIENCE_ZONE		6

#define DEFAULT_TEXT_COLOR					"#000000"
#define DEFAULT_FONT_NAME					"Verdana"
#define DEFAULT_FONT_STYLE					None
#define DEFAULT_FONT_SIZE					16

#define UNDO_MENU_ITEM_TEXT					QT_TRANSLATE_NOOP("@default", "UndoMenuItem")
#define REDO_MENU_ITEM_TEXT					QT_TRANSLATE_NOOP("@default", "RedoMenuItem")
#define CUT_MENU_ITEM_TEXT					QT_TRANSLATE_NOOP("Velasquez::TextElement", "CutMenuItem")
#define COPY_MENU_ITEM_TEXT					QT_TRANSLATE_NOOP("Velasquez::TextElement", "CopyMenuItem")
#define PASTE_MENU_ITEM_TEXT				QT_TRANSLATE_NOOP("Velasquez::TextElement", "PasteMenuItem")
#define SELECT_ALL_MENU_ITEM_TEXT			QT_TRANSLATE_NOOP("Velasquez::TextElement", "SelectAllMenuItem")

// Metainfo tags
#define METAINFO_TEXTCONTENT_TITLE			"Text"
#define METAINFO_TEXTCONTENT_TAGNAME		"Content"

// Rich text scene element
// NOTE: TextElement is largely based on QGraphicsTextItem code and uses the same undocumented (sic!) QTextControl class.
// This is how it is and does its job. In case TT changes/removes QTextControl the code of TextElement should be updated in
// accordance with new QGraphicsTextItem code.
class TextElement : public TransformableElement
{
	Q_OBJECT

public:
	enum {Type = 10003};
	enum Settings {TextColor, FontName = 3, FontStyle, FontSize};
	
	enum StyleFlag {None = 0x00, Bold = 0x01, Italic = 0x02, Underline = 0x04};
	Q_DECLARE_FLAGS(Style, StyleFlag)

public:
	TextElement(QGraphicsItem *parentItem = 0);
	TextElement(const QString &text, bool isHtml = false, QGraphicsItem *parentItem = 0);
	virtual ~TextElement();

	virtual bool isEmpty() const;
	
	inline virtual bool isDeselectable() const { return !isEditing(); };

	virtual void setSetting(qint32 id, const QVariant &value);
	virtual QVariant getSetting(qint32 id) const;
	virtual QList <qint32> getSettingsList() const;
	virtual bool isSettingReversible(qint32 id) const;

	virtual QRectF boundingRect() const;
	virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget = 0);

	virtual void setTransformation(const Transformation &transformation, bool combine = false);

	// Start editing element
	virtual void edit();

	// Universal undo/redo
	virtual void undo();
	virtual void redo();
	
	// Text operations
	bool isModified() const;
	int revision() const;
	bool hasSelection() const;
	
	// Text only undo/redo
	virtual void textUndo();
	virtual void textRedo();

	void setPlainText(const QString &text);
	QString getPlainText() const;

	void setHtml(const QString &html);
	QString getHtml() const;

	void setFont(const QFont &font);
	QFont getFont() const;

	// Settings
	void setTextColor(const QColor &color);
    QColor getTextColor() const;

	void setFontName(const QString &fontName);
	QString getFontName() const;

	void setFontStyle(Style style);
	Style getFontStyle() const;

	void setFontSize(qreal size);
	qreal getFontSize() const;

	QRectF getCursorRect() const;

	// Shared undo and redo actions
	void setCommonUndoAction(QAction *undoAction);
	void setCommonRedoAction(QAction *redoAction);

	// Metainfo
	inline virtual QString getInfoTitle() const { return METAINFO_TEXTCONTENT_TITLE; };
	virtual QStringList getTagNames() const;
	virtual QString getTagContent(const QString &tagName) const;

signals:
	void editingStarted(TextElement *element);
	void editingFinished(TextElement *element);

	void textChanged(TextElement *element);
	void textUndoAdded(TextElement *element);
	
	void cursorMoved(TextElement *element);

protected:
	virtual void mousePressEvent(QGraphicsSceneMouseEvent *event);
	virtual void mouseMoveEvent(QGraphicsSceneMouseEvent *event);
	virtual void mouseReleaseEvent(QGraphicsSceneMouseEvent *event);
	virtual void mouseDoubleClickEvent(QGraphicsSceneMouseEvent *event);

	virtual void keyPressEvent(QKeyEvent *event);
	virtual void keyReleaseEvent(QKeyEvent *event);
    
	virtual void focusInEvent(QFocusEvent *event);
	virtual void focusOutEvent(QFocusEvent *event);

	virtual void dragEnterEvent(QGraphicsSceneDragDropEvent *event);
	virtual void dragLeaveEvent(QGraphicsSceneDragDropEvent *event);
	virtual void dragMoveEvent(QGraphicsSceneDragDropEvent *event);
	virtual void dropEvent(QGraphicsSceneDragDropEvent *event);

	virtual void inputMethodEvent(QInputMethodEvent *event);
	
	virtual void hoverEnterEvent(QGraphicsSceneHoverEvent *event);
	virtual void hoverMoveEvent(QGraphicsSceneHoverEvent *event);
	virtual void hoverLeaveEvent(QGraphicsSceneHoverEvent *event);

	virtual QVariant itemChange(GraphicsItemChange change, const QVariant &value);

	virtual QVariant inputMethodQuery(Qt::InputMethodQuery query) const;

	virtual qint32 getType() const;
	
	virtual QMenu *getContextMenu();

	QAction *createDefaultUndoAction();
	QAction *createDefaultRedoAction();

	inline bool isEditing() const { return editing; };

	void mergeTextFormat(const QTextCharFormat &format);
	QTextCharFormat getTextFormat() const;

	void updateMouseCursor(const QPointF &pos);

private:
	QTextControl *control;
	
	QMenu *editingMenu;
	QAction *undoAction, *redoAction, *pasteAction;
	bool commonUndo, commonRedo;

	QColor currentColor;
	
	QRectF rect;
	QSizeF controlSize;
	
	bool usingDefaultMoving;
	bool editing, tryStartEditing;

	qreal dragZoneSize;

	void init();
	
	QPointF getControlOffset() const;
	
	bool updateDragZoneSize();
	void setRectSize(const QSizeF &controlSize);

	void silentAlignCenter(const QPointF &oldCenter);

	void sendControlEvent(QEvent *event);

	bool isMouseOnEdge(QGraphicsSceneMouseEvent *mouseEvent) const;

	void clearSelection();
	void moveCursorToEnd();

	void startEditControl(QGraphicsSceneMouseEvent *initMouseEvent = NULL);
	void stopEditControl();

private slots:
	void updateRect(const QSizeF &size);

	void onUpdateRequest(const QRectF &rect);
	void onVisibilityRequest(const QRectF &rect);

	void onTextChanged();
	void onUndoCommandAdded();
	
	void onCursorPositionChanged();

};

Q_DECLARE_OPERATORS_FOR_FLAGS(TextElement::Style)

}

#endif // TEXTELEMENT_H
