#include "textelement.h"

#ifdef Q_WS_WIN
#include <gui/text/qtextcontrol_p.h>
#include <gui/text/qtextdocumentlayout_p.h>
#else 
#include <qtextcontrol_p.h>
#include <qtextdocumentlayout_p.h>
#endif

#include <QPainter>
#include <QStyleOptionGraphicsItem>
#include <QGraphicsSceneMouseEvent>
#include <QTextCharFormat>
#include <QFont>

#include "vcommon.h"
#include "debug.h"

namespace Velasquez
{

TextElement::TextElement(QGraphicsItem *parentItem)
	: TransformableElement(parentItem), editingMenu(NULL), pasteAction(NULL),
	  commonUndo(false), commonRedo(false), usingDefaultMoving(false), editing(false), tryStartEditing(false)
{
	init();
}

TextElement::TextElement(const QString &text, bool isHtml, QGraphicsItem *parentItem)
	: TransformableElement(parentItem), editingMenu(NULL), pasteAction(NULL),
	  commonUndo(false), commonRedo(false), usingDefaultMoving(false), editing(false), tryStartEditing(false)
{
	init();
	
	if (!text.isEmpty())
		if (!isHtml)
			setPlainText(text);
		else
			setHtml(text);
}

TextElement::~TextElement()
{
	if (editingMenu != NULL)
		delete editingMenu;
}

bool TextElement::isEmpty() const
{
	return control->document()->isEmpty();
}

void TextElement::setSetting(qint32 id, const QVariant &value)
{
	switch (id)
	{
		case TextColor:
			if (!value.canConvert<QColor>())
				return;

			setTextColor(value.value<QColor>());

			break;

		case FontName:
			if (!value.canConvert<QString>())
				return;

			setFontName(value.toString());

			break;

		case FontStyle:
			if (!value.canConvert<qint32>())
				return;

			setFontStyle((Style) value.toInt());

			break;

		case FontSize:
			if (!value.canConvert<qreal>())
				return;
			
			setFontSize(value.value<qreal>());
			update();

			break;
	}
}

QVariant TextElement::getSetting(qint32 id) const 
{
	switch (id)
	{
		case TextColor: return getTextColor();
		case FontName: return getFontName();
		case FontStyle: return (qint32) getFontStyle();
		case FontSize: return getFontSize();
	}

	return QVariant(); 
}

QList <qint32> TextElement::getSettingsList() const
{ 
	QList<qint32> settings;

	settings << TextColor << FontName << FontStyle << FontSize;

	return settings;
}

bool TextElement::isSettingReversible(qint32 id) const
{
	Q_UNUSED(id)

	// Changes in settings undo via internal undo commands
	return false;
}

QRectF TextElement::boundingRect() const
{
	return rect;
}

void TextElement::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
	Q_UNUSED(widget);

	painter->save();
	QRectF r = option->exposedRect;

	painter->translate(-getControlOffset());

	r.translate(getControlOffset());

	QTextDocument *doc = control->document();
    QTextDocumentLayout *layout = qobject_cast<QTextDocumentLayout *>(doc->documentLayout());

    // the layout might need to expand the root frame to
    // the viewport if NoWrap is set
    if (layout)
		layout->setViewport(rect);

    control->drawContents(painter, r);

    if (layout)
		layout->setViewport(QRect());
   
    painter->restore();

	TransformableElement::paint(painter, option, widget);
}

void TextElement::setTransformation(const Transformation &transformation, bool combine)
{
	QPointF oldCenter = mapToParent(boundingRect().center());

	TransformableElement::setTransformation(transformation, combine);

	// We need to decrease size of drop zone for big-scale objects
	if (updateDragZoneSize())
	{
		setRectSize(controlSize);
		silentAlignCenter(oldCenter);
	}
}

void TextElement::edit()
{
	if (isEditing())
		return;

	startEditControl();

	if (!isSelected())
		setSelected(true);
	
	if (!hasFocus())
		setFocus(Qt::OtherFocusReason);
}

void TextElement::undo()
{
	if (undoAction->isEnabled())
		undoAction->trigger();
}

void TextElement::redo()
{
	if (redoAction->isEnabled())
		redoAction->trigger();
}

bool TextElement::isModified() const
{
	return control->document()->isModified();
}

int TextElement::revision() const
{
	return control->document()->revision();
}

bool TextElement::hasSelection() const
{
	return control->textCursor().hasSelection();
}

void TextElement::textUndo()
{
	control->document()->undo();
}

void TextElement::textRedo()
{
	control->document()->redo();
}

void TextElement::setPlainText(const QString &text)
{
	control->setPlainText(text);
	moveCursorToEnd();
}

QString TextElement::getPlainText() const
{
	return control->toPlainText();
}

void TextElement::setHtml(const QString &text)
{
    control->setHtml(text);
	moveCursorToEnd();
}

QString TextElement::getHtml() const
{
	return control->toHtml();
}

void TextElement::setFont(const QFont &font)
{
	if (isEmpty())
	{
		control->document()->setDefaultFont(font);
	}
	else
	{
		QTextCharFormat format;
		format.setFont(font);
		mergeTextFormat(format);
	}
}

QFont TextElement::getFont() const
{
	if (isEmpty())
	{
		return control->document()->defaultFont();
	}
	else
	{
		QTextCharFormat format = getTextFormat();
		return format.font();
	}
}

void TextElement::setTextColor(const QColor &color)
{	
	currentColor = color;

	QTextCharFormat format;
	format.setForeground(color);
	mergeTextFormat(format);
}

QColor TextElement::getTextColor() const
{
	if (isEmpty())
	{
		return control->palette().color(QPalette::Text);
	}
	else
	{
		QTextCharFormat format = getTextFormat();
		return format.foreground().color();
	}
}

void TextElement::setFontName(const QString &fontName)
{
	if (isEmpty())
	{
		QFont defaultFont = control->document()->defaultFont();
		defaultFont.setFamily(fontName);
		control->document()->setDefaultFont(defaultFont);
	}
	else
	{
		QTextCharFormat format;
		format.setFontFamily(fontName);
		mergeTextFormat(format);
	}
}

QString TextElement::getFontName() const
{
	if (isEmpty())
	{
		return control->document()->defaultFont().family();
	}
	else
	{
		QTextCharFormat format = getTextFormat();
		return format.fontFamily();
	}
}

void TextElement::setFontStyle(Style style)
{
	if (isEmpty())
	{
		QFont defaultFont = control->document()->defaultFont();

		defaultFont.setWeight( (style & Bold) ? QFont::Bold : QFont::Normal);
		defaultFont.setItalic(style & Italic);
		defaultFont.setUnderline(style & Underline);

		control->document()->setDefaultFont(defaultFont);
	}
	else
	{
		QTextCharFormat format;
		format.setFontWeight( (style & Bold) ? QFont::Bold : QFont::Normal);
		format.setFontItalic(style & Italic);
		format.setFontUnderline(style & Underline);
		mergeTextFormat(format);
	}
}

TextElement::Style TextElement::getFontStyle() const
{
	Style style = None;

	if (isEmpty())
	{
		QFont defaultFont = control->document()->defaultFont();

		if (defaultFont.weight() == QFont::Bold)
			style |= Bold;
	
		if (defaultFont.italic())
			style |= Italic;
	
		if (defaultFont.underline())
			style |= Underline;
	}
	else
	{
		QTextCharFormat format = getTextFormat();
		
		if (format.fontWeight() == QFont::Bold)
			style |= Bold;
		
		if (format.fontItalic())
			style |= Italic;
		
		if (format.fontUnderline())
			style |= Underline;
	}

	return style;
}

void TextElement::setFontSize(qreal size)
{
	if (isEmpty())
	{
		QFont defaultFont = control->document()->defaultFont();
		defaultFont.setPointSize(size);
		control->document()->setDefaultFont(defaultFont);
	}
	else
	{
		QTextCharFormat format;
		format.setFontPointSize(size);
		mergeTextFormat(format);
	}
}

qreal TextElement::getFontSize() const
{
	QTextCharFormat format = getTextFormat();
	qreal size = format.fontPointSize();

	return (size == 0) ? control->document()->defaultFont().pointSize() : size;
}

QRectF TextElement::getCursorRect() const
{
	QRectF cursorRect = control->cursorRect();
	cursorRect.translate(-getControlOffset());

	return cursorRect;
}

void TextElement::setCommonUndoAction(QAction *undoAction)
{
	if (this->undoAction == undoAction)
		return;

	// Setting new?
	if (undoAction != NULL)
	{
		// Clear local, if any	
		if (this->undoAction != NULL)
		{
			if (editingMenu != NULL)
				editingMenu->removeAction(this->undoAction);
			
			if (!commonUndo)
				// Local can be just deleted
				delete this->undoAction;
		}

		this->undoAction = undoAction;
		commonUndo = true;
	}
	else
	{
		// Nothing to clear
		if (!commonUndo)
			return;

		// Clear common, if any
		if (this->undoAction != NULL)
		{
			if (editingMenu != NULL)
				editingMenu->removeAction(this->undoAction);
		}

		this->undoAction = createDefaultUndoAction();

		commonUndo = false;
	}

	// Put to menu, if initialized
	if (editingMenu != NULL)
		editingMenu->insertAction(this->redoAction, this->undoAction);
}

void TextElement::setCommonRedoAction(QAction *redoAction)
{
	if (this->redoAction == redoAction)
		return;

	// Setting new?
	if (redoAction != NULL)
	{
		// Clear local, if any	
		if (this->redoAction != NULL)
		{
			// Common should be removed
			if (editingMenu != NULL)
				editingMenu->removeAction(this->redoAction);
				
			if (!commonRedo)
				// Local can be just deleted
				delete this->redoAction;
		}

		this->redoAction = redoAction;
		commonRedo = true;
	}
	else
	{
		// Nothing to clear
		if (!commonRedo)
			return;

		// Clear common, if any
		if (this->redoAction != NULL)
		{
			if (editingMenu != NULL)
				editingMenu->removeAction(this->redoAction);
		}

		this->redoAction = createDefaultRedoAction();

		commonRedo = false;
	}

	// Put to menu, if initialized
	if (editingMenu != NULL)
		editingMenu->insertAction(editingMenu->actions().at(1), this->redoAction);
}

QStringList TextElement::getTagNames() const
{
	static QStringList tagNames;

	if (tagNames.isEmpty())
		tagNames << QString(METAINFO_TEXTCONTENT_TAGNAME);

	return tagNames;
}

QString TextElement::getTagContent(const QString &tagName) const
{
	if (tagName != METAINFO_TEXTCONTENT_TAGNAME)
		return QString();

	return getPlainText();
}

void TextElement::mousePressEvent(QGraphicsSceneMouseEvent *event)
{
	if ((flags() & (ItemIsSelectable | ItemIsMovable)) && (event->buttons() & Qt::LeftButton) && isMouseOnEdge(event)) 
	{
		// User left-pressed on edge of selectable/movable item, use
		// base impl.
		usingDefaultMoving = true;
	} 
	else 
		if (event->buttons() == event->button() && control->textInteractionFlags() == Qt::NoTextInteraction) 
		{
			// User pressed first button on non-interactive item.
			usingDefaultMoving = true;
		}

    // If yes, just move
	if (usingDefaultMoving)
	{
        TransformableElement::mousePressEvent(event);

		if (!event->isAccepted())
			usingDefaultMoving = false;

        return;
    }

	// If editing and not moving - let's send event to control
	if (isEditing())
	{
		sendControlEvent(event);
		return;
	}

	if (event->button() == Qt::LeftButton)
		tryStartEditing = true;

	TransformableElement::mousePressEvent(event);	
}

void TextElement::mouseMoveEvent(QGraphicsSceneMouseEvent *event)
{
	if (tryStartEditing)
		// No editing if move
		if (event->buttons() & Qt::LeftButton)
			tryStartEditing = false;

	if (usingDefaultMoving || !isEditing())
		TransformableElement::mouseMoveEvent(event);
	else
		sendControlEvent(event);
}

void TextElement::mouseReleaseEvent(QGraphicsSceneMouseEvent *event)
{
	// Ignore if moved
	if (usingDefaultMoving)
	{
		TransformableElement::mouseReleaseEvent(event);

		if (control->textInteractionFlags() == Qt::NoTextInteraction && !event->buttons()) 
		{
			// User released last button on non-interactive item.
            usingDefaultMoving = false;
		} 
		else  
			if ((event->buttons() & Qt::LeftButton) == 0) 
			{
				// User released the left button on an interactive item.
				usingDefaultMoving = false;
			}

		return;
	}

	if (!isEditing())
	{
		if (tryStartEditing)
		{
			// We really can start editing!
			startEditControl(event);

			tryStartEditing = false;
			
			sendControlEvent(event);			
		}

		// In any case we should process release event
		TransformableElement::mouseReleaseEvent(event);
	}
	else
		sendControlEvent(event);
}

void TextElement::mouseDoubleClickEvent(QGraphicsSceneMouseEvent *event)
{
	if (!isEditing())
		startEditControl(event);

    sendControlEvent(event);
}

void TextElement::keyPressEvent(QKeyEvent *event)
{
	// No direct undo/redo 
	if (event->matches(QKeySequence::Undo))
	{
		undo();
		return;
	}

	if (event->matches(QKeySequence::Redo))
	{
		redo();
		return;
	}

	// Escape means exit from text mode
	if (event->key() == Qt::Key_Escape && hasFocus())
	{
		clearFocus();

		return;
	}
	
	// Keep the text color for new text
	bool wasEmpty = isEmpty();

	sendControlEvent(event);

	if (wasEmpty && !isEmpty())
		setTextColor(currentColor);
}

void TextElement::keyReleaseEvent(QKeyEvent *event)
{
	sendControlEvent(event);
}

void TextElement::focusInEvent(QFocusEvent *event)
{
	if (isEditing())
	{
		sendControlEvent(event);

		update();
	}
}

void TextElement::focusOutEvent(QFocusEvent *event)
{
	sendControlEvent(event);

	if (event->reason() != Qt::PopupFocusReason)
	{
		clearSelection();
		stopEditControl();
	}

	update();
}

void TextElement::dragEnterEvent(QGraphicsSceneDragDropEvent *event)
{
	sendControlEvent(event);
}

void TextElement::dragLeaveEvent(QGraphicsSceneDragDropEvent *event)
{
	sendControlEvent(event);
}

void TextElement::dragMoveEvent(QGraphicsSceneDragDropEvent *event)
{
	sendControlEvent(event);
}

void TextElement::dropEvent(QGraphicsSceneDragDropEvent *event)
{
	sendControlEvent(event);
}

void TextElement::inputMethodEvent(QInputMethodEvent *event)
{
	sendControlEvent(event);
}

void TextElement::hoverEnterEvent(QGraphicsSceneHoverEvent *event)
{
	updateMouseCursor(event->pos());

	TransformableElement::hoverEnterEvent(event);

	sendControlEvent(event);
}

void TextElement::hoverMoveEvent(QGraphicsSceneHoverEvent *event)
{
	updateMouseCursor(event->pos());

	TransformableElement::hoverMoveEvent(event);

	sendControlEvent(event);
}

void TextElement::hoverLeaveEvent(QGraphicsSceneHoverEvent *event)
{
	updateMouseCursor(event->pos());

	TransformableElement::hoverLeaveEvent(event);

	sendControlEvent(event);
}

QVariant TextElement::itemChange(GraphicsItemChange change, const QVariant &value)
{
	if (change == ItemTransformHasChanged || change == ItemPositionHasChanged)
		onCursorPositionChanged();

	return TransformableElement::itemChange(change, value);
}

QVariant TextElement::inputMethodQuery(Qt::InputMethodQuery query) const
{
    QVariant v = control->inputMethodQuery(query);

    if (v.type() == QVariant::RectF)
        v = v.toRectF().translated(-getControlOffset());
    else 
		if (v.type() == QVariant::PointF)
			v = v.toPointF() - getControlOffset();
		else 
			if (v.type() == QVariant::Rect)
				v = v.toRect().translated(-getControlOffset().toPoint());
			else 
				if (v.type() == QVariant::Point)
					v = v.toPoint() - getControlOffset().toPoint();
    
	return v;
}

qint32 TextElement::getType() const
{
	return Type;
}

QMenu *TextElement::getContextMenu()
{
	if (!isEditing() || isHoverContextMenu())
		return TransformableElement::getContextMenu();

	if (editingMenu == NULL)
	{
		editingMenu = new QMenu();

		// Undo		
		editingMenu->addAction(undoAction);

		// Redo
		editingMenu->addAction(redoAction);

		editingMenu->addSeparator();

		// Cut
		QAction *action = editingMenu->addAction(tr(CUT_MENU_ITEM_TEXT));
		action->setEnabled(hasSelection());

		connect(control, SIGNAL(copyAvailable(bool)), action, SLOT(setEnabled(bool)));
		connect(action, SIGNAL(triggered()), control, SLOT(cut()));

		// Copy
		action = editingMenu->addAction(tr(COPY_MENU_ITEM_TEXT));
		action->setEnabled(hasSelection());
		
		connect(control, SIGNAL(copyAvailable(bool)), action, SLOT(setEnabled(bool)));
		connect(action, SIGNAL(triggered()), control, SLOT(copy()));
		
		// Paste
		pasteAction = editingMenu->addAction(tr(PASTE_MENU_ITEM_TEXT));
		connect(pasteAction, SIGNAL(triggered()), control, SLOT(paste()));

		editingMenu->addSeparator();

		// Select all
		action = editingMenu->addAction(tr(SELECT_ALL_MENU_ITEM_TEXT));
		connect(action, SIGNAL(triggered()), control, SLOT(selectAll()));

		connect(editingMenu, SIGNAL(aboutToShow()), this, SLOT(onContextMenuShow()));
		connect(editingMenu, SIGNAL(aboutToHide()), this, SLOT(onContextMenuHide()));
	}

	pasteAction->setEnabled(control->canPaste());

	return editingMenu;
}

QAction *TextElement::createDefaultUndoAction()
{
	QAction *action = new QAction(tr(UNDO_MENU_ITEM_TEXT), this);
	action->setEnabled(control->document()->isUndoAvailable());
	connect(action, SIGNAL(triggered()), control, SLOT(undo()));
	connect(control, SIGNAL(undoAvailable(bool)), action, SLOT(setEnabled(bool)));

	return action;
}

QAction *TextElement::createDefaultRedoAction()
{
	QAction *action = new QAction(tr(REDO_MENU_ITEM_TEXT), this);
	action->setEnabled(control->document()->isRedoAvailable());
	connect(action, SIGNAL(triggered()), control, SLOT(redo()));
	connect(control, SIGNAL(redoAvailable(bool)), action, SLOT(setEnabled(bool)));

	return action;
}

void TextElement::mergeTextFormat(const QTextCharFormat &format)
{
	QTextCursor cursor = control->textCursor();
	  
	if (!cursor.hasSelection())
		cursor.select(QTextCursor::Document);

    cursor.mergeCharFormat(format);
    
	control->mergeCurrentCharFormat(format);
}

QTextCharFormat TextElement::getTextFormat() const
{
	QTextCursor cursor = control->textCursor();
	return cursor.charFormat();
}

void TextElement::updateMouseCursor(const QPointF &pos)
{
	QRectF textBounds(dragZoneSize, dragZoneSize, controlSize.width(), controlSize.height());

	if (textBounds.contains(pos) && isEditing())
		setCursor(Qt::IBeamCursor);
	else
		setCursor(Qt::PointingHandCursor);
}

void TextElement::init()
{
	setAcceptDrops(true);
	setAcceptHoverEvents(true);

	control = new QTextControl(this);
	control->setTextInteractionFlags(Qt::TextEditorInteraction);
	setFlags(QGraphicsItem::ItemIsFocusable | QGraphicsItem::ItemIsSelectable | QGraphicsItem::ItemIsMovable);

	// Set defaults
	QFont font(DEFAULT_FONT_NAME, DEFAULT_FONT_SIZE);
	setFontStyle(DEFAULT_FONT_STYLE);
	setFont(font);
	currentColor = QColor(DEFAULT_TEXT_COLOR);
	setTextColor(currentColor);

	connect(control, SIGNAL(updateRequest(const QRectF &)), this, SLOT(onUpdateRequest(const QRectF &)));
	connect(control, SIGNAL(documentSizeChanged(const QSizeF &)), this, SLOT(updateRect(const QSizeF &)));
	connect(control, SIGNAL(visibilityRequest(const QRectF &)), this, SLOT(onVisibilityRequest(const QRectF &)));

	connect(control, SIGNAL(textChanged()), this, SLOT(onTextChanged()));
	connect(control->document(), SIGNAL(undoCommandAdded()), this, SLOT(onUndoCommandAdded()));
	connect(control, SIGNAL(cursorPositionChanged()), this, SLOT(onCursorPositionChanged()));

	const QSizeF pageSize = control->document()->pageSize();
	
	dragZoneSize = TEXT_ELEMENT_CONVENIENCE_ZONE;
	updateDragZoneSize();
	
	if (pageSize.height() != -1) 
	{
		setRectSize(pageSize);
	}
	else 
	{
		updateRect(control->size());
		updateHoverPoints();
    }

	// Actions
	undoAction = createDefaultUndoAction();
	redoAction = createDefaultRedoAction();

	// Cursor
	updateMouseCursor(-getControlOffset());
}

QPointF TextElement::getControlOffset() const
{
	return QPointF(-dragZoneSize, -dragZoneSize);
}

bool TextElement::updateDragZoneSize()
{
	qreal sceneSize = getFontSize() * getTransformation().getScale();

	if (sceneSize == 0)
		return false;

	qreal newSize = (DEFAULT_FONT_SIZE * TEXT_ELEMENT_CONVENIENCE_ZONE) / sceneSize;

	if (newSize == dragZoneSize)
		return false;

	dragZoneSize = newSize;
	return true;
}

void TextElement::setRectSize(const QSizeF &controlSize)
{	
	prepareGeometryChange();

	this->controlSize = controlSize;
    rect.setSize(controlSize);
	rect.adjust(0, 0, dragZoneSize * 2, dragZoneSize * 2);	

	updateHoverPoints();
    update();
}

void TextElement::silentAlignCenter(const QPointF &oldCenter)
{
	QPointF newCenter = mapToParent(boundingRect().center());

	if (oldCenter == newCenter)
		return;

	setSilentChanging(true);		
	setPos(pos() + oldCenter - newCenter);
	setSilentChanging(false);
}

void TextElement::sendControlEvent(QEvent *event)
{
	control->processEvent(event, getControlOffset());
}

bool TextElement::isMouseOnEdge(QGraphicsSceneMouseEvent *mouseEvent) const
{
	QPainterPath path;
	path.addRect(boundingRect());

	QPainterPath innerPath;
	innerPath.addRect(boundingRect().adjusted(dragZoneSize, dragZoneSize, -dragZoneSize, -dragZoneSize));
	
    return path.subtracted(innerPath).contains(mouseEvent->pos());
}

void TextElement::clearSelection()
{
	QTextCursor cursor = control->textCursor();
    cursor.clearSelection();
    control->setTextCursor(cursor);
}

void TextElement::moveCursorToEnd()
{
	QTextCursor cursor = control->textCursor();
    cursor.movePosition(QTextCursor::End);
    control->setTextCursor(cursor);
}

void TextElement::startEditControl(QGraphicsSceneMouseEvent *initMouseEvent)
{
	if (editing)
		return;

	editing = true;

	if (initMouseEvent != NULL)
	{
		QGraphicsSceneMouseEvent pressEvent(QEvent::GraphicsSceneMousePress);
		pressEvent.setButton(Qt::LeftButton);
		pressEvent.setPos(initMouseEvent->pos());
		pressEvent.setScreenPos(initMouseEvent->screenPos());
		pressEvent.setScenePos(initMouseEvent->scenePos());
		pressEvent.setWidget(initMouseEvent->widget());
				
		QFocusEvent focusEvent(QEvent::FocusIn, Qt::MouseFocusReason);

		// Send wake-up events, focus goes first
		sendControlEvent(&focusEvent);
		sendControlEvent(&pressEvent);
	}

	// Update mouse cursor
	updateMouseCursor( (initMouseEvent != NULL) ? initMouseEvent->pos() : -getControlOffset());

	emit editingStarted(this);
}

void TextElement::stopEditControl()
{
	if (!isEditing())
		return;

	editing = false;

	// Update mouse cursor
	updateMouseCursor(-getControlOffset());

	emit editingFinished(this);
}

void TextElement::updateRect(const QSizeF &size)
{
	const QSizeF pageSize = control->document()->pageSize();

    // Paged items have a constant (page) size
	if (size == controlSize || pageSize.height() != -1)
        return;

	setRectSize(size);
}

void TextElement::onUpdateRequest(const QRectF &rect)
{
	QRectF updatingRect = rect;

	if (updatingRect.isValid()) 
		updatingRect.translate(-getControlOffset());
	else 
		updatingRect = this->rect;

	updatingRect.adjust(0, 0, dragZoneSize, dragZoneSize);

	if (updatingRect.intersects(this->rect))
		update(updatingRect);
}

void TextElement::onVisibilityRequest(const QRectF &rect)
{
	QRectF visibilityRect = rect;

	if (hasFocus()) 
	{
        visibilityRect.translate(-getControlOffset());
        ensureVisible(visibilityRect, /*xmargin=*/0, /*ymargin=*/0);
    }
}

void TextElement::onTextChanged()
{
	emit textChanged(this);
}

void TextElement::onUndoCommandAdded()
{
	emit textUndoAdded(this);
}

void TextElement::onCursorPositionChanged()
{
	emit cursorMoved(this);
}
	
}
