#ifndef EDITORFORM_H
#define EDITORFORM_H

#include <QWidget>

#include "ui_editorform.h"

class QUndoStack;
class QUndoView;

namespace Velasquez
{

using namespace Ui;

class SettingsProvider;
class ToolBox;
class DrawingTool;
class DrawingElement;
class EditorScene;

class EditorForm : public QWidget, public EditorFormClass
{
	Q_OBJECT

public:
	EditorForm(QWidget *parent = 0);
	~EditorForm();

private:
	QUndoStack *undoStack;
	QUndoView *undoView;
	EditorScene *scene;
	SettingsProvider *settings, *oilBrushSettings, *spraySettings, *simpleBrushSettings;
	ToolBox *toolBox;
	DrawingElement* elementCache;

private slots:
	void on_widthEdit_valueChanged(int width);
	void on_undoViewButton_released();

	void on_penToolButton_released();
	void on_svgToolButton_released();
	void on_pixmapToolButton_released();
	void on_textToolButton_released();
	void on_oilBrushToolButton_released();
	void on_eraserToolButton_released();
	void on_sprayToolButton_released();
	void on_simpleBrushToolButton_released();
	
	void on_saveMetaButton_released();
	void on_savePNGButton_released();
		
	void on_clearButton_released();

	void on_fontSizeBox_valueChanged(double value);
	void on_fontComboBox_currentFontChanged(const QFont &font);
	void on_boldButton_toggled(bool checked);
	void on_paletteManagerButton_clicked();
	void on_bgButton_clicked();

	void onElementSelected(DrawingElement *element);
	void onToolActivated(DrawingTool *tool);

	void onColorSelected(const QColor &color);
	void onSizeSelected(qreal size);
	void onAlphaSelected(qreal alpha);
};

}

#endif // EDITORFORM_H
