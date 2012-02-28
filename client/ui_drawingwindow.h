/********************************************************************************
** Form generated from reading ui file 'drawingwindow.ui'
**
** Created: Wed Jun 24 21:01:09 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_DRAWINGWINDOW_H
#define UI_DRAWINGWINDOW_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QFrame>
#include <QtGui/QGraphicsView>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_DrawingWindowClass
{
public:
    QVBoxLayout *verticalLayout_2;
    QFrame *styledFrame;
    QGridLayout *gridLayout;
    QFrame *drawFrame;
    QVBoxLayout *verticalLayout;
    QGraphicsView *drawingView;
    QFrame *toolSettingsHost;
    QHBoxLayout *hboxLayout;
    QFrame *toolbarFrame;
    QHBoxLayout *horizontalLayout_2;
    QToolButton *penToolButton;
    QToolButton *simplebrushToolButton;
    QToolButton *oilbrushToolButton;
    QToolButton *sprayToolButton;
    QToolButton *eraserToolButton;
    QToolButton *textToolButton;
    QToolButton *picturesToolButton;
    QSpacerItem *horizontalSpacer;
    QFrame *subToolBarFrame;
    QHBoxLayout *horizontalLayout_3;
    QToolButton *undoButton;
    QToolButton *redoButton;
    QToolButton *clearButton;
    QSpacerItem *horizontalSpacer_2;
    QHBoxLayout *horizontalLayout;
    QPushButton *sendButton;
    QFrame *frame;

    void setupUi(QWidget *DrawingWindowClass)
    {
        if (DrawingWindowClass->objectName().isEmpty())
            DrawingWindowClass->setObjectName(QString::fromUtf8("DrawingWindowClass"));
        DrawingWindowClass->resize(496, 336);
        verticalLayout_2 = new QVBoxLayout(DrawingWindowClass);
        verticalLayout_2->setSpacing(6);
        verticalLayout_2->setMargin(0);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        styledFrame = new QFrame(DrawingWindowClass);
        styledFrame->setObjectName(QString::fromUtf8("styledFrame"));
        styledFrame->setFrameShape(QFrame::NoFrame);
        styledFrame->setFrameShadow(QFrame::Plain);
        styledFrame->setLineWidth(0);
        gridLayout = new QGridLayout(styledFrame);
        gridLayout->setSpacing(6);
        gridLayout->setMargin(0);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        drawFrame = new QFrame(styledFrame);
        drawFrame->setObjectName(QString::fromUtf8("drawFrame"));
        drawFrame->setFrameShape(QFrame::NoFrame);
        drawFrame->setFrameShadow(QFrame::Plain);
        drawFrame->setLineWidth(0);
        verticalLayout = new QVBoxLayout(drawFrame);
        verticalLayout->setSpacing(6);
        verticalLayout->setMargin(0);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        drawingView = new QGraphicsView(drawFrame);
        drawingView->setObjectName(QString::fromUtf8("drawingView"));
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(drawingView->sizePolicy().hasHeightForWidth());
        drawingView->setSizePolicy(sizePolicy);
        drawingView->setMinimumSize(QSize(388, 240));
        drawingView->setFocusPolicy(Qt::StrongFocus);
        drawingView->setFrameShape(QFrame::NoFrame);
        drawingView->setFrameShadow(QFrame::Plain);
        drawingView->setLineWidth(0);
        drawingView->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        drawingView->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        drawingView->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignTop);
        drawingView->setTransformationAnchor(QGraphicsView::NoAnchor);

        verticalLayout->addWidget(drawingView);


        gridLayout->addWidget(drawFrame, 2, 1, 1, 1);

        toolSettingsHost = new QFrame(styledFrame);
        toolSettingsHost->setObjectName(QString::fromUtf8("toolSettingsHost"));
        toolSettingsHost->setMinimumSize(QSize(86, 0));
        toolSettingsHost->setFrameShape(QFrame::NoFrame);
        toolSettingsHost->setFrameShadow(QFrame::Plain);
        toolSettingsHost->setLineWidth(0);
        hboxLayout = new QHBoxLayout(toolSettingsHost);
        hboxLayout->setSpacing(0);
        hboxLayout->setMargin(11);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        hboxLayout->setContentsMargins(0, 0, 9, 0);

        gridLayout->addWidget(toolSettingsHost, 2, 2, 1, 2);

        toolbarFrame = new QFrame(styledFrame);
        toolbarFrame->setObjectName(QString::fromUtf8("toolbarFrame"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(toolbarFrame->sizePolicy().hasHeightForWidth());
        toolbarFrame->setSizePolicy(sizePolicy1);
        toolbarFrame->setMinimumSize(QSize(388, 41));
        toolbarFrame->setMaximumSize(QSize(16777215, 41));
        toolbarFrame->setCursor(QCursor(Qt::PointingHandCursor));
        toolbarFrame->setFrameShape(QFrame::NoFrame);
        toolbarFrame->setFrameShadow(QFrame::Plain);
        toolbarFrame->setLineWidth(0);
        horizontalLayout_2 = new QHBoxLayout(toolbarFrame);
        horizontalLayout_2->setSpacing(0);
        horizontalLayout_2->setMargin(11);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        horizontalLayout_2->setContentsMargins(9, 9, 1, 0);
        penToolButton = new QToolButton(toolbarFrame);
        penToolButton->setObjectName(QString::fromUtf8("penToolButton"));
        penToolButton->setMaximumSize(QSize(32, 32));
        penToolButton->setCursor(QCursor(Qt::ArrowCursor));
        penToolButton->setFocusPolicy(Qt::NoFocus);
        penToolButton->setIconSize(QSize(32, 32));
        penToolButton->setCheckable(true);
        penToolButton->setChecked(false);
        penToolButton->setAutoExclusive(true);
        penToolButton->setAutoRaise(true);

        horizontalLayout_2->addWidget(penToolButton);

        simplebrushToolButton = new QToolButton(toolbarFrame);
        simplebrushToolButton->setObjectName(QString::fromUtf8("simplebrushToolButton"));
        simplebrushToolButton->setMaximumSize(QSize(32, 32));
        simplebrushToolButton->setCursor(QCursor(Qt::ArrowCursor));
        simplebrushToolButton->setFocusPolicy(Qt::NoFocus);
        simplebrushToolButton->setIconSize(QSize(32, 32));
        simplebrushToolButton->setCheckable(true);
        simplebrushToolButton->setChecked(true);
        simplebrushToolButton->setAutoExclusive(true);
        simplebrushToolButton->setAutoRaise(true);

        horizontalLayout_2->addWidget(simplebrushToolButton);

        oilbrushToolButton = new QToolButton(toolbarFrame);
        oilbrushToolButton->setObjectName(QString::fromUtf8("oilbrushToolButton"));
        oilbrushToolButton->setMaximumSize(QSize(32, 32));
        oilbrushToolButton->setCursor(QCursor(Qt::ArrowCursor));
        oilbrushToolButton->setFocusPolicy(Qt::NoFocus);
        oilbrushToolButton->setIconSize(QSize(32, 32));
        oilbrushToolButton->setCheckable(true);
        oilbrushToolButton->setChecked(false);
        oilbrushToolButton->setAutoExclusive(true);
        oilbrushToolButton->setAutoRaise(true);

        horizontalLayout_2->addWidget(oilbrushToolButton);

        sprayToolButton = new QToolButton(toolbarFrame);
        sprayToolButton->setObjectName(QString::fromUtf8("sprayToolButton"));
        sprayToolButton->setMaximumSize(QSize(32, 32));
        sprayToolButton->setCursor(QCursor(Qt::ArrowCursor));
        sprayToolButton->setFocusPolicy(Qt::NoFocus);
        sprayToolButton->setIconSize(QSize(32, 32));
        sprayToolButton->setCheckable(true);
        sprayToolButton->setAutoExclusive(true);
        sprayToolButton->setAutoRaise(true);

        horizontalLayout_2->addWidget(sprayToolButton);

        eraserToolButton = new QToolButton(toolbarFrame);
        eraserToolButton->setObjectName(QString::fromUtf8("eraserToolButton"));
        eraserToolButton->setMaximumSize(QSize(32, 32));
        eraserToolButton->setCursor(QCursor(Qt::ArrowCursor));
        eraserToolButton->setFocusPolicy(Qt::NoFocus);
        eraserToolButton->setIconSize(QSize(32, 32));
        eraserToolButton->setCheckable(true);
        eraserToolButton->setAutoExclusive(true);
        eraserToolButton->setAutoRaise(true);

        horizontalLayout_2->addWidget(eraserToolButton);

        textToolButton = new QToolButton(toolbarFrame);
        textToolButton->setObjectName(QString::fromUtf8("textToolButton"));
        textToolButton->setMaximumSize(QSize(32, 32));
        textToolButton->setCursor(QCursor(Qt::ArrowCursor));
        textToolButton->setFocusPolicy(Qt::NoFocus);
        textToolButton->setIconSize(QSize(32, 32));
        textToolButton->setCheckable(true);
        textToolButton->setAutoExclusive(true);
        textToolButton->setAutoRaise(true);

        horizontalLayout_2->addWidget(textToolButton);

        picturesToolButton = new QToolButton(toolbarFrame);
        picturesToolButton->setObjectName(QString::fromUtf8("picturesToolButton"));
        picturesToolButton->setMaximumSize(QSize(32, 32));
        picturesToolButton->setCursor(QCursor(Qt::ArrowCursor));
        picturesToolButton->setFocusPolicy(Qt::NoFocus);
        picturesToolButton->setIconSize(QSize(32, 32));
        picturesToolButton->setCheckable(true);
        picturesToolButton->setAutoExclusive(true);
        picturesToolButton->setAutoRaise(true);

        horizontalLayout_2->addWidget(picturesToolButton);

        horizontalSpacer = new QSpacerItem(73, 20, QSizePolicy::Fixed, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer);

        subToolBarFrame = new QFrame(toolbarFrame);
        subToolBarFrame->setObjectName(QString::fromUtf8("subToolBarFrame"));
        subToolBarFrame->setMinimumSize(QSize(96, 0));
        subToolBarFrame->setCursor(QCursor(Qt::ArrowCursor));
        subToolBarFrame->setFrameShape(QFrame::NoFrame);
        subToolBarFrame->setFrameShadow(QFrame::Raised);
        subToolBarFrame->setLineWidth(0);
        horizontalLayout_3 = new QHBoxLayout(subToolBarFrame);
        horizontalLayout_3->setSpacing(0);
        horizontalLayout_3->setMargin(0);
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        undoButton = new QToolButton(subToolBarFrame);
        undoButton->setObjectName(QString::fromUtf8("undoButton"));
        undoButton->setMaximumSize(QSize(32, 32));
        undoButton->setCursor(QCursor(Qt::ArrowCursor));
        undoButton->setFocusPolicy(Qt::NoFocus);
        undoButton->setIconSize(QSize(32, 32));
        undoButton->setAutoRaise(true);

        horizontalLayout_3->addWidget(undoButton);

        redoButton = new QToolButton(subToolBarFrame);
        redoButton->setObjectName(QString::fromUtf8("redoButton"));
        redoButton->setMaximumSize(QSize(32, 32));
        redoButton->setCursor(QCursor(Qt::ArrowCursor));
        redoButton->setFocusPolicy(Qt::NoFocus);
        redoButton->setIconSize(QSize(32, 32));
        redoButton->setAutoRaise(true);

        horizontalLayout_3->addWidget(redoButton);

        clearButton = new QToolButton(subToolBarFrame);
        clearButton->setObjectName(QString::fromUtf8("clearButton"));
        clearButton->setMaximumSize(QSize(32, 32));
        clearButton->setCursor(QCursor(Qt::ArrowCursor));
        clearButton->setFocusPolicy(Qt::NoFocus);
        clearButton->setIconSize(QSize(32, 32));
        clearButton->setAutoRaise(true);

        horizontalLayout_3->addWidget(clearButton);


        horizontalLayout_2->addWidget(subToolBarFrame);

        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_2);


        gridLayout->addWidget(toolbarFrame, 0, 0, 2, 4);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalLayout->setSizeConstraint(QLayout::SetFixedSize);
        horizontalLayout->setContentsMargins(-1, 0, -1, 18);
        sendButton = new QPushButton(styledFrame);
        sendButton->setObjectName(QString::fromUtf8("sendButton"));
        sizePolicy.setHeightForWidth(sendButton->sizePolicy().hasHeightForWidth());
        sendButton->setSizePolicy(sizePolicy);
        sendButton->setMinimumSize(QSize(130, 20));
        sendButton->setFocusPolicy(Qt::NoFocus);

        horizontalLayout->addWidget(sendButton);


        gridLayout->addLayout(horizontalLayout, 3, 1, 1, 1);

        frame = new QFrame(styledFrame);
        frame->setObjectName(QString::fromUtf8("frame"));
        frame->setMinimumSize(QSize(6, 0));
        frame->setMaximumSize(QSize(6, 16777215));
        frame->setFrameShape(QFrame::NoFrame);
        frame->setFrameShadow(QFrame::Plain);
        frame->setLineWidth(0);

        gridLayout->addWidget(frame, 2, 0, 2, 1);


        verticalLayout_2->addWidget(styledFrame);


        retranslateUi(DrawingWindowClass);

        QMetaObject::connectSlotsByName(DrawingWindowClass);
    } // setupUi

    void retranslateUi(QWidget *DrawingWindowClass)
    {
#ifndef QT_NO_TOOLTIP
        penToolButton->setToolTip(QApplication::translate("DrawingWindowClass", "PencilHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        penToolButton->setAccessibleName(QApplication::translate("DrawingWindowClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        penToolButton->setText(QString());
#ifndef QT_NO_TOOLTIP
        simplebrushToolButton->setToolTip(QApplication::translate("DrawingWindowClass", "BrushHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        simplebrushToolButton->setAccessibleName(QApplication::translate("DrawingWindowClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_TOOLTIP
        oilbrushToolButton->setToolTip(QApplication::translate("DrawingWindowClass", "HairBrushHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        oilbrushToolButton->setAccessibleName(QApplication::translate("DrawingWindowClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_TOOLTIP
        sprayToolButton->setToolTip(QApplication::translate("DrawingWindowClass", "AirbrushHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        sprayToolButton->setAccessibleName(QApplication::translate("DrawingWindowClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_TOOLTIP
        eraserToolButton->setToolTip(QApplication::translate("DrawingWindowClass", "EraserHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        eraserToolButton->setAccessibleName(QApplication::translate("DrawingWindowClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_TOOLTIP
        textToolButton->setToolTip(QApplication::translate("DrawingWindowClass", "TextHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        textToolButton->setAccessibleName(QApplication::translate("DrawingWindowClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        textToolButton->setText(QString());
#ifndef QT_NO_TOOLTIP
        picturesToolButton->setToolTip(QApplication::translate("DrawingWindowClass", "PicturesHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        picturesToolButton->setAccessibleName(QApplication::translate("DrawingWindowClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        picturesToolButton->setText(QString());
#ifndef QT_NO_TOOLTIP
        undoButton->setToolTip(QApplication::translate("DrawingWindowClass", "UndoHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        undoButton->setAccessibleName(QApplication::translate("DrawingWindowClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        undoButton->setText(QString());
#ifndef QT_NO_TOOLTIP
        redoButton->setToolTip(QApplication::translate("DrawingWindowClass", "RedoHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        redoButton->setAccessibleName(QApplication::translate("DrawingWindowClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        redoButton->setText(QString());
#ifndef QT_NO_TOOLTIP
        clearButton->setToolTip(QApplication::translate("DrawingWindowClass", "ClearHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        clearButton->setAccessibleName(QApplication::translate("DrawingWindowClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        clearButton->setText(QString());
#ifndef QT_NO_TOOLTIP
        sendButton->setToolTip(QApplication::translate("DrawingWindowClass", "SendHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        sendButton->setAccessibleName(QApplication::translate("DrawingWindowClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        Q_UNUSED(DrawingWindowClass);
    } // retranslateUi

};

namespace Ui {
    class DrawingWindowClass: public Ui_DrawingWindowClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_DRAWINGWINDOW_H
