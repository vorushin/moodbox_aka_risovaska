/********************************************************************************
** Form generated from reading ui file 'palettemanager.ui'
**
** Created: Tue May 5 10:34:49 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_PALETTEMANAGER_H
#define UI_PALETTEMANAGER_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QSpacerItem>
#include <QtGui/QTabWidget>
#include <QtGui/QToolButton>
#include <QtGui/QWidget>
#include "palettelistwidget.h"
#include "paletteviewwidget.h"

QT_BEGIN_NAMESPACE

class Ui_PaletteManagerClass
{
public:
    QFrame *styledFrame;
    QWidget *layoutWidget;
    QHBoxLayout *horizontalLayout;
    MoodBox::PaletteViewWidget *currentPalette;
    QSpacerItem *horizontalSpacer;
    QToolButton *savePaletteButton;
    QSpacerItem *horizontalSpacer_2;
    QTabWidget *paletteTabWidget;
    QWidget *standardPaletteTab;
    QGridLayout *gridLayout_2;
    MoodBox::PaletteListWidget *standardPalettes;
    QWidget *scrollAreaWidgetContents;
    QFrame *topPaletteFrame;
    QLabel *label;
    QFrame *frame;
    QLabel *label_2;
    QWidget *customPaletteTab;
    QGridLayout *gridLayout_3;
    MoodBox::PaletteListWidget *customPalettes;
    QWidget *scrollAreaWidgetContents_2;
    QFrame *topPaletteFrame_2;
    QLabel *label_3;
    QFrame *frame_2;
    QLabel *label_4;
    QToolButton *closeToolButton;

    void setupUi(QWidget *PaletteManagerClass)
    {
        if (PaletteManagerClass->objectName().isEmpty())
            PaletteManagerClass->setObjectName(QString::fromUtf8("PaletteManagerClass"));
        PaletteManagerClass->resize(261, 330);
        styledFrame = new QFrame(PaletteManagerClass);
        styledFrame->setObjectName(QString::fromUtf8("styledFrame"));
        styledFrame->setGeometry(QRect(0, 0, 261, 330));
        QSizePolicy sizePolicy(QSizePolicy::Maximum, QSizePolicy::Maximum);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(styledFrame->sizePolicy().hasHeightForWidth());
        styledFrame->setSizePolicy(sizePolicy);
        styledFrame->setFrameShape(QFrame::StyledPanel);
        styledFrame->setFrameShadow(QFrame::Raised);
        layoutWidget = new QWidget(styledFrame);
        layoutWidget->setObjectName(QString::fromUtf8("layoutWidget"));
        layoutWidget->setGeometry(QRect(12, 14, 228, 43));
        horizontalLayout = new QHBoxLayout(layoutWidget);
        horizontalLayout->setSpacing(0);
        horizontalLayout->setMargin(11);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalLayout->setContentsMargins(0, 0, 0, 0);
        currentPalette = new MoodBox::PaletteViewWidget(layoutWidget);
        currentPalette->setObjectName(QString::fromUtf8("currentPalette"));
        currentPalette->setMinimumSize(QSize(163, 41));
        currentPalette->setMaximumSize(QSize(163, 41));

        horizontalLayout->addWidget(currentPalette);

        horizontalSpacer = new QSpacerItem(30, 20, QSizePolicy::Fixed, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);

        savePaletteButton = new QToolButton(layoutWidget);
        savePaletteButton->setObjectName(QString::fromUtf8("savePaletteButton"));
        savePaletteButton->setMinimumSize(QSize(22, 22));
        savePaletteButton->setFocusPolicy(Qt::NoFocus);
        savePaletteButton->setIconSize(QSize(22, 22));

        horizontalLayout->addWidget(savePaletteButton);

        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer_2);

        paletteTabWidget = new QTabWidget(styledFrame);
        paletteTabWidget->setObjectName(QString::fromUtf8("paletteTabWidget"));
        paletteTabWidget->setGeometry(QRect(3, 71, 256, 257));
        paletteTabWidget->setTabPosition(QTabWidget::North);
        paletteTabWidget->setElideMode(Qt::ElideNone);
        standardPaletteTab = new QWidget();
        standardPaletteTab->setObjectName(QString::fromUtf8("standardPaletteTab"));
        gridLayout_2 = new QGridLayout(standardPaletteTab);
        gridLayout_2->setSpacing(6);
        gridLayout_2->setMargin(11);
        gridLayout_2->setObjectName(QString::fromUtf8("gridLayout_2"));
        gridLayout_2->setVerticalSpacing(6);
        standardPalettes = new MoodBox::PaletteListWidget(standardPaletteTab);
        standardPalettes->setObjectName(QString::fromUtf8("standardPalettes"));
        standardPalettes->setFrameShape(QFrame::NoFrame);
        standardPalettes->setFrameShadow(QFrame::Plain);
        standardPalettes->setLineWidth(0);
        standardPalettes->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        standardPalettes->setWidgetResizable(true);
        scrollAreaWidgetContents = new QWidget();
        scrollAreaWidgetContents->setObjectName(QString::fromUtf8("scrollAreaWidgetContents"));
        scrollAreaWidgetContents->setGeometry(QRect(0, 0, 232, 176));
        standardPalettes->setWidget(scrollAreaWidgetContents);

        gridLayout_2->addWidget(standardPalettes, 1, 0, 1, 1);

        topPaletteFrame = new QFrame(standardPaletteTab);
        topPaletteFrame->setObjectName(QString::fromUtf8("topPaletteFrame"));
        topPaletteFrame->setMinimumSize(QSize(0, 14));
        topPaletteFrame->setFrameShape(QFrame::NoFrame);
        topPaletteFrame->setFrameShadow(QFrame::Raised);
        label = new QLabel(topPaletteFrame);
        label->setObjectName(QString::fromUtf8("label"));
        label->setGeometry(QRect(12, 8, 222, 6));
        label->setPixmap(QPixmap(QString::fromUtf8(":/MoodBox/Resources/palette_horizontal_top_line.png")));

        gridLayout_2->addWidget(topPaletteFrame, 0, 0, 1, 1);

        frame = new QFrame(standardPaletteTab);
        frame->setObjectName(QString::fromUtf8("frame"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(frame->sizePolicy().hasHeightForWidth());
        frame->setSizePolicy(sizePolicy1);
        frame->setMinimumSize(QSize(0, 10));
        frame->setFrameShape(QFrame::NoFrame);
        frame->setFrameShadow(QFrame::Raised);
        label_2 = new QLabel(frame);
        label_2->setObjectName(QString::fromUtf8("label_2"));
        label_2->setGeometry(QRect(12, -2, 222, 6));
        label_2->setPixmap(QPixmap(QString::fromUtf8(":/MoodBox/Resources/palette_horizontal_bottom_line.png")));

        gridLayout_2->addWidget(frame, 2, 0, 1, 1);

        paletteTabWidget->addTab(standardPaletteTab, QString());
        customPaletteTab = new QWidget();
        customPaletteTab->setObjectName(QString::fromUtf8("customPaletteTab"));
        gridLayout_3 = new QGridLayout(customPaletteTab);
        gridLayout_3->setSpacing(6);
        gridLayout_3->setMargin(11);
        gridLayout_3->setObjectName(QString::fromUtf8("gridLayout_3"));
        customPalettes = new MoodBox::PaletteListWidget(customPaletteTab);
        customPalettes->setObjectName(QString::fromUtf8("customPalettes"));
        customPalettes->setFrameShape(QFrame::NoFrame);
        customPalettes->setFrameShadow(QFrame::Plain);
        customPalettes->setLineWidth(0);
        customPalettes->setWidgetResizable(true);
        scrollAreaWidgetContents_2 = new QWidget();
        scrollAreaWidgetContents_2->setObjectName(QString::fromUtf8("scrollAreaWidgetContents_2"));
        scrollAreaWidgetContents_2->setGeometry(QRect(0, 0, 232, 176));
        customPalettes->setWidget(scrollAreaWidgetContents_2);

        gridLayout_3->addWidget(customPalettes, 1, 0, 1, 1);

        topPaletteFrame_2 = new QFrame(customPaletteTab);
        topPaletteFrame_2->setObjectName(QString::fromUtf8("topPaletteFrame_2"));
        topPaletteFrame_2->setMinimumSize(QSize(0, 14));
        topPaletteFrame_2->setFrameShape(QFrame::NoFrame);
        topPaletteFrame_2->setFrameShadow(QFrame::Raised);
        label_3 = new QLabel(topPaletteFrame_2);
        label_3->setObjectName(QString::fromUtf8("label_3"));
        label_3->setGeometry(QRect(12, 8, 222, 6));
        label_3->setPixmap(QPixmap(QString::fromUtf8(":/MoodBox/Resources/palette_horizontal_top_line.png")));

        gridLayout_3->addWidget(topPaletteFrame_2, 0, 0, 1, 1);

        frame_2 = new QFrame(customPaletteTab);
        frame_2->setObjectName(QString::fromUtf8("frame_2"));
        sizePolicy1.setHeightForWidth(frame_2->sizePolicy().hasHeightForWidth());
        frame_2->setSizePolicy(sizePolicy1);
        frame_2->setMinimumSize(QSize(0, 10));
        frame_2->setFrameShape(QFrame::NoFrame);
        frame_2->setFrameShadow(QFrame::Raised);
        label_4 = new QLabel(frame_2);
        label_4->setObjectName(QString::fromUtf8("label_4"));
        label_4->setGeometry(QRect(12, -2, 222, 6));
        label_4->setPixmap(QPixmap(QString::fromUtf8(":/MoodBox/Resources/palette_horizontal_bottom_line.png")));

        gridLayout_3->addWidget(frame_2, 2, 0, 1, 1);

        paletteTabWidget->addTab(customPaletteTab, QString());
        closeToolButton = new QToolButton(styledFrame);
        closeToolButton->setObjectName(QString::fromUtf8("closeToolButton"));
        closeToolButton->setGeometry(QRect(241, 5, 23, 22));
        closeToolButton->setFocusPolicy(Qt::NoFocus);

        retranslateUi(PaletteManagerClass);

        paletteTabWidget->setCurrentIndex(0);


        QMetaObject::connectSlotsByName(PaletteManagerClass);
    } // setupUi

    void retranslateUi(QWidget *PaletteManagerClass)
    {
#ifndef QT_NO_TOOLTIP
        savePaletteButton->setToolTip(QApplication::translate("PaletteManagerClass", "SavePaletteHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        savePaletteButton->setAccessibleName(QApplication::translate("PaletteManagerClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        paletteTabWidget->setTabText(paletteTabWidget->indexOf(standardPaletteTab), QApplication::translate("PaletteManagerClass", "CatalogueTab", 0, QApplication::UnicodeUTF8));
        paletteTabWidget->setTabText(paletteTabWidget->indexOf(customPaletteTab), QApplication::translate("PaletteManagerClass", "MyPalettesTab", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        closeToolButton->setAccessibleName(QApplication::translate("PaletteManagerClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        Q_UNUSED(PaletteManagerClass);
    } // retranslateUi

};

namespace Ui {
    class PaletteManagerClass: public Ui_PaletteManagerClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_PALETTEMANAGER_H
