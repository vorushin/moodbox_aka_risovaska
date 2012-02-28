/********************************************************************************
** Form generated from reading ui file 'coloreditor.ui'
**
** Created: Tue May 5 10:34:38 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_COLOREDITOR_H
#define UI_COLOREDITOR_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QFrame>
#include <QtGui/QHeaderView>
#include <QtGui/QPushButton>
#include <QtGui/QToolButton>
#include <QtGui/QWidget>
#include "colorvolumewidget.h"
#include "colorwheelwidget.h"
#include "colorwidget.h"

QT_BEGIN_NAMESPACE

class Ui_ColorEditorClass
{
public:
    QFrame *styledFrame;
    QPushButton *okButton;
    MoodBox::ColorWheelWidget *colorWheel;
    QToolButton *closeToolButton;
    MoodBox::ColorVolumeWidget *colorVolume;
    QFrame *colorFrame;
    MoodBox::ColorWidget *colorWidget;

    void setupUi(QWidget *ColorEditorClass)
    {
        if (ColorEditorClass->objectName().isEmpty())
            ColorEditorClass->setObjectName(QString::fromUtf8("ColorEditorClass"));
        ColorEditorClass->resize(235, 238);
        styledFrame = new QFrame(ColorEditorClass);
        styledFrame->setObjectName(QString::fromUtf8("styledFrame"));
        styledFrame->setGeometry(QRect(24, 0, 211, 238));
        QSizePolicy sizePolicy(QSizePolicy::Maximum, QSizePolicy::Maximum);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(styledFrame->sizePolicy().hasHeightForWidth());
        styledFrame->setSizePolicy(sizePolicy);
        styledFrame->setFrameShape(QFrame::NoFrame);
        styledFrame->setFrameShadow(QFrame::Raised);
        styledFrame->setLineWidth(0);
        okButton = new QPushButton(styledFrame);
        okButton->setObjectName(QString::fromUtf8("okButton"));
        okButton->setGeometry(QRect(74, 198, 36, 23));
        colorWheel = new MoodBox::ColorWheelWidget(styledFrame);
        colorWheel->setObjectName(QString::fromUtf8("colorWheel"));
        colorWheel->setGeometry(QRect(32, 10, 147, 147));
        closeToolButton = new QToolButton(styledFrame);
        closeToolButton->setObjectName(QString::fromUtf8("closeToolButton"));
        closeToolButton->setGeometry(QRect(191, 5, 23, 22));
        closeToolButton->setFocusPolicy(Qt::NoFocus);
        colorVolume = new MoodBox::ColorVolumeWidget(styledFrame);
        colorVolume->setObjectName(QString::fromUtf8("colorVolume"));
        colorVolume->setGeometry(QRect(32, 164, 147, 22));
        colorFrame = new QFrame(ColorEditorClass);
        colorFrame->setObjectName(QString::fromUtf8("colorFrame"));
        colorFrame->setGeometry(QRect(0, 29, 27, 28));
        QSizePolicy sizePolicy1(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(colorFrame->sizePolicy().hasHeightForWidth());
        colorFrame->setSizePolicy(sizePolicy1);
        colorFrame->setFrameShape(QFrame::NoFrame);
        colorFrame->setFrameShadow(QFrame::Raised);
        colorFrame->setLineWidth(0);
        colorWidget = new MoodBox::ColorWidget(colorFrame);
        colorWidget->setObjectName(QString::fromUtf8("colorWidget"));
        colorWidget->setGeometry(QRect(5, 6, 21, 21));

        retranslateUi(ColorEditorClass);

        QMetaObject::connectSlotsByName(ColorEditorClass);
    } // setupUi

    void retranslateUi(QWidget *ColorEditorClass)
    {
#ifndef QT_NO_ACCESSIBILITY
        okButton->setAccessibleName(QApplication::translate("ColorEditorClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        okButton->setText(QApplication::translate("ColorEditorClass", "OkButton", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        closeToolButton->setAccessibleName(QApplication::translate("ColorEditorClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        Q_UNUSED(ColorEditorClass);
    } // retranslateUi

};

namespace Ui {
    class ColorEditorClass: public Ui_ColorEditorClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_COLOREDITOR_H
