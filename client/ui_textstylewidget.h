/********************************************************************************
** Form generated from reading ui file 'textstylewidget.ui'
**
** Created: Tue May 5 10:34:55 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_TEXTSTYLEWIDGET_H
#define UI_TEXTSTYLEWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QHeaderView>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_TextStyleWidgetClass
{
public:
    QVBoxLayout *verticalLayout;
    QToolButton *textStyleButton1;
    QToolButton *textStyleButton2;
    QToolButton *textStyleButton3;

    void setupUi(QWidget *TextStyleWidgetClass)
    {
        if (TextStyleWidgetClass->objectName().isEmpty())
            TextStyleWidgetClass->setObjectName(QString::fromUtf8("TextStyleWidgetClass"));
        TextStyleWidgetClass->resize(145, 130);
        verticalLayout = new QVBoxLayout(TextStyleWidgetClass);
        verticalLayout->setSpacing(6);
        verticalLayout->setMargin(11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        textStyleButton1 = new QToolButton(TextStyleWidgetClass);
        textStyleButton1->setObjectName(QString::fromUtf8("textStyleButton1"));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(textStyleButton1->sizePolicy().hasHeightForWidth());
        textStyleButton1->setSizePolicy(sizePolicy);
        textStyleButton1->setMinimumSize(QSize(100, 20));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        font.setPointSize(12);
        font.setBold(false);
        font.setItalic(false);
        font.setUnderline(false);
        font.setWeight(50);
        font.setStrikeOut(false);
        textStyleButton1->setFont(font);
        textStyleButton1->setCheckable(true);
        textStyleButton1->setChecked(true);
        textStyleButton1->setAutoExclusive(true);
        textStyleButton1->setAutoRaise(false);

        verticalLayout->addWidget(textStyleButton1);

        textStyleButton2 = new QToolButton(TextStyleWidgetClass);
        textStyleButton2->setObjectName(QString::fromUtf8("textStyleButton2"));
        sizePolicy.setHeightForWidth(textStyleButton2->sizePolicy().hasHeightForWidth());
        textStyleButton2->setSizePolicy(sizePolicy);
        textStyleButton2->setMinimumSize(QSize(100, 20));
        QFont font1;
        font1.setFamily(QString::fromUtf8("Verdana"));
        font1.setPointSize(12);
        font1.setBold(false);
        font1.setItalic(false);
        font1.setUnderline(false);
        font1.setWeight(50);
        font1.setStrikeOut(false);
        textStyleButton2->setFont(font1);
        textStyleButton2->setCheckable(true);
        textStyleButton2->setAutoExclusive(true);
        textStyleButton2->setAutoRaise(false);

        verticalLayout->addWidget(textStyleButton2);

        textStyleButton3 = new QToolButton(TextStyleWidgetClass);
        textStyleButton3->setObjectName(QString::fromUtf8("textStyleButton3"));
        sizePolicy.setHeightForWidth(textStyleButton3->sizePolicy().hasHeightForWidth());
        textStyleButton3->setSizePolicy(sizePolicy);
        textStyleButton3->setMinimumSize(QSize(100, 20));
        QFont font2;
        font2.setFamily(QString::fromUtf8("Comic Sans MS"));
        font2.setPointSize(12);
        font2.setBold(false);
        font2.setItalic(false);
        font2.setUnderline(false);
        font2.setWeight(50);
        font2.setStrikeOut(false);
        textStyleButton3->setFont(font2);
        textStyleButton3->setCheckable(true);
        textStyleButton3->setAutoExclusive(true);
        textStyleButton3->setAutoRaise(false);

        verticalLayout->addWidget(textStyleButton3);


        retranslateUi(TextStyleWidgetClass);

        QMetaObject::connectSlotsByName(TextStyleWidgetClass);
    } // setupUi

    void retranslateUi(QWidget *TextStyleWidgetClass)
    {
#ifndef QT_NO_ACCESSIBILITY
        textStyleButton1->setAccessibleName(QApplication::translate("TextStyleWidgetClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        textStyleButton1->setText(QApplication::translate("TextStyleWidgetClass", "..._DT", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        textStyleButton2->setAccessibleName(QApplication::translate("TextStyleWidgetClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        textStyleButton2->setText(QApplication::translate("TextStyleWidgetClass", "..._DT", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        textStyleButton3->setAccessibleName(QApplication::translate("TextStyleWidgetClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        textStyleButton3->setText(QApplication::translate("TextStyleWidgetClass", "..._DT", 0, QApplication::UnicodeUTF8));
        Q_UNUSED(TextStyleWidgetClass);
    } // retranslateUi

};

namespace Ui {
    class TextStyleWidgetClass: public Ui_TextStyleWidgetClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_TEXTSTYLEWIDGET_H
