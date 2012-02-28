/********************************************************************************
** Form generated from reading ui file 'textsettingsbar.ui'
**
** Created: Tue May 5 10:34:55 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_TEXTSETTINGSBAR_H
#define UI_TEXTSETTINGSBAR_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QSpacerItem>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include "palettewidget2.h"
#include "textstylebutton.h"

QT_BEGIN_NAMESPACE

class Ui_TextSettingsBarClass
{
public:
    QVBoxLayout *verticalLayout;
    QHBoxLayout *horizontalLayout;
    MoodBox::PaletteWidget2 *paletteWidget;
    QHBoxLayout *horizontalLayout_2;
    QToolButton *paletteManagerButton;
    QSpacerItem *verticalSpacer;
    QHBoxLayout *horizontalLayout_3;
    QToolButton *italicButton;
    QToolButton *boldButton;
    QToolButton *underlineButton;
    QHBoxLayout *horizontalLayout_4;
    MoodBox::TextStyleButton *styleButton;

    void setupUi(QWidget *TextSettingsBarClass)
    {
        if (TextSettingsBarClass->objectName().isEmpty())
            TextSettingsBarClass->setObjectName(QString::fromUtf8("TextSettingsBarClass"));
        TextSettingsBarClass->resize(82, 240);
        verticalLayout = new QVBoxLayout(TextSettingsBarClass);
        verticalLayout->setSpacing(0);
        verticalLayout->setMargin(11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        verticalLayout->setContentsMargins(0, 0, -1, 0);
        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        paletteWidget = new MoodBox::PaletteWidget2(TextSettingsBarClass);
        paletteWidget->setObjectName(QString::fromUtf8("paletteWidget"));
        paletteWidget->setMinimumSize(QSize(47, 139));
        paletteWidget->setMaximumSize(QSize(47, 139));

        horizontalLayout->addWidget(paletteWidget);


        verticalLayout->addLayout(horizontalLayout);

        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setSpacing(6);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        paletteManagerButton = new QToolButton(TextSettingsBarClass);
        paletteManagerButton->setObjectName(QString::fromUtf8("paletteManagerButton"));
        paletteManagerButton->setMinimumSize(QSize(47, 24));
        paletteManagerButton->setMaximumSize(QSize(47, 24));
        paletteManagerButton->setFocusPolicy(Qt::NoFocus);
        paletteManagerButton->setCheckable(true);
        paletteManagerButton->setArrowType(Qt::NoArrow);

        horizontalLayout_2->addWidget(paletteManagerButton);


        verticalLayout->addLayout(horizontalLayout_2);

        verticalSpacer = new QSpacerItem(20, 10, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout->addItem(verticalSpacer);

        horizontalLayout_3 = new QHBoxLayout();
        horizontalLayout_3->setSpacing(0);
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        horizontalLayout_3->setContentsMargins(-1, -1, -1, 0);
        italicButton = new QToolButton(TextSettingsBarClass);
        italicButton->setObjectName(QString::fromUtf8("italicButton"));
        QFont font;
        font.setFamily(QString::fromUtf8("MS Shell Dlg 2"));
        font.setPointSize(8);
        font.setBold(false);
        font.setItalic(true);
        font.setUnderline(false);
        font.setWeight(50);
        font.setStrikeOut(false);
        italicButton->setFont(font);
        italicButton->setFocusPolicy(Qt::NoFocus);
        italicButton->setCheckable(false);

        horizontalLayout_3->addWidget(italicButton);

        boldButton = new QToolButton(TextSettingsBarClass);
        boldButton->setObjectName(QString::fromUtf8("boldButton"));
        QFont font1;
        font1.setFamily(QString::fromUtf8("MS Shell Dlg 2"));
        font1.setPointSize(8);
        font1.setBold(true);
        font1.setItalic(false);
        font1.setUnderline(false);
        font1.setWeight(75);
        font1.setStrikeOut(false);
        boldButton->setFont(font1);
        boldButton->setFocusPolicy(Qt::NoFocus);
        boldButton->setCheckable(false);

        horizontalLayout_3->addWidget(boldButton);

        underlineButton = new QToolButton(TextSettingsBarClass);
        underlineButton->setObjectName(QString::fromUtf8("underlineButton"));
        QFont font2;
        font2.setFamily(QString::fromUtf8("MS Shell Dlg 2"));
        font2.setPointSize(8);
        font2.setBold(false);
        font2.setItalic(false);
        font2.setUnderline(true);
        font2.setWeight(50);
        font2.setStrikeOut(false);
        underlineButton->setFont(font2);
        underlineButton->setFocusPolicy(Qt::NoFocus);

        horizontalLayout_3->addWidget(underlineButton);


        verticalLayout->addLayout(horizontalLayout_3);

        horizontalLayout_4 = new QHBoxLayout();
        horizontalLayout_4->setSpacing(0);
        horizontalLayout_4->setObjectName(QString::fromUtf8("horizontalLayout_4"));
        horizontalLayout_4->setContentsMargins(2, -1, -1, -1);
        styleButton = new MoodBox::TextStyleButton(TextSettingsBarClass);
        styleButton->setObjectName(QString::fromUtf8("styleButton"));
        styleButton->setMinimumSize(QSize(70, 30));
        styleButton->setFocusPolicy(Qt::NoFocus);

        horizontalLayout_4->addWidget(styleButton);


        verticalLayout->addLayout(horizontalLayout_4);


        retranslateUi(TextSettingsBarClass);

        QMetaObject::connectSlotsByName(TextSettingsBarClass);
    } // setupUi

    void retranslateUi(QWidget *TextSettingsBarClass)
    {
#ifndef QT_NO_TOOLTIP
        paletteWidget->setToolTip(QApplication::translate("TextSettingsBarClass", "ColorsWidgetHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        paletteManagerButton->setToolTip(QApplication::translate("TextSettingsBarClass", "PalettesButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        paletteManagerButton->setAccessibleName(QApplication::translate("TextSettingsBarClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_TOOLTIP
        italicButton->setToolTip(QApplication::translate("TextSettingsBarClass", "ItalicButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        italicButton->setAccessibleName(QApplication::translate("TextSettingsBarClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        italicButton->setText(QApplication::translate("TextSettingsBarClass", "I", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        boldButton->setToolTip(QApplication::translate("TextSettingsBarClass", "BoldButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        boldButton->setAccessibleName(QApplication::translate("TextSettingsBarClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        boldButton->setText(QApplication::translate("TextSettingsBarClass", "B", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        underlineButton->setToolTip(QApplication::translate("TextSettingsBarClass", "UnderlineButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        underlineButton->setAccessibleName(QApplication::translate("TextSettingsBarClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        underlineButton->setText(QApplication::translate("TextSettingsBarClass", "U", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        styleButton->setToolTip(QApplication::translate("TextSettingsBarClass", "FontButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        styleButton->setAccessibleName(QApplication::translate("TextSettingsBarClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        Q_UNUSED(TextSettingsBarClass);
    } // retranslateUi

};

namespace Ui {
    class TextSettingsBarClass: public Ui_TextSettingsBarClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_TEXTSETTINGSBAR_H
