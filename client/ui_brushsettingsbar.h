/********************************************************************************
** Form generated from reading ui file 'brushsettingsbar.ui'
**
** Created: Tue May 5 10:34:35 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_BRUSHSETTINGSBAR_H
#define UI_BRUSHSETTINGSBAR_H

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
#include "brushstylebuttons.h"
#include "palettewidget2.h"

QT_BEGIN_NAMESPACE

class Ui_BrushSettingsBarClass
{
public:
    QVBoxLayout *verticalLayout;
    QHBoxLayout *horizontalLayout;
    MoodBox::PaletteWidget2 *paletteWidget;
    QHBoxLayout *horizontalLayout_2;
    QToolButton *paletteManagerButton;
    QSpacerItem *verticalSpacer;
    QHBoxLayout *horizontalLayout_3;
    MoodBox::BrushSizeButton *sizeButton;
    QHBoxLayout *horizontalLayout_4;
    MoodBox::BrushAlphaButton *alphaButton;

    void setupUi(QWidget *BrushSettingsBarClass)
    {
        if (BrushSettingsBarClass->objectName().isEmpty())
            BrushSettingsBarClass->setObjectName(QString::fromUtf8("BrushSettingsBarClass"));
        BrushSettingsBarClass->resize(82, 240);
        verticalLayout = new QVBoxLayout(BrushSettingsBarClass);
        verticalLayout->setSpacing(0);
        verticalLayout->setMargin(11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        verticalLayout->setContentsMargins(0, 0, -1, 0);
        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        paletteWidget = new MoodBox::PaletteWidget2(BrushSettingsBarClass);
        paletteWidget->setObjectName(QString::fromUtf8("paletteWidget"));
        paletteWidget->setMinimumSize(QSize(47, 139));
        paletteWidget->setMaximumSize(QSize(47, 139));

        horizontalLayout->addWidget(paletteWidget);


        verticalLayout->addLayout(horizontalLayout);

        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setSpacing(0);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        paletteManagerButton = new QToolButton(BrushSettingsBarClass);
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
        sizeButton = new MoodBox::BrushSizeButton(BrushSettingsBarClass);
        sizeButton->setObjectName(QString::fromUtf8("sizeButton"));
        sizeButton->setMinimumSize(QSize(41, 29));
        sizeButton->setMaximumSize(QSize(41, 29));
        sizeButton->setFocusPolicy(Qt::NoFocus);
        sizeButton->setIconSize(QSize(41, 29));

        horizontalLayout_3->addWidget(sizeButton);


        verticalLayout->addLayout(horizontalLayout_3);

        horizontalLayout_4 = new QHBoxLayout();
        horizontalLayout_4->setSpacing(0);
        horizontalLayout_4->setObjectName(QString::fromUtf8("horizontalLayout_4"));
        alphaButton = new MoodBox::BrushAlphaButton(BrushSettingsBarClass);
        alphaButton->setObjectName(QString::fromUtf8("alphaButton"));
        alphaButton->setMinimumSize(QSize(41, 29));
        alphaButton->setMaximumSize(QSize(41, 29));
        alphaButton->setFocusPolicy(Qt::NoFocus);
        alphaButton->setIconSize(QSize(41, 29));

        horizontalLayout_4->addWidget(alphaButton);


        verticalLayout->addLayout(horizontalLayout_4);


        retranslateUi(BrushSettingsBarClass);

        QMetaObject::connectSlotsByName(BrushSettingsBarClass);
    } // setupUi

    void retranslateUi(QWidget *BrushSettingsBarClass)
    {
#ifndef QT_NO_TOOLTIP
        paletteWidget->setToolTip(QApplication::translate("BrushSettingsBarClass", "ColorsWidgetHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        paletteManagerButton->setToolTip(QApplication::translate("BrushSettingsBarClass", "PalettesButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        sizeButton->setToolTip(QApplication::translate("BrushSettingsBarClass", "SizeButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        alphaButton->setToolTip(QApplication::translate("BrushSettingsBarClass", "TransparencyButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
        Q_UNUSED(BrushSettingsBarClass);
    } // retranslateUi

};

namespace Ui {
    class BrushSettingsBarClass: public Ui_BrushSettingsBarClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_BRUSHSETTINGSBAR_H
