/********************************************************************************
** Form generated from reading ui file 'palettelistwidgetitem.ui'
**
** Created: Tue May 5 10:34:48 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_PALETTELISTWIDGETITEM_H
#define UI_PALETTELISTWIDGETITEM_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QSpacerItem>
#include <QtGui/QToolButton>
#include <QtGui/QWidget>
#include "paletteviewwidget.h"

QT_BEGIN_NAMESPACE

class Ui_PaletteListWidgetItemClass
{
public:
    QGridLayout *gridLayout;
    MoodBox::PaletteViewWidget *paletteWidget;
    QWidget *removeButtonHost;
    QGridLayout *gridLayout_2;
    QToolButton *addPaletteButton;
    QSpacerItem *horizontalSpacer;
    QToolButton *deletePaletteButton;
    QLabel *nameLabel;

    void setupUi(QFrame *PaletteListWidgetItemClass)
    {
        if (PaletteListWidgetItemClass->objectName().isEmpty())
            PaletteListWidgetItemClass->setObjectName(QString::fromUtf8("PaletteListWidgetItemClass"));
        PaletteListWidgetItemClass->resize(232, 70);
        PaletteListWidgetItemClass->setLineWidth(0);
        gridLayout = new QGridLayout(PaletteListWidgetItemClass);
        gridLayout->setSpacing(0);
        gridLayout->setMargin(0);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        gridLayout->setHorizontalSpacing(2);
        gridLayout->setVerticalSpacing(0);
        gridLayout->setContentsMargins(20, 4, 2, 5);
        paletteWidget = new MoodBox::PaletteViewWidget(PaletteListWidgetItemClass);
        paletteWidget->setObjectName(QString::fromUtf8("paletteWidget"));
        paletteWidget->setMinimumSize(QSize(163, 41));
        paletteWidget->setMaximumSize(QSize(163, 41));

        gridLayout->addWidget(paletteWidget, 0, 0, 1, 1);

        removeButtonHost = new QWidget(PaletteListWidgetItemClass);
        removeButtonHost->setObjectName(QString::fromUtf8("removeButtonHost"));
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(removeButtonHost->sizePolicy().hasHeightForWidth());
        removeButtonHost->setSizePolicy(sizePolicy);
        removeButtonHost->setMinimumSize(QSize(45, 20));
        gridLayout_2 = new QGridLayout(removeButtonHost);
        gridLayout_2->setSpacing(2);
        gridLayout_2->setMargin(0);
        gridLayout_2->setObjectName(QString::fromUtf8("gridLayout_2"));
        gridLayout_2->setContentsMargins(2, 2, 0, 2);
        addPaletteButton = new QToolButton(removeButtonHost);
        addPaletteButton->setObjectName(QString::fromUtf8("addPaletteButton"));
        addPaletteButton->setMinimumSize(QSize(22, 22));
        addPaletteButton->setFocusPolicy(Qt::NoFocus);
        addPaletteButton->setIconSize(QSize(22, 22));

        gridLayout_2->addWidget(addPaletteButton, 0, 2, 1, 1);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout_2->addItem(horizontalSpacer, 0, 0, 1, 1);

        deletePaletteButton = new QToolButton(removeButtonHost);
        deletePaletteButton->setObjectName(QString::fromUtf8("deletePaletteButton"));
        deletePaletteButton->setMinimumSize(QSize(22, 22));
        deletePaletteButton->setIconSize(QSize(22, 22));

        gridLayout_2->addWidget(deletePaletteButton, 0, 1, 1, 1);


        gridLayout->addWidget(removeButtonHost, 0, 1, 1, 2);

        nameLabel = new QLabel(PaletteListWidgetItemClass);
        nameLabel->setObjectName(QString::fromUtf8("nameLabel"));
        nameLabel->setAlignment(Qt::AlignCenter|Qt::AlignHCenter|Qt::AlignLeading|Qt::AlignLeft|Qt::AlignVCenter);

        gridLayout->addWidget(nameLabel, 1, 0, 1, 1);


        retranslateUi(PaletteListWidgetItemClass);

        QMetaObject::connectSlotsByName(PaletteListWidgetItemClass);
    } // setupUi

    void retranslateUi(QFrame *PaletteListWidgetItemClass)
    {
#ifndef QT_NO_ACCESSIBILITY
        addPaletteButton->setAccessibleName(QApplication::translate("PaletteListWidgetItemClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        deletePaletteButton->setAccessibleName(QApplication::translate("PaletteListWidgetItemClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        Q_UNUSED(PaletteListWidgetItemClass);
    } // retranslateUi

};

namespace Ui {
    class PaletteListWidgetItemClass: public Ui_PaletteListWidgetItemClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_PALETTELISTWIDGETITEM_H
