/********************************************************************************
** Form generated from reading ui file 'statusbutton.ui'
**
** Created: Tue May 5 10:34:48 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_STATUSBUTTON_H
#define UI_STATUSBUTTON_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QSpacerItem>
#include <QtGui/QToolButton>

QT_BEGIN_NAMESPACE

class Ui_StatusButtonClass
{
public:
    QHBoxLayout *horizontalLayout;
    QSpacerItem *horizontalSpacer_2;
    QLabel *iconLabel;
    QLabel *textLabel;
    QLabel *arrowLabel;
    QSpacerItem *horizontalSpacer;

    void setupUi(QToolButton *StatusButtonClass)
    {
        if (StatusButtonClass->objectName().isEmpty())
            StatusButtonClass->setObjectName(QString::fromUtf8("StatusButtonClass"));
        StatusButtonClass->resize(173, 34);
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(StatusButtonClass->sizePolicy().hasHeightForWidth());
        StatusButtonClass->setSizePolicy(sizePolicy);
        StatusButtonClass->setMaximumSize(QSize(16777215, 34));
        StatusButtonClass->setFocusPolicy(Qt::NoFocus);
        StatusButtonClass->setPopupMode(QToolButton::InstantPopup);
        StatusButtonClass->setArrowType(Qt::NoArrow);
        horizontalLayout = new QHBoxLayout(StatusButtonClass);
        horizontalLayout->setSpacing(6);
        horizontalLayout->setMargin(0);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalSpacer_2 = new QSpacerItem(8, 5, QSizePolicy::Fixed, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer_2);

        iconLabel = new QLabel(StatusButtonClass);
        iconLabel->setObjectName(QString::fromUtf8("iconLabel"));
        iconLabel->setMaximumSize(QSize(16, 16));

        horizontalLayout->addWidget(iconLabel);

        textLabel = new QLabel(StatusButtonClass);
        textLabel->setObjectName(QString::fromUtf8("textLabel"));

        horizontalLayout->addWidget(textLabel);

        arrowLabel = new QLabel(StatusButtonClass);
        arrowLabel->setObjectName(QString::fromUtf8("arrowLabel"));

        horizontalLayout->addWidget(arrowLabel);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);

        iconLabel->raise();
        textLabel->raise();
        arrowLabel->raise();

        retranslateUi(StatusButtonClass);

        QMetaObject::connectSlotsByName(StatusButtonClass);
    } // setupUi

    void retranslateUi(QToolButton *StatusButtonClass)
    {
        iconLabel->setText(QApplication::translate("StatusButtonClass", "TextLabel_DT", 0, QApplication::UnicodeUTF8));
        textLabel->setText(QApplication::translate("StatusButtonClass", "TextLabel_DT", 0, QApplication::UnicodeUTF8));
        arrowLabel->setText(QApplication::translate("StatusButtonClass", "TextLabel_DT", 0, QApplication::UnicodeUTF8));
        Q_UNUSED(StatusButtonClass);
    } // retranslateUi

};

namespace Ui {
    class StatusButtonClass: public Ui_StatusButtonClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_STATUSBUTTON_H
