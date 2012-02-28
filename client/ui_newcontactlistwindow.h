/********************************************************************************
** Form generated from reading ui file 'newcontactlistwindow.ui'
**
** Created: Tue May 5 10:34:48 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_NEWCONTACTLISTWINDOW_H
#define UI_NEWCONTACTLISTWINDOW_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QFrame>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include "newcontactlistgripwidget.h"
#include "newcontactlistwidget2.h"

QT_BEGIN_NAMESPACE

class Ui_NewContactListWindowClass
{
public:
    QVBoxLayout *verticalLayout_2;
    QFrame *styledContactsFrame;
    QHBoxLayout *horizontalLayout;
    NewContactListGripWidget *gripWidget;
    QVBoxLayout *verticalLayout_4;
    QFrame *buttonsSetupFrame;
    QVBoxLayout *verticalLayout_3;
    QHBoxLayout *horizontalLayout_5;
    QToolButton *findButton;
    QLabel *verticalLineLabel;
    QToolButton *setupToolButton;
    QFrame *contactListFrame;
    QVBoxLayout *verticalLayout;
    MoodBox::NewContactListWidget2 *newContactListWidget;

    void setupUi(QWidget *NewContactListWindowClass)
    {
        if (NewContactListWindowClass->objectName().isEmpty())
            NewContactListWindowClass->setObjectName(QString::fromUtf8("NewContactListWindowClass"));
        NewContactListWindowClass->resize(185, 388);
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(NewContactListWindowClass->sizePolicy().hasHeightForWidth());
        NewContactListWindowClass->setSizePolicy(sizePolicy);
        NewContactListWindowClass->setMinimumSize(QSize(185, 0));
        verticalLayout_2 = new QVBoxLayout(NewContactListWindowClass);
        verticalLayout_2->setSpacing(0);
        verticalLayout_2->setMargin(0);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        styledContactsFrame = new QFrame(NewContactListWindowClass);
        styledContactsFrame->setObjectName(QString::fromUtf8("styledContactsFrame"));
        styledContactsFrame->setFrameShape(QFrame::NoFrame);
        styledContactsFrame->setFrameShadow(QFrame::Plain);
        styledContactsFrame->setLineWidth(0);
        horizontalLayout = new QHBoxLayout(styledContactsFrame);
        horizontalLayout->setSpacing(0);
        horizontalLayout->setMargin(0);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalLayout->setContentsMargins(0, 0, 18, 6);
        gripWidget = new NewContactListGripWidget(styledContactsFrame);
        gripWidget->setObjectName(QString::fromUtf8("gripWidget"));
        QSizePolicy sizePolicy1(QSizePolicy::Fixed, QSizePolicy::Expanding);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(gripWidget->sizePolicy().hasHeightForWidth());
        gripWidget->setSizePolicy(sizePolicy1);
        gripWidget->setMinimumSize(QSize(6, 50));
        gripWidget->setMaximumSize(QSize(6, 10000));

        horizontalLayout->addWidget(gripWidget);

        verticalLayout_4 = new QVBoxLayout();
        verticalLayout_4->setSpacing(0);
        verticalLayout_4->setObjectName(QString::fromUtf8("verticalLayout_4"));
        buttonsSetupFrame = new QFrame(styledContactsFrame);
        buttonsSetupFrame->setObjectName(QString::fromUtf8("buttonsSetupFrame"));
        buttonsSetupFrame->setFrameShape(QFrame::NoFrame);
        buttonsSetupFrame->setFrameShadow(QFrame::Plain);
        buttonsSetupFrame->setLineWidth(0);
        verticalLayout_3 = new QVBoxLayout(buttonsSetupFrame);
        verticalLayout_3->setSpacing(0);
        verticalLayout_3->setMargin(0);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        verticalLayout_3->setContentsMargins(0, 6, 0, 6);
        horizontalLayout_5 = new QHBoxLayout();
        horizontalLayout_5->setSpacing(0);
        horizontalLayout_5->setObjectName(QString::fromUtf8("horizontalLayout_5"));
        horizontalLayout_5->setSizeConstraint(QLayout::SetDefaultConstraint);
        findButton = new QToolButton(buttonsSetupFrame);
        findButton->setObjectName(QString::fromUtf8("findButton"));
        QSizePolicy sizePolicy2(QSizePolicy::Expanding, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(findButton->sizePolicy().hasHeightForWidth());
        findButton->setSizePolicy(sizePolicy2);
        findButton->setFocusPolicy(Qt::NoFocus);
        findButton->setCheckable(true);
        findButton->setToolButtonStyle(Qt::ToolButtonTextOnly);

        horizontalLayout_5->addWidget(findButton);

        verticalLineLabel = new QLabel(buttonsSetupFrame);
        verticalLineLabel->setObjectName(QString::fromUtf8("verticalLineLabel"));
        verticalLineLabel->setMinimumSize(QSize(1, 0));
        verticalLineLabel->setLineWidth(0);
        verticalLineLabel->setPixmap(QPixmap(QString::fromUtf8(":/MoodBox/Resources/button_vertical_line.png")));
        verticalLineLabel->setScaledContents(true);

        horizontalLayout_5->addWidget(verticalLineLabel);

        setupToolButton = new QToolButton(buttonsSetupFrame);
        setupToolButton->setObjectName(QString::fromUtf8("setupToolButton"));
        sizePolicy2.setHeightForWidth(setupToolButton->sizePolicy().hasHeightForWidth());
        setupToolButton->setSizePolicy(sizePolicy2);
        setupToolButton->setFocusPolicy(Qt::NoFocus);
        setupToolButton->setCheckable(true);
        setupToolButton->setToolButtonStyle(Qt::ToolButtonTextOnly);

        horizontalLayout_5->addWidget(setupToolButton);


        verticalLayout_3->addLayout(horizontalLayout_5);


        verticalLayout_4->addWidget(buttonsSetupFrame);

        contactListFrame = new QFrame(styledContactsFrame);
        contactListFrame->setObjectName(QString::fromUtf8("contactListFrame"));
        contactListFrame->setFrameShape(QFrame::NoFrame);
        contactListFrame->setFrameShadow(QFrame::Plain);
        contactListFrame->setLineWidth(0);
        verticalLayout = new QVBoxLayout(contactListFrame);
        verticalLayout->setSpacing(0);
        verticalLayout->setMargin(0);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        newContactListWidget = new MoodBox::NewContactListWidget2(contactListFrame);
        newContactListWidget->setObjectName(QString::fromUtf8("newContactListWidget"));
        QSizePolicy sizePolicy3(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy3.setHorizontalStretch(0);
        sizePolicy3.setVerticalStretch(0);
        sizePolicy3.setHeightForWidth(newContactListWidget->sizePolicy().hasHeightForWidth());
        newContactListWidget->setSizePolicy(sizePolicy3);
        newContactListWidget->setAcceptDrops(true);
        newContactListWidget->setFrameShape(QFrame::NoFrame);
        newContactListWidget->setFrameShadow(QFrame::Plain);
        newContactListWidget->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        newContactListWidget->setEditTriggers(QAbstractItemView::NoEditTriggers);
        newContactListWidget->setDragDropMode(QAbstractItemView::DropOnly);
        newContactListWidget->setIconSize(QSize(10, 10));
        newContactListWidget->setResizeMode(QListView::Adjust);
        newContactListWidget->setLayoutMode(QListView::Batched);
        newContactListWidget->setUniformItemSizes(true);

        verticalLayout->addWidget(newContactListWidget);


        verticalLayout_4->addWidget(contactListFrame);


        horizontalLayout->addLayout(verticalLayout_4);


        verticalLayout_2->addWidget(styledContactsFrame);


        retranslateUi(NewContactListWindowClass);

        QMetaObject::connectSlotsByName(NewContactListWindowClass);
    } // setupUi

    void retranslateUi(QWidget *NewContactListWindowClass)
    {
        Q_UNUSED(NewContactListWindowClass);
    } // retranslateUi

};

namespace Ui {
    class NewContactListWindowClass: public Ui_NewContactListWindowClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_NEWCONTACTLISTWINDOW_H
