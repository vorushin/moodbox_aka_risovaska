/********************************************************************************
** Form generated from reading ui file 'contactlistmenu.ui'
**
** Created: Tue May 5 10:34:39 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_CONTACTLISTMENU_H
#define UI_CONTACTLISTMENU_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QFrame>
#include <QtGui/QHeaderView>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_ContactListMenuClass
{
public:
    QVBoxLayout *verticalLayout;
    QFrame *menuFrame;
    QVBoxLayout *verticalLayout_2;
    QToolButton *onlineButton;
    QToolButton *offlineButton;
    QFrame *separatorFrame;
    QToolButton *logoutButton;
    QToolButton *exitButton;

    void setupUi(QWidget *ContactListMenuClass)
    {
        if (ContactListMenuClass->objectName().isEmpty())
            ContactListMenuClass->setObjectName(QString::fromUtf8("ContactListMenuClass"));
        ContactListMenuClass->resize(176, 130);
        verticalLayout = new QVBoxLayout(ContactListMenuClass);
        verticalLayout->setSpacing(0);
        verticalLayout->setMargin(0);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        menuFrame = new QFrame(ContactListMenuClass);
        menuFrame->setObjectName(QString::fromUtf8("menuFrame"));
        menuFrame->setFrameShape(QFrame::NoFrame);
        menuFrame->setFrameShadow(QFrame::Plain);
        menuFrame->setLineWidth(0);
        verticalLayout_2 = new QVBoxLayout(menuFrame);
        verticalLayout_2->setSpacing(0);
        verticalLayout_2->setMargin(0);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        onlineButton = new QToolButton(menuFrame);
        onlineButton->setObjectName(QString::fromUtf8("onlineButton"));
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(onlineButton->sizePolicy().hasHeightForWidth());
        onlineButton->setSizePolicy(sizePolicy);
        onlineButton->setMinimumSize(QSize(0, 32));
        onlineButton->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);

        verticalLayout_2->addWidget(onlineButton);

        offlineButton = new QToolButton(menuFrame);
        offlineButton->setObjectName(QString::fromUtf8("offlineButton"));
        sizePolicy.setHeightForWidth(offlineButton->sizePolicy().hasHeightForWidth());
        offlineButton->setSizePolicy(sizePolicy);
        offlineButton->setMinimumSize(QSize(0, 32));
        offlineButton->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);

        verticalLayout_2->addWidget(offlineButton);

        separatorFrame = new QFrame(menuFrame);
        separatorFrame->setObjectName(QString::fromUtf8("separatorFrame"));
        separatorFrame->setMinimumSize(QSize(0, 2));
        separatorFrame->setMaximumSize(QSize(16777215, 2));
        separatorFrame->setFrameShape(QFrame::Box);
        separatorFrame->setFrameShadow(QFrame::Plain);

        verticalLayout_2->addWidget(separatorFrame);

        logoutButton = new QToolButton(menuFrame);
        logoutButton->setObjectName(QString::fromUtf8("logoutButton"));
        sizePolicy.setHeightForWidth(logoutButton->sizePolicy().hasHeightForWidth());
        logoutButton->setSizePolicy(sizePolicy);
        logoutButton->setMinimumSize(QSize(0, 32));
        logoutButton->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);

        verticalLayout_2->addWidget(logoutButton);

        exitButton = new QToolButton(menuFrame);
        exitButton->setObjectName(QString::fromUtf8("exitButton"));
        sizePolicy.setHeightForWidth(exitButton->sizePolicy().hasHeightForWidth());
        exitButton->setSizePolicy(sizePolicy);
        exitButton->setMinimumSize(QSize(0, 32));
        exitButton->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);

        verticalLayout_2->addWidget(exitButton);


        verticalLayout->addWidget(menuFrame);


        retranslateUi(ContactListMenuClass);

        QMetaObject::connectSlotsByName(ContactListMenuClass);
    } // setupUi

    void retranslateUi(QWidget *ContactListMenuClass)
    {
        onlineButton->setText(QString());
        Q_UNUSED(ContactListMenuClass);
    } // retranslateUi

};

namespace Ui {
    class ContactListMenuClass: public Ui_ContactListMenuClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CONTACTLISTMENU_H
