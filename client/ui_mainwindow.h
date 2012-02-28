/********************************************************************************
** Form generated from reading ui file 'mainwindow.ui'
**
** Created: Tue May 5 10:34:45 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_MAINWINDOW_H
#define UI_MAINWINDOW_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QFrame>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QMainWindow>
#include <QtGui/QSpacerItem>
#include <QtGui/QStackedWidget>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include "contactlistbutton.h"
#include "drawbutton.h"
#include "forgotpasswordwidget.h"
#include "infowidget.h"
#include "invitecodewidget.h"
#include "logonwidget.h"
#include "registrationwidget.h"
#include "tvwidget.h"
#include "waitingwidget.h"
#include "welcomewidget.h"

QT_BEGIN_NAMESPACE

class Ui_MainWindowClass
{
public:
    QWidget *centralWidget;
    QVBoxLayout *verticalLayout_2;
    QFrame *tvFrame;
    QVBoxLayout *verticalLayout;
    QFrame *closeFrame;
    QHBoxLayout *horizontalLayout;
    QSpacerItem *horizontalSpacer;
    QToolButton *tvCloseButton;
    QSpacerItem *horizontalSpacer_2;
    QFrame *tvBorderMainFrame;
    QVBoxLayout *verticalLayout_5;
    QStackedWidget *stackedWidget;
    MoodBox::LogonWidget *logonWidget;
    MoodBox::RegistrationWidget *registrationWidget;
    MoodBox::ForgotPasswordWidget *forgotPasswordWidget;
    MoodBox::WelcomeWidget *welcomeWidget;
    MoodBox::TVWidget *tvWidget;
    MoodBox::InviteCodeWidget *inviteCodeWidget;
    MoodBox::InfoWidget *infoWidget;
    MoodBox::WaitingWidget *waitingWidget;
    QFrame *frame;
    QVBoxLayout *verticalLayout_3;
    QFrame *buttonsFrame;
    QHBoxLayout *horizontalLayout_3;
    MoodBox::ContactListButton *contactListButton;
    QSpacerItem *horizontalSpacer_3;
    MoodBox::DrawButton *drawButton;
    QSpacerItem *horizontalSpacer_4;
    QToolButton *previousButton;
    QToolButton *nextButton;
    QSpacerItem *horizontalSpacer_5;
    QToolButton *historyButton;

    void setupUi(QMainWindow *MainWindowClass)
    {
        if (MainWindowClass->objectName().isEmpty())
            MainWindowClass->setObjectName(QString::fromUtf8("MainWindowClass"));
        MainWindowClass->resize(406, 356);
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(MainWindowClass->sizePolicy().hasHeightForWidth());
        MainWindowClass->setSizePolicy(sizePolicy);
        MainWindowClass->setAnimated(false);
        centralWidget = new QWidget(MainWindowClass);
        centralWidget->setObjectName(QString::fromUtf8("centralWidget"));
        verticalLayout_2 = new QVBoxLayout(centralWidget);
        verticalLayout_2->setSpacing(0);
        verticalLayout_2->setMargin(0);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        tvFrame = new QFrame(centralWidget);
        tvFrame->setObjectName(QString::fromUtf8("tvFrame"));
        tvFrame->setFrameShape(QFrame::NoFrame);
        tvFrame->setFrameShadow(QFrame::Plain);
        tvFrame->setLineWidth(0);
        verticalLayout = new QVBoxLayout(tvFrame);
        verticalLayout->setSpacing(0);
        verticalLayout->setMargin(0);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        verticalLayout->setContentsMargins(0, 0, 0, 10);
        closeFrame = new QFrame(tvFrame);
        closeFrame->setObjectName(QString::fromUtf8("closeFrame"));
        closeFrame->setFrameShape(QFrame::NoFrame);
        closeFrame->setFrameShadow(QFrame::Plain);
        closeFrame->setLineWidth(0);
        horizontalLayout = new QHBoxLayout(closeFrame);
        horizontalLayout->setSpacing(0);
        horizontalLayout->setMargin(0);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalSpacer = new QSpacerItem(40, 10, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);

        tvCloseButton = new QToolButton(closeFrame);
        tvCloseButton->setObjectName(QString::fromUtf8("tvCloseButton"));
        tvCloseButton->setMaximumSize(QSize(16777215, 14));
        tvCloseButton->setFocusPolicy(Qt::NoFocus);

        horizontalLayout->addWidget(tvCloseButton);

        horizontalSpacer_2 = new QSpacerItem(14, 10, QSizePolicy::Fixed, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer_2);


        verticalLayout->addWidget(closeFrame);

        tvBorderMainFrame = new QFrame(tvFrame);
        tvBorderMainFrame->setObjectName(QString::fromUtf8("tvBorderMainFrame"));
        tvBorderMainFrame->setFrameShape(QFrame::NoFrame);
        tvBorderMainFrame->setFrameShadow(QFrame::Plain);
        tvBorderMainFrame->setLineWidth(0);
        verticalLayout_5 = new QVBoxLayout(tvBorderMainFrame);
        verticalLayout_5->setSpacing(0);
        verticalLayout_5->setMargin(0);
        verticalLayout_5->setObjectName(QString::fromUtf8("verticalLayout_5"));
        stackedWidget = new QStackedWidget(tvBorderMainFrame);
        stackedWidget->setObjectName(QString::fromUtf8("stackedWidget"));
        sizePolicy.setHeightForWidth(stackedWidget->sizePolicy().hasHeightForWidth());
        stackedWidget->setSizePolicy(sizePolicy);
        stackedWidget->setMinimumSize(QSize(388, 267));
        stackedWidget->setMaximumSize(QSize(388, 267));
        stackedWidget->setLineWidth(0);
        logonWidget = new MoodBox::LogonWidget();
        logonWidget->setObjectName(QString::fromUtf8("logonWidget"));
        stackedWidget->addWidget(logonWidget);
        registrationWidget = new MoodBox::RegistrationWidget();
        registrationWidget->setObjectName(QString::fromUtf8("registrationWidget"));
        stackedWidget->addWidget(registrationWidget);
        forgotPasswordWidget = new MoodBox::ForgotPasswordWidget();
        forgotPasswordWidget->setObjectName(QString::fromUtf8("forgotPasswordWidget"));
        stackedWidget->addWidget(forgotPasswordWidget);
        welcomeWidget = new MoodBox::WelcomeWidget();
        welcomeWidget->setObjectName(QString::fromUtf8("welcomeWidget"));
        stackedWidget->addWidget(welcomeWidget);
        tvWidget = new MoodBox::TVWidget();
        tvWidget->setObjectName(QString::fromUtf8("tvWidget"));
        stackedWidget->addWidget(tvWidget);
        inviteCodeWidget = new MoodBox::InviteCodeWidget();
        inviteCodeWidget->setObjectName(QString::fromUtf8("inviteCodeWidget"));
        stackedWidget->addWidget(inviteCodeWidget);
        infoWidget = new MoodBox::InfoWidget();
        infoWidget->setObjectName(QString::fromUtf8("infoWidget"));
        stackedWidget->addWidget(infoWidget);
        waitingWidget = new MoodBox::WaitingWidget();
        waitingWidget->setObjectName(QString::fromUtf8("waitingWidget"));
        stackedWidget->addWidget(waitingWidget);

        verticalLayout_5->addWidget(stackedWidget);


        verticalLayout->addWidget(tvBorderMainFrame);

        frame = new QFrame(tvFrame);
        frame->setObjectName(QString::fromUtf8("frame"));
        frame->setMinimumSize(QSize(0, 47));
        frame->setMaximumSize(QSize(16777215, 47));
        frame->setFrameShape(QFrame::NoFrame);
        frame->setFrameShadow(QFrame::Plain);
        frame->setLineWidth(0);
        verticalLayout_3 = new QVBoxLayout(frame);
        verticalLayout_3->setSpacing(0);
        verticalLayout_3->setMargin(0);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        buttonsFrame = new QFrame(frame);
        buttonsFrame->setObjectName(QString::fromUtf8("buttonsFrame"));
        buttonsFrame->setMinimumSize(QSize(0, 47));
        buttonsFrame->setMaximumSize(QSize(16777215, 56));
        buttonsFrame->setFrameShape(QFrame::NoFrame);
        buttonsFrame->setFrameShadow(QFrame::Plain);
        buttonsFrame->setLineWidth(0);
        horizontalLayout_3 = new QHBoxLayout(buttonsFrame);
        horizontalLayout_3->setSpacing(0);
        horizontalLayout_3->setMargin(0);
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        horizontalLayout_3->setContentsMargins(-1, 0, -1, 0);
        contactListButton = new MoodBox::ContactListButton(buttonsFrame);
        contactListButton->setObjectName(QString::fromUtf8("contactListButton"));
        contactListButton->setMinimumSize(QSize(39, 36));
        contactListButton->setFocusPolicy(Qt::NoFocus);
        contactListButton->setAcceptDrops(true);
        contactListButton->setCheckable(true);

        horizontalLayout_3->addWidget(contactListButton);

        horizontalSpacer_3 = new QSpacerItem(45, 10, QSizePolicy::Fixed, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_3);

        drawButton = new MoodBox::DrawButton(buttonsFrame);
        drawButton->setObjectName(QString::fromUtf8("drawButton"));
        QSizePolicy sizePolicy1(QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(drawButton->sizePolicy().hasHeightForWidth());
        drawButton->setSizePolicy(sizePolicy1);
        drawButton->setFocusPolicy(Qt::NoFocus);
        drawButton->setAcceptDrops(true);
        drawButton->setCheckable(true);

        horizontalLayout_3->addWidget(drawButton);

        horizontalSpacer_4 = new QSpacerItem(10, 24, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_4);

        previousButton = new QToolButton(buttonsFrame);
        previousButton->setObjectName(QString::fromUtf8("previousButton"));
        previousButton->setFocusPolicy(Qt::NoFocus);

        horizontalLayout_3->addWidget(previousButton);

        nextButton = new QToolButton(buttonsFrame);
        nextButton->setObjectName(QString::fromUtf8("nextButton"));
        nextButton->setFocusPolicy(Qt::NoFocus);

        horizontalLayout_3->addWidget(nextButton);

        horizontalSpacer_5 = new QSpacerItem(25, 125, QSizePolicy::Fixed, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_5);

        historyButton = new QToolButton(buttonsFrame);
        historyButton->setObjectName(QString::fromUtf8("historyButton"));
        historyButton->setFocusPolicy(Qt::NoFocus);
        historyButton->setCheckable(true);

        horizontalLayout_3->addWidget(historyButton);


        verticalLayout_3->addWidget(buttonsFrame);


        verticalLayout->addWidget(frame);


        verticalLayout_2->addWidget(tvFrame);

        MainWindowClass->setCentralWidget(centralWidget);

        retranslateUi(MainWindowClass);

        stackedWidget->setCurrentIndex(7);


        QMetaObject::connectSlotsByName(MainWindowClass);
    } // setupUi

    void retranslateUi(QMainWindow *MainWindowClass)
    {
#ifndef QT_NO_TOOLTIP
        tvCloseButton->setToolTip(QApplication::translate("MainWindowClass", "CloseButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        tvCloseButton->setAccessibleName(QApplication::translate("MainWindowClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_TOOLTIP
        contactListButton->setToolTip(QApplication::translate("MainWindowClass", "ContactsButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        contactListButton->setAccessibleName(QApplication::translate("MainWindowClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_TOOLTIP
        drawButton->setToolTip(QApplication::translate("MainWindowClass", "DrawButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        drawButton->setAccessibleName(QApplication::translate("MainWindowClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_TOOLTIP
        previousButton->setToolTip(QApplication::translate("MainWindowClass", "PrevButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        previousButton->setAccessibleName(QApplication::translate("MainWindowClass", "roundButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_TOOLTIP
        nextButton->setToolTip(QApplication::translate("MainWindowClass", "NextButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        nextButton->setAccessibleName(QApplication::translate("MainWindowClass", "roundButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_TOOLTIP
        historyButton->setToolTip(QApplication::translate("MainWindowClass", "HistoryButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        historyButton->setAccessibleName(QApplication::translate("MainWindowClass", "roundButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        Q_UNUSED(MainWindowClass);
    } // retranslateUi

};

namespace Ui {
    class MainWindowClass: public Ui_MainWindowClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_MAINWINDOW_H
