/********************************************************************************
** Form generated from reading ui file 'finddialog.ui'
**
** Created: Tue May 5 10:34:41 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_FINDDIALOG_H
#define UI_FINDDIALOG_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialog>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QTabWidget>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include "findchannelframe.h"
#include "findpeopleframe.h"

QT_BEGIN_NAMESPACE

class Ui_FindDialogClass
{
public:
    QVBoxLayout *verticalLayout_2;
    QFrame *styledFrame;
    QVBoxLayout *frameVLayout;
    QHBoxLayout *horizontalLayout_2;
    QSpacerItem *horizontalSpacer_4;
    QToolButton *closeToolButton;
    QTabWidget *findTabWidget;
    QWidget *friendsTab;
    QGridLayout *gridLayout;
    MoodBox::FindPeopleFrame *findPeopleFrame;
    QWidget *channelsTab;
    QVBoxLayout *verticalLayout;
    MoodBox::FindChannelFrame *findChannelFrame;
    QFrame *frameBottom;
    QHBoxLayout *buttonsHLayout;
    QSpacerItem *horizontalSpacer_2;
    QPushButton *addUserButton;
    QPushButton *closeButton;
    QHBoxLayout *horizontalLayout;
    QSpacerItem *horizontalSpacer_3;
    QWidget *widget;

    void setupUi(QDialog *FindDialogClass)
    {
        if (FindDialogClass->objectName().isEmpty())
            FindDialogClass->setObjectName(QString::fromUtf8("FindDialogClass"));
        FindDialogClass->setWindowModality(Qt::NonModal);
        FindDialogClass->resize(503, 581);
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(FindDialogClass->sizePolicy().hasHeightForWidth());
        FindDialogClass->setSizePolicy(sizePolicy);
        verticalLayout_2 = new QVBoxLayout(FindDialogClass);
        verticalLayout_2->setSpacing(0);
        verticalLayout_2->setMargin(0);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        styledFrame = new QFrame(FindDialogClass);
        styledFrame->setObjectName(QString::fromUtf8("styledFrame"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(styledFrame->sizePolicy().hasHeightForWidth());
        styledFrame->setSizePolicy(sizePolicy1);
        styledFrame->setFrameShape(QFrame::NoFrame);
        styledFrame->setFrameShadow(QFrame::Plain);
        styledFrame->setLineWidth(0);
        frameVLayout = new QVBoxLayout(styledFrame);
        frameVLayout->setSpacing(5);
        frameVLayout->setMargin(1);
        frameVLayout->setObjectName(QString::fromUtf8("frameVLayout"));
        frameVLayout->setSizeConstraint(QLayout::SetDefaultConstraint);
        frameVLayout->setContentsMargins(1, 0, 1, 1);
        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setSpacing(1);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        horizontalLayout_2->setSizeConstraint(QLayout::SetDefaultConstraint);
        horizontalLayout_2->setContentsMargins(-1, 5, 4, -1);
        horizontalSpacer_4 = new QSpacerItem(10, 5, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_4);

        closeToolButton = new QToolButton(styledFrame);
        closeToolButton->setObjectName(QString::fromUtf8("closeToolButton"));
        closeToolButton->setFocusPolicy(Qt::NoFocus);
        closeToolButton->setLayoutDirection(Qt::LeftToRight);
        closeToolButton->setPopupMode(QToolButton::DelayedPopup);

        horizontalLayout_2->addWidget(closeToolButton);


        frameVLayout->addLayout(horizontalLayout_2);

        findTabWidget = new QTabWidget(styledFrame);
        findTabWidget->setObjectName(QString::fromUtf8("findTabWidget"));
        friendsTab = new QWidget();
        friendsTab->setObjectName(QString::fromUtf8("friendsTab"));
        gridLayout = new QGridLayout(friendsTab);
        gridLayout->setSpacing(1);
        gridLayout->setMargin(1);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        findPeopleFrame = new MoodBox::FindPeopleFrame(friendsTab);
        findPeopleFrame->setObjectName(QString::fromUtf8("findPeopleFrame"));
        findPeopleFrame->setFrameShape(QFrame::NoFrame);
        findPeopleFrame->setFrameShadow(QFrame::Plain);
        findPeopleFrame->setLineWidth(0);

        gridLayout->addWidget(findPeopleFrame, 0, 0, 1, 1);

        findTabWidget->addTab(friendsTab, QString());
        channelsTab = new QWidget();
        channelsTab->setObjectName(QString::fromUtf8("channelsTab"));
        verticalLayout = new QVBoxLayout(channelsTab);
        verticalLayout->setSpacing(1);
        verticalLayout->setMargin(1);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        findChannelFrame = new MoodBox::FindChannelFrame(channelsTab);
        findChannelFrame->setObjectName(QString::fromUtf8("findChannelFrame"));
        findChannelFrame->setFrameShape(QFrame::NoFrame);
        findChannelFrame->setFrameShadow(QFrame::Plain);
        findChannelFrame->setLineWidth(0);

        verticalLayout->addWidget(findChannelFrame);

        findTabWidget->addTab(channelsTab, QString());

        frameVLayout->addWidget(findTabWidget);

        frameBottom = new QFrame(styledFrame);
        frameBottom->setObjectName(QString::fromUtf8("frameBottom"));
        sizePolicy1.setHeightForWidth(frameBottom->sizePolicy().hasHeightForWidth());
        frameBottom->setSizePolicy(sizePolicy1);
        frameBottom->setFrameShape(QFrame::NoFrame);
        frameBottom->setFrameShadow(QFrame::Plain);
        frameBottom->setLineWidth(0);
        buttonsHLayout = new QHBoxLayout(frameBottom);
        buttonsHLayout->setSpacing(1);
        buttonsHLayout->setMargin(1);
        buttonsHLayout->setObjectName(QString::fromUtf8("buttonsHLayout"));
        buttonsHLayout->setContentsMargins(-1, 2, 15, 0);
        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        buttonsHLayout->addItem(horizontalSpacer_2);

        addUserButton = new QPushButton(frameBottom);
        addUserButton->setObjectName(QString::fromUtf8("addUserButton"));
        addUserButton->setEnabled(false);

        buttonsHLayout->addWidget(addUserButton);

        closeButton = new QPushButton(frameBottom);
        closeButton->setObjectName(QString::fromUtf8("closeButton"));

        buttonsHLayout->addWidget(closeButton);


        frameVLayout->addWidget(frameBottom);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(1);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalLayout->setContentsMargins(-1, -1, -1, 0);
        horizontalSpacer_3 = new QSpacerItem(40, 10, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer_3);

        widget = new QWidget(styledFrame);
        widget->setObjectName(QString::fromUtf8("widget"));
        QSizePolicy sizePolicy2(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(widget->sizePolicy().hasHeightForWidth());
        widget->setSizePolicy(sizePolicy2);
        widget->setMinimumSize(QSize(16, 16));

        horizontalLayout->addWidget(widget);


        frameVLayout->addLayout(horizontalLayout);


        verticalLayout_2->addWidget(styledFrame);

        QWidget::setTabOrder(addUserButton, closeButton);

        retranslateUi(FindDialogClass);

        findTabWidget->setCurrentIndex(0);


        QMetaObject::connectSlotsByName(FindDialogClass);
    } // setupUi

    void retranslateUi(QDialog *FindDialogClass)
    {
#ifndef QT_NO_ACCESSIBILITY
        closeToolButton->setAccessibleName(QApplication::translate("FindDialogClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        findTabWidget->setTabText(findTabWidget->indexOf(friendsTab), QApplication::translate("FindDialogClass", "People", 0, QApplication::UnicodeUTF8));
        findTabWidget->setTabText(findTabWidget->indexOf(channelsTab), QApplication::translate("FindDialogClass", "Channels", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        addUserButton->setAccessibleName(QApplication::translate("FindDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        addUserButton->setText(QApplication::translate("FindDialogClass", "AddUserButton", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        closeButton->setAccessibleName(QApplication::translate("FindDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        closeButton->setText(QApplication::translate("FindDialogClass", "CloseButton", 0, QApplication::UnicodeUTF8));
        Q_UNUSED(FindDialogClass);
    } // retranslateUi

};

namespace Ui {
    class FindDialogClass: public Ui_FindDialogClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_FINDDIALOG_H
