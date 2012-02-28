/********************************************************************************
** Form generated from reading ui file 'authorizationdialog.ui'
**
** Created: Tue May 5 10:34:33 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_AUTHORIZATIONDIALOG_H
#define UI_AUTHORIZATIONDIALOG_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialog>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QPlainTextEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include "contactavatarbutton.h"

QT_BEGIN_NAMESPACE

class Ui_AuthorizationDialogClass
{
public:
    QVBoxLayout *verticalLayout_2;
    QFrame *styledFrame;
    QVBoxLayout *frameVLayout;
    QHBoxLayout *horizontalLayout_3;
    QSpacerItem *horizontalSpacer_4;
    QToolButton *closeToolButton;
    QFrame *mainDialogFrame;
    QVBoxLayout *verticalLayout;
    QGridLayout *gridLayout;
    QLabel *topLabel;
    QSpacerItem *verticalSpacer_2;
    QLabel *authUserNameLabel;
    QHBoxLayout *horizontalLayout_2;
    MoodBox::ContactAvatarButton *contactAvatarToolButton;
    QPlainTextEdit *messageEdit;
    QSpacerItem *verticalSpacer;
    QHBoxLayout *horizontalLayout;
    QSpacerItem *horizontalSpacer;
    QPushButton *okButton;
    QPushButton *rejectButton;
    QPushButton *cancelButton;

    void setupUi(QDialog *AuthorizationDialogClass)
    {
        if (AuthorizationDialogClass->objectName().isEmpty())
            AuthorizationDialogClass->setObjectName(QString::fromUtf8("AuthorizationDialogClass"));
        AuthorizationDialogClass->setWindowModality(Qt::WindowModal);
        AuthorizationDialogClass->resize(370, 294);
        verticalLayout_2 = new QVBoxLayout(AuthorizationDialogClass);
        verticalLayout_2->setSpacing(6);
        verticalLayout_2->setMargin(0);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        styledFrame = new QFrame(AuthorizationDialogClass);
        styledFrame->setObjectName(QString::fromUtf8("styledFrame"));
        styledFrame->setFrameShape(QFrame::NoFrame);
        styledFrame->setFrameShadow(QFrame::Plain);
        styledFrame->setLineWidth(0);
        frameVLayout = new QVBoxLayout(styledFrame);
        frameVLayout->setSpacing(5);
        frameVLayout->setMargin(11);
        frameVLayout->setObjectName(QString::fromUtf8("frameVLayout"));
        frameVLayout->setContentsMargins(1, 0, 1, 0);
        horizontalLayout_3 = new QHBoxLayout();
        horizontalLayout_3->setSpacing(6);
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        horizontalLayout_3->setContentsMargins(-1, 9, 4, 9);
        horizontalSpacer_4 = new QSpacerItem(10, 5, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_4);

        closeToolButton = new QToolButton(styledFrame);
        closeToolButton->setObjectName(QString::fromUtf8("closeToolButton"));
        closeToolButton->setFocusPolicy(Qt::NoFocus);
        closeToolButton->setLayoutDirection(Qt::LeftToRight);
        closeToolButton->setPopupMode(QToolButton::DelayedPopup);

        horizontalLayout_3->addWidget(closeToolButton);


        frameVLayout->addLayout(horizontalLayout_3);

        mainDialogFrame = new QFrame(styledFrame);
        mainDialogFrame->setObjectName(QString::fromUtf8("mainDialogFrame"));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Expanding);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mainDialogFrame->sizePolicy().hasHeightForWidth());
        mainDialogFrame->setSizePolicy(sizePolicy);
        mainDialogFrame->setFrameShape(QFrame::NoFrame);
        mainDialogFrame->setFrameShadow(QFrame::Plain);
        mainDialogFrame->setLineWidth(0);
        verticalLayout = new QVBoxLayout(mainDialogFrame);
        verticalLayout->setSpacing(6);
        verticalLayout->setMargin(11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setMargin(9);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        gridLayout->setHorizontalSpacing(12);
        gridLayout->setVerticalSpacing(0);
        topLabel = new QLabel(mainDialogFrame);
        topLabel->setObjectName(QString::fromUtf8("topLabel"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(topLabel->sizePolicy().hasHeightForWidth());
        topLabel->setSizePolicy(sizePolicy1);
        QFont font;
        font.setFamily(QString::fromUtf8("MS Shell Dlg 2"));
        font.setPointSize(9);
        font.setBold(false);
        font.setItalic(false);
        font.setUnderline(false);
        font.setWeight(50);
        font.setStrikeOut(false);
        topLabel->setFont(font);
        topLabel->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignVCenter);
        topLabel->setWordWrap(true);

        gridLayout->addWidget(topLabel, 0, 0, 1, 2);

        verticalSpacer_2 = new QSpacerItem(20, 20, QSizePolicy::Minimum, QSizePolicy::Fixed);

        gridLayout->addItem(verticalSpacer_2, 1, 1, 1, 1);

        authUserNameLabel = new QLabel(mainDialogFrame);
        authUserNameLabel->setObjectName(QString::fromUtf8("authUserNameLabel"));
        sizePolicy1.setHeightForWidth(authUserNameLabel->sizePolicy().hasHeightForWidth());
        authUserNameLabel->setSizePolicy(sizePolicy1);
        QFont font1;
        font1.setFamily(QString::fromUtf8("MS Shell Dlg 2"));
        font1.setPointSize(11);
        font1.setBold(true);
        font1.setItalic(false);
        font1.setUnderline(false);
        font1.setWeight(75);
        font1.setStrikeOut(false);
        authUserNameLabel->setFont(font1);
        authUserNameLabel->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignTop);

        gridLayout->addWidget(authUserNameLabel, 2, 1, 1, 1);

        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setSpacing(6);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        contactAvatarToolButton = new MoodBox::ContactAvatarButton(mainDialogFrame);
        contactAvatarToolButton->setObjectName(QString::fromUtf8("contactAvatarToolButton"));
        contactAvatarToolButton->setFocusPolicy(Qt::NoFocus);
        contactAvatarToolButton->setIconSize(QSize(74, 74));

        horizontalLayout_2->addWidget(contactAvatarToolButton);


        gridLayout->addLayout(horizontalLayout_2, 3, 0, 1, 1);

        messageEdit = new QPlainTextEdit(mainDialogFrame);
        messageEdit->setObjectName(QString::fromUtf8("messageEdit"));
        QSizePolicy sizePolicy2(QSizePolicy::Expanding, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(messageEdit->sizePolicy().hasHeightForWidth());
        messageEdit->setSizePolicy(sizePolicy2);
        messageEdit->setMaximumSize(QSize(16777215, 70));
        messageEdit->setFrameShape(QFrame::NoFrame);
        messageEdit->setFrameShadow(QFrame::Plain);
        messageEdit->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
        messageEdit->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);

        gridLayout->addWidget(messageEdit, 3, 1, 1, 1);


        verticalLayout->addLayout(gridLayout);

        verticalSpacer = new QSpacerItem(20, 0, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout->addItem(verticalSpacer);


        frameVLayout->addWidget(mainDialogFrame);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalLayout->setContentsMargins(-1, 9, 9, 9);
        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);

        okButton = new QPushButton(styledFrame);
        okButton->setObjectName(QString::fromUtf8("okButton"));

        horizontalLayout->addWidget(okButton);

        rejectButton = new QPushButton(styledFrame);
        rejectButton->setObjectName(QString::fromUtf8("rejectButton"));

        horizontalLayout->addWidget(rejectButton);

        cancelButton = new QPushButton(styledFrame);
        cancelButton->setObjectName(QString::fromUtf8("cancelButton"));

        horizontalLayout->addWidget(cancelButton);


        frameVLayout->addLayout(horizontalLayout);


        verticalLayout_2->addWidget(styledFrame);

        QWidget::setTabOrder(messageEdit, okButton);
        QWidget::setTabOrder(okButton, rejectButton);
        QWidget::setTabOrder(rejectButton, cancelButton);

        retranslateUi(AuthorizationDialogClass);

        QMetaObject::connectSlotsByName(AuthorizationDialogClass);
    } // setupUi

    void retranslateUi(QDialog *AuthorizationDialogClass)
    {
#ifndef QT_NO_TOOLTIP
        closeToolButton->setToolTip(QApplication::translate("AuthorizationDialogClass", "CloseButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        closeToolButton->setAccessibleName(QApplication::translate("AuthorizationDialogClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        topLabel->setText(QApplication::translate("AuthorizationDialogClass", "TopText_DT", 0, QApplication::UnicodeUTF8));
        authUserNameLabel->setText(QApplication::translate("AuthorizationDialogClass", "UserName_DT", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        contactAvatarToolButton->setAccessibleName(QApplication::translate("AuthorizationDialogClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        okButton->setAccessibleName(QApplication::translate("AuthorizationDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        okButton->setText(QApplication::translate("AuthorizationDialogClass", "Send_DT", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        rejectButton->setAccessibleName(QApplication::translate("AuthorizationDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        rejectButton->setText(QApplication::translate("AuthorizationDialogClass", "Reject_DT", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        cancelButton->setAccessibleName(QApplication::translate("AuthorizationDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        cancelButton->setText(QApplication::translate("AuthorizationDialogClass", "Cancel_DT", 0, QApplication::UnicodeUTF8));
        Q_UNUSED(AuthorizationDialogClass);
    } // retranslateUi

};

namespace Ui {
    class AuthorizationDialogClass: public Ui_AuthorizationDialogClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_AUTHORIZATIONDIALOG_H
