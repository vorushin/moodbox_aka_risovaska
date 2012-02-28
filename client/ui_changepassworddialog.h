/********************************************************************************
** Form generated from reading ui file 'changepassworddialog.ui'
**
** Created: Tue May 5 10:34:35 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_CHANGEPASSWORDDIALOG_H
#define UI_CHANGEPASSWORDDIALOG_H

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
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_ChangePasswordDialogClass
{
public:
    QVBoxLayout *verticalLayout_2;
    QFrame *styledFrame;
    QVBoxLayout *verticalLayout;
    QHBoxLayout *horizontalLayout_3;
    QSpacerItem *horizontalSpacer_4;
    QToolButton *closeToolButton;
    QLabel *titleLabel;
    QFrame *mainDialogFrame;
    QGridLayout *gridLayout;
    QLabel *currentPasswordLabel;
    QLineEdit *currentPasswordEdit;
    QSpacerItem *verticalSpacer_4;
    QLabel *newPasswordLabel;
    QLineEdit *newPasswordEdit;
    QLabel *repeatNewPasswordLabel;
    QLineEdit *repeatNewPasswordEdit;
    QHBoxLayout *horizontalLayout;
    QSpacerItem *horizontalSpacer;
    QPushButton *okButton;
    QPushButton *cancelButton;

    void setupUi(QDialog *ChangePasswordDialogClass)
    {
        if (ChangePasswordDialogClass->objectName().isEmpty())
            ChangePasswordDialogClass->setObjectName(QString::fromUtf8("ChangePasswordDialogClass"));
        ChangePasswordDialogClass->resize(354, 228);
        ChangePasswordDialogClass->setModal(true);
        verticalLayout_2 = new QVBoxLayout(ChangePasswordDialogClass);
        verticalLayout_2->setSpacing(0);
        verticalLayout_2->setMargin(0);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        styledFrame = new QFrame(ChangePasswordDialogClass);
        styledFrame->setObjectName(QString::fromUtf8("styledFrame"));
        styledFrame->setFrameShape(QFrame::NoFrame);
        styledFrame->setFrameShadow(QFrame::Plain);
        styledFrame->setLineWidth(0);
        verticalLayout = new QVBoxLayout(styledFrame);
        verticalLayout->setSpacing(5);
        verticalLayout->setMargin(0);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        verticalLayout->setContentsMargins(1, 5, 1, 5);
        horizontalLayout_3 = new QHBoxLayout();
        horizontalLayout_3->setSpacing(0);
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        horizontalLayout_3->setContentsMargins(-1, 5, 4, -1);
        horizontalSpacer_4 = new QSpacerItem(10, 5, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_4);

        closeToolButton = new QToolButton(styledFrame);
        closeToolButton->setObjectName(QString::fromUtf8("closeToolButton"));
        closeToolButton->setFocusPolicy(Qt::NoFocus);
        closeToolButton->setLayoutDirection(Qt::LeftToRight);
        closeToolButton->setPopupMode(QToolButton::DelayedPopup);

        horizontalLayout_3->addWidget(closeToolButton);


        verticalLayout->addLayout(horizontalLayout_3);

        titleLabel = new QLabel(styledFrame);
        titleLabel->setObjectName(QString::fromUtf8("titleLabel"));
        titleLabel->setIndent(9);

        verticalLayout->addWidget(titleLabel);

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
        gridLayout = new QGridLayout(mainDialogFrame);
        gridLayout->setSpacing(0);
        gridLayout->setMargin(0);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        gridLayout->setHorizontalSpacing(11);
        gridLayout->setVerticalSpacing(9);
        gridLayout->setContentsMargins(50, 15, 50, 15);
        currentPasswordLabel = new QLabel(mainDialogFrame);
        currentPasswordLabel->setObjectName(QString::fromUtf8("currentPasswordLabel"));

        gridLayout->addWidget(currentPasswordLabel, 0, 0, 1, 1);

        currentPasswordEdit = new QLineEdit(mainDialogFrame);
        currentPasswordEdit->setObjectName(QString::fromUtf8("currentPasswordEdit"));
        currentPasswordEdit->setEchoMode(QLineEdit::Password);

        gridLayout->addWidget(currentPasswordEdit, 0, 1, 1, 1);

        verticalSpacer_4 = new QSpacerItem(150, 10, QSizePolicy::Minimum, QSizePolicy::Expanding);

        gridLayout->addItem(verticalSpacer_4, 1, 1, 1, 1);

        newPasswordLabel = new QLabel(mainDialogFrame);
        newPasswordLabel->setObjectName(QString::fromUtf8("newPasswordLabel"));

        gridLayout->addWidget(newPasswordLabel, 2, 0, 1, 1);

        newPasswordEdit = new QLineEdit(mainDialogFrame);
        newPasswordEdit->setObjectName(QString::fromUtf8("newPasswordEdit"));
        newPasswordEdit->setEchoMode(QLineEdit::Password);

        gridLayout->addWidget(newPasswordEdit, 2, 1, 1, 1);

        repeatNewPasswordLabel = new QLabel(mainDialogFrame);
        repeatNewPasswordLabel->setObjectName(QString::fromUtf8("repeatNewPasswordLabel"));

        gridLayout->addWidget(repeatNewPasswordLabel, 3, 0, 1, 1);

        repeatNewPasswordEdit = new QLineEdit(mainDialogFrame);
        repeatNewPasswordEdit->setObjectName(QString::fromUtf8("repeatNewPasswordEdit"));
        repeatNewPasswordEdit->setEchoMode(QLineEdit::Password);

        gridLayout->addWidget(repeatNewPasswordEdit, 3, 1, 1, 1);


        verticalLayout->addWidget(mainDialogFrame);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(0);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalLayout->setContentsMargins(-1, 9, 9, 9);
        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);

        okButton = new QPushButton(styledFrame);
        okButton->setObjectName(QString::fromUtf8("okButton"));
        QSizePolicy sizePolicy1(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(okButton->sizePolicy().hasHeightForWidth());
        okButton->setSizePolicy(sizePolicy1);

        horizontalLayout->addWidget(okButton);

        cancelButton = new QPushButton(styledFrame);
        cancelButton->setObjectName(QString::fromUtf8("cancelButton"));
        sizePolicy1.setHeightForWidth(cancelButton->sizePolicy().hasHeightForWidth());
        cancelButton->setSizePolicy(sizePolicy1);

        horizontalLayout->addWidget(cancelButton);


        verticalLayout->addLayout(horizontalLayout);


        verticalLayout_2->addWidget(styledFrame);

#ifndef QT_NO_SHORTCUT
        currentPasswordLabel->setBuddy(currentPasswordEdit);
        newPasswordLabel->setBuddy(newPasswordEdit);
        repeatNewPasswordLabel->setBuddy(repeatNewPasswordEdit);
#endif // QT_NO_SHORTCUT

        retranslateUi(ChangePasswordDialogClass);

        QMetaObject::connectSlotsByName(ChangePasswordDialogClass);
    } // setupUi

    void retranslateUi(QDialog *ChangePasswordDialogClass)
    {
#ifndef QT_NO_TOOLTIP
        closeToolButton->setToolTip(QApplication::translate("ChangePasswordDialogClass", "CloseHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        closeToolButton->setAccessibleName(QApplication::translate("ChangePasswordDialogClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        titleLabel->setText(QApplication::translate("ChangePasswordDialogClass", "Title", 0, QApplication::UnicodeUTF8));
        currentPasswordLabel->setText(QApplication::translate("ChangePasswordDialogClass", "CurrentPassword", 0, QApplication::UnicodeUTF8));
        newPasswordLabel->setText(QApplication::translate("ChangePasswordDialogClass", "NewPassword", 0, QApplication::UnicodeUTF8));
        repeatNewPasswordLabel->setText(QApplication::translate("ChangePasswordDialogClass", "NewPassword2", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        okButton->setAccessibleName(QApplication::translate("ChangePasswordDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        okButton->setText(QApplication::translate("ChangePasswordDialogClass", "OkButton", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        cancelButton->setAccessibleName(QApplication::translate("ChangePasswordDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        cancelButton->setText(QApplication::translate("ChangePasswordDialogClass", "CancelButton", 0, QApplication::UnicodeUTF8));
        Q_UNUSED(ChangePasswordDialogClass);
    } // retranslateUi

};

namespace Ui {
    class ChangePasswordDialogClass: public Ui_ChangePasswordDialogClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CHANGEPASSWORDDIALOG_H
