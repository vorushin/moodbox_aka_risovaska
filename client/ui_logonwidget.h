/********************************************************************************
** Form generated from reading ui file 'logonwidget.ui'
**
** Created: Tue May 5 10:34:45 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_LOGONWIDGET_H
#define UI_LOGONWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QComboBox>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_LogonWidgetClass
{
public:
    QFrame *frame_;
    QVBoxLayout *verticalLayout;
    QFrame *topTitleFrame;
    QHBoxLayout *horizontalLayout;
    QPushButton *settingsButton;
    QPushButton *forgotPasswordButton;
    QPushButton *signUpButton;
    QSpacerItem *verticalSpacer_4;
    QFrame *frame;
    QVBoxLayout *verticalLayout_3;
    QGridLayout *gridLayout;
    QLabel *loginLabel;
    QComboBox *loginCombo;
    QLabel *passwordLabel;
    QVBoxLayout *verticalLayout_4;
    QCheckBox *savePasswordCheck;
    QCheckBox *autoLogonCheckBox;
    QVBoxLayout *verticalLayout_2;
    QLineEdit *passwordEdit;
    QSpacerItem *verticalSpacer_5;
    QFrame *bottomLinksFrame;
    QHBoxLayout *horizontalLayout_2;
    QSpacerItem *horizontalSpacer_2;
    QPushButton *signInButton;
    QSpacerItem *horizontalSpacer;
    QFrame *transBorder_4;
    QFrame *transBorder;
    QFrame *transBorder_2;
    QFrame *transBorder_3;

    void setupUi(QWidget *LogonWidgetClass)
    {
        if (LogonWidgetClass->objectName().isEmpty())
            LogonWidgetClass->setObjectName(QString::fromUtf8("LogonWidgetClass"));
        LogonWidgetClass->resize(388, 267);
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(LogonWidgetClass->sizePolicy().hasHeightForWidth());
        LogonWidgetClass->setSizePolicy(sizePolicy);
        frame_ = new QFrame(LogonWidgetClass);
        frame_->setObjectName(QString::fromUtf8("frame_"));
        frame_->setGeometry(QRect(9, 9, 370, 249));
        frame_->setFrameShape(QFrame::NoFrame);
        frame_->setFrameShadow(QFrame::Plain);
        frame_->setLineWidth(0);
        verticalLayout = new QVBoxLayout(frame_);
        verticalLayout->setSpacing(0);
        verticalLayout->setMargin(0);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        topTitleFrame = new QFrame(frame_);
        topTitleFrame->setObjectName(QString::fromUtf8("topTitleFrame"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(topTitleFrame->sizePolicy().hasHeightForWidth());
        topTitleFrame->setSizePolicy(sizePolicy1);
        topTitleFrame->setMaximumSize(QSize(16777215, 35));
        topTitleFrame->setFrameShape(QFrame::NoFrame);
        topTitleFrame->setFrameShadow(QFrame::Plain);
        topTitleFrame->setLineWidth(0);
        horizontalLayout = new QHBoxLayout(topTitleFrame);
        horizontalLayout->setSpacing(0);
        horizontalLayout->setMargin(0);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalLayout->setContentsMargins(0, -1, 0, 0);
        settingsButton = new QPushButton(topTitleFrame);
        settingsButton->setObjectName(QString::fromUtf8("settingsButton"));
        QFont font;
        font.setFamily(QString::fromUtf8("MS Shell Dlg 2"));
        font.setPointSize(8);
        font.setBold(false);
        font.setItalic(false);
        font.setUnderline(true);
        font.setWeight(50);
        font.setStrikeOut(false);
        settingsButton->setFont(font);
        settingsButton->setCursor(QCursor(Qt::PointingHandCursor));
        settingsButton->setFlat(true);

        horizontalLayout->addWidget(settingsButton);

        forgotPasswordButton = new QPushButton(topTitleFrame);
        forgotPasswordButton->setObjectName(QString::fromUtf8("forgotPasswordButton"));
        forgotPasswordButton->setFont(font);
        forgotPasswordButton->setCursor(QCursor(Qt::PointingHandCursor));
        forgotPasswordButton->setFlat(true);

        horizontalLayout->addWidget(forgotPasswordButton);

        signUpButton = new QPushButton(topTitleFrame);
        signUpButton->setObjectName(QString::fromUtf8("signUpButton"));
        signUpButton->setFont(font);
        signUpButton->setCursor(QCursor(Qt::PointingHandCursor));
        signUpButton->setFlat(true);

        horizontalLayout->addWidget(signUpButton);


        verticalLayout->addWidget(topTitleFrame);

        verticalSpacer_4 = new QSpacerItem(20, 35, QSizePolicy::Minimum, QSizePolicy::Fixed);

        verticalLayout->addItem(verticalSpacer_4);

        frame = new QFrame(frame_);
        frame->setObjectName(QString::fromUtf8("frame"));
        frame->setFrameShape(QFrame::NoFrame);
        frame->setFrameShadow(QFrame::Plain);
        frame->setLineWidth(0);
        verticalLayout_3 = new QVBoxLayout(frame);
        verticalLayout_3->setSpacing(0);
        verticalLayout_3->setMargin(0);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        verticalLayout_3->setContentsMargins(20, 0, 30, 0);
        gridLayout = new QGridLayout();
        gridLayout->setSpacing(0);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        gridLayout->setHorizontalSpacing(10);
        gridLayout->setVerticalSpacing(2);
        gridLayout->setContentsMargins(-1, 2, -1, -1);
        loginLabel = new QLabel(frame);
        loginLabel->setObjectName(QString::fromUtf8("loginLabel"));
        QFont font1;
        font1.setFamily(QString::fromUtf8("MS Shell Dlg 2"));
        font1.setPointSize(10);
        font1.setBold(false);
        font1.setItalic(false);
        font1.setUnderline(false);
        font1.setWeight(50);
        font1.setStrikeOut(false);
        loginLabel->setFont(font1);
        loginLabel->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        gridLayout->addWidget(loginLabel, 0, 0, 1, 1);

        loginCombo = new QComboBox(frame);
        loginCombo->setObjectName(QString::fromUtf8("loginCombo"));
        loginCombo->setEditable(true);

        gridLayout->addWidget(loginCombo, 0, 1, 1, 1);

        passwordLabel = new QLabel(frame);
        passwordLabel->setObjectName(QString::fromUtf8("passwordLabel"));
        passwordLabel->setFont(font1);
        passwordLabel->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        gridLayout->addWidget(passwordLabel, 1, 0, 1, 1);

        verticalLayout_4 = new QVBoxLayout();
        verticalLayout_4->setSpacing(5);
        verticalLayout_4->setObjectName(QString::fromUtf8("verticalLayout_4"));
        verticalLayout_4->setContentsMargins(-1, 10, -1, -1);
        savePasswordCheck = new QCheckBox(frame);
        savePasswordCheck->setObjectName(QString::fromUtf8("savePasswordCheck"));

        verticalLayout_4->addWidget(savePasswordCheck);

        autoLogonCheckBox = new QCheckBox(frame);
        autoLogonCheckBox->setObjectName(QString::fromUtf8("autoLogonCheckBox"));
        autoLogonCheckBox->setEnabled(false);
        QSizePolicy sizePolicy2(QSizePolicy::Expanding, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(autoLogonCheckBox->sizePolicy().hasHeightForWidth());
        autoLogonCheckBox->setSizePolicy(sizePolicy2);

        verticalLayout_4->addWidget(autoLogonCheckBox);


        gridLayout->addLayout(verticalLayout_4, 3, 1, 1, 1);

        verticalLayout_2 = new QVBoxLayout();
        verticalLayout_2->setSpacing(0);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        verticalLayout_2->setContentsMargins(-1, 8, -1, -1);
        passwordEdit = new QLineEdit(frame);
        passwordEdit->setObjectName(QString::fromUtf8("passwordEdit"));
        passwordEdit->setEchoMode(QLineEdit::Password);

        verticalLayout_2->addWidget(passwordEdit);


        gridLayout->addLayout(verticalLayout_2, 1, 1, 1, 1);


        verticalLayout_3->addLayout(gridLayout);


        verticalLayout->addWidget(frame);

        verticalSpacer_5 = new QSpacerItem(20, 15, QSizePolicy::Minimum, QSizePolicy::Fixed);

        verticalLayout->addItem(verticalSpacer_5);

        bottomLinksFrame = new QFrame(frame_);
        bottomLinksFrame->setObjectName(QString::fromUtf8("bottomLinksFrame"));
        sizePolicy1.setHeightForWidth(bottomLinksFrame->sizePolicy().hasHeightForWidth());
        bottomLinksFrame->setSizePolicy(sizePolicy1);
        bottomLinksFrame->setMaximumSize(QSize(16777215, 35));
        bottomLinksFrame->setFrameShape(QFrame::NoFrame);
        bottomLinksFrame->setFrameShadow(QFrame::Plain);
        bottomLinksFrame->setLineWidth(0);
        horizontalLayout_2 = new QHBoxLayout(bottomLinksFrame);
        horizontalLayout_2->setSpacing(30);
        horizontalLayout_2->setMargin(0);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        horizontalLayout_2->setContentsMargins(-1, 0, -1, -1);
        horizontalSpacer_2 = new QSpacerItem(10, 14, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_2);

        signInButton = new QPushButton(bottomLinksFrame);
        signInButton->setObjectName(QString::fromUtf8("signInButton"));
        signInButton->setFont(font);
        signInButton->setCursor(QCursor(Qt::PointingHandCursor));
        signInButton->setChecked(false);
        signInButton->setDefault(true);
        signInButton->setFlat(true);

        horizontalLayout_2->addWidget(signInButton);

        horizontalSpacer = new QSpacerItem(10, 14, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer);


        verticalLayout->addWidget(bottomLinksFrame);

        transBorder_4 = new QFrame(LogonWidgetClass);
        transBorder_4->setObjectName(QString::fromUtf8("transBorder_4"));
        transBorder_4->setGeometry(QRect(0, 0, 401, 2));
        transBorder_4->setMinimumSize(QSize(0, 2));
        transBorder_4->setMaximumSize(QSize(16777215, 2));
        transBorder_4->setFrameShape(QFrame::Box);
        transBorder_4->setFrameShadow(QFrame::Plain);
        transBorder = new QFrame(LogonWidgetClass);
        transBorder->setObjectName(QString::fromUtf8("transBorder"));
        transBorder->setGeometry(QRect(386, 2, 2, 263));
        transBorder->setMinimumSize(QSize(2, 263));
        transBorder->setMaximumSize(QSize(2, 263));
        transBorder->setFrameShape(QFrame::Box);
        transBorder->setFrameShadow(QFrame::Plain);
        transBorder->setLineWidth(1);
        transBorder_2 = new QFrame(LogonWidgetClass);
        transBorder_2->setObjectName(QString::fromUtf8("transBorder_2"));
        transBorder_2->setGeometry(QRect(0, 2, 2, 263));
        transBorder_2->setMinimumSize(QSize(2, 263));
        transBorder_2->setMaximumSize(QSize(2, 263));
        transBorder_2->setFrameShape(QFrame::Box);
        transBorder_2->setFrameShadow(QFrame::Plain);
        transBorder_3 = new QFrame(LogonWidgetClass);
        transBorder_3->setObjectName(QString::fromUtf8("transBorder_3"));
        transBorder_3->setGeometry(QRect(0, 265, 401, 2));
        transBorder_3->setMinimumSize(QSize(0, 2));
        transBorder_3->setMaximumSize(QSize(16777215, 2));
        transBorder_3->setFrameShape(QFrame::Box);
        transBorder_3->setFrameShadow(QFrame::Plain);
#ifndef QT_NO_SHORTCUT
        loginLabel->setBuddy(loginCombo);
        passwordLabel->setBuddy(passwordEdit);
#endif // QT_NO_SHORTCUT
        QWidget::setTabOrder(loginCombo, passwordEdit);
        QWidget::setTabOrder(passwordEdit, savePasswordCheck);
        QWidget::setTabOrder(savePasswordCheck, autoLogonCheckBox);

        retranslateUi(LogonWidgetClass);

        QMetaObject::connectSlotsByName(LogonWidgetClass);
    } // setupUi

    void retranslateUi(QWidget *LogonWidgetClass)
    {
#ifndef QT_NO_ACCESSIBILITY
        settingsButton->setAccessibleName(QApplication::translate("LogonWidgetClass", "blueButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        settingsButton->setText(QApplication::translate("LogonWidgetClass", "Settings", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        forgotPasswordButton->setAccessibleName(QApplication::translate("LogonWidgetClass", "blueButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        forgotPasswordButton->setText(QApplication::translate("LogonWidgetClass", "RestorePassword", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        signUpButton->setAccessibleName(QApplication::translate("LogonWidgetClass", "blueButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        signUpButton->setText(QApplication::translate("LogonWidgetClass", "SignUp!", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        loginLabel->setAccessibleName(QApplication::translate("LogonWidgetClass", "whiteLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        loginLabel->setText(QApplication::translate("LogonWidgetClass", "Login", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        passwordLabel->setAccessibleName(QApplication::translate("LogonWidgetClass", "whiteLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        passwordLabel->setText(QApplication::translate("LogonWidgetClass", "Password", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        savePasswordCheck->setAccessibleName(QApplication::translate("LogonWidgetClass", "whiteLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        savePasswordCheck->setText(QApplication::translate("LogonWidgetClass", "SavePasswordCheckBox", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        autoLogonCheckBox->setAccessibleName(QApplication::translate("LogonWidgetClass", "whiteLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        autoLogonCheckBox->setText(QApplication::translate("LogonWidgetClass", "AutoLoginCheckBox", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        signInButton->setAccessibleName(QApplication::translate("LogonWidgetClass", "bigBlueButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        signInButton->setText(QApplication::translate("LogonWidgetClass", "SignIn", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        transBorder_4->setAccessibleName(QApplication::translate("LogonWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder->setAccessibleName(QApplication::translate("LogonWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder_2->setAccessibleName(QApplication::translate("LogonWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder_3->setAccessibleName(QApplication::translate("LogonWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        Q_UNUSED(LogonWidgetClass);
    } // retranslateUi

};

namespace Ui {
    class LogonWidgetClass: public Ui_LogonWidgetClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_LOGONWIDGET_H
