/********************************************************************************
** Form generated from reading ui file 'registrationwidget.ui'
**
** Created: Tue May 5 10:34:51 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_REGISTRATIONWIDGET_H
#define UI_REGISTRATIONWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
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

class Ui_RegistrationWidgetClass
{
public:
    QFrame *frame_2;
    QVBoxLayout *verticalLayout;
    QFrame *topTitleFrame;
    QHBoxLayout *horizontalLayout_4;
    QLabel *titleLabel;
    QHBoxLayout *horizontalLayout_2;
    QSpacerItem *horizontalSpacer;
    QLabel *label_3;
    QLabel *label_2;
    QLabel *labelMandatoryFields;
    QSpacerItem *verticalSpacer_4;
    QFrame *frame;
    QVBoxLayout *verticalLayout_4;
    QGridLayout *gridLayout;
    QLineEdit *nameEdit;
    QLineEdit *password2Edit;
    QVBoxLayout *verticalLayout_3;
    QCheckBox *receiveNewsCheckBox;
    QCheckBox *iAcceptCheckBox;
    QVBoxLayout *verticalLayout_2;
    QHBoxLayout *horizontalLayout_6;
    QPushButton *termOfUseButton;
    QLabel *andLabel;
    QPushButton *privacyPolicyButton;
    QSpacerItem *horizontalSpacer_4;
    QLineEdit *passwordEdit;
    QLineEdit *loginEdit;
    QLabel *nameLabel;
    QLabel *loginLabel;
    QLabel *labelRequest_8;
    QLabel *passwordLabel;
    QLabel *labelRequest_9;
    QLabel *password2Label;
    QLabel *labelRequest_7;
    QLabel *emailLabel;
    QLineEdit *emailEdit;
    QLabel *labelRequest_11;
    QSpacerItem *verticalSpacer_5;
    QFrame *bottomLinksFrame;
    QHBoxLayout *horizontalLayout_3;
    QSpacerItem *horizontalSpacer_2;
    QPushButton *backButton;
    QPushButton *nextButton;
    QSpacerItem *horizontalSpacer_3;
    QFrame *transBorder_4;
    QFrame *transBorder;
    QFrame *transBorder_2;
    QFrame *transBorder_3;

    void setupUi(QWidget *RegistrationWidgetClass)
    {
        if (RegistrationWidgetClass->objectName().isEmpty())
            RegistrationWidgetClass->setObjectName(QString::fromUtf8("RegistrationWidgetClass"));
        RegistrationWidgetClass->resize(388, 267);
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(RegistrationWidgetClass->sizePolicy().hasHeightForWidth());
        RegistrationWidgetClass->setSizePolicy(sizePolicy);
        frame_2 = new QFrame(RegistrationWidgetClass);
        frame_2->setObjectName(QString::fromUtf8("frame_2"));
        frame_2->setGeometry(QRect(9, 9, 370, 249));
        frame_2->setFrameShape(QFrame::NoFrame);
        frame_2->setFrameShadow(QFrame::Plain);
        frame_2->setLineWidth(0);
        verticalLayout = new QVBoxLayout(frame_2);
        verticalLayout->setSpacing(3);
        verticalLayout->setMargin(0);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        topTitleFrame = new QFrame(frame_2);
        topTitleFrame->setObjectName(QString::fromUtf8("topTitleFrame"));
        sizePolicy.setHeightForWidth(topTitleFrame->sizePolicy().hasHeightForWidth());
        topTitleFrame->setSizePolicy(sizePolicy);
        topTitleFrame->setFrameShape(QFrame::NoFrame);
        topTitleFrame->setFrameShadow(QFrame::Plain);
        topTitleFrame->setLineWidth(0);
        horizontalLayout_4 = new QHBoxLayout(topTitleFrame);
        horizontalLayout_4->setSpacing(0);
        horizontalLayout_4->setMargin(0);
        horizontalLayout_4->setObjectName(QString::fromUtf8("horizontalLayout_4"));
        horizontalLayout_4->setContentsMargins(-1, -1, -1, 0);
        titleLabel = new QLabel(topTitleFrame);
        titleLabel->setObjectName(QString::fromUtf8("titleLabel"));
        titleLabel->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignVCenter);

        horizontalLayout_4->addWidget(titleLabel);

        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setSpacing(0);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        horizontalSpacer = new QSpacerItem(40, 10, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer);

        label_3 = new QLabel(topTitleFrame);
        label_3->setObjectName(QString::fromUtf8("label_3"));
        label_3->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        horizontalLayout_2->addWidget(label_3);

        label_2 = new QLabel(topTitleFrame);
        label_2->setObjectName(QString::fromUtf8("label_2"));

        horizontalLayout_2->addWidget(label_2);

        labelMandatoryFields = new QLabel(topTitleFrame);
        labelMandatoryFields->setObjectName(QString::fromUtf8("labelMandatoryFields"));
        labelMandatoryFields->setTextFormat(Qt::RichText);
        labelMandatoryFields->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        horizontalLayout_2->addWidget(labelMandatoryFields);


        horizontalLayout_4->addLayout(horizontalLayout_2);


        verticalLayout->addWidget(topTitleFrame);

        verticalSpacer_4 = new QSpacerItem(20, 0, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout->addItem(verticalSpacer_4);

        frame = new QFrame(frame_2);
        frame->setObjectName(QString::fromUtf8("frame"));
        frame->setFrameShape(QFrame::NoFrame);
        frame->setFrameShadow(QFrame::Plain);
        frame->setLineWidth(0);
        verticalLayout_4 = new QVBoxLayout(frame);
        verticalLayout_4->setSpacing(5);
        verticalLayout_4->setMargin(0);
        verticalLayout_4->setObjectName(QString::fromUtf8("verticalLayout_4"));
        gridLayout = new QGridLayout();
        gridLayout->setSpacing(0);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        gridLayout->setHorizontalSpacing(0);
        gridLayout->setVerticalSpacing(5);
        gridLayout->setContentsMargins(0, 9, 8, 0);
        nameEdit = new QLineEdit(frame);
        nameEdit->setObjectName(QString::fromUtf8("nameEdit"));
        nameEdit->setMaxLength(256);

        gridLayout->addWidget(nameEdit, 0, 2, 1, 1);

        password2Edit = new QLineEdit(frame);
        password2Edit->setObjectName(QString::fromUtf8("password2Edit"));
        password2Edit->setMaxLength(32);
        password2Edit->setEchoMode(QLineEdit::Password);

        gridLayout->addWidget(password2Edit, 3, 2, 1, 1);

        verticalLayout_3 = new QVBoxLayout();
        verticalLayout_3->setSpacing(2);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        receiveNewsCheckBox = new QCheckBox(frame);
        receiveNewsCheckBox->setObjectName(QString::fromUtf8("receiveNewsCheckBox"));
        receiveNewsCheckBox->setChecked(true);

        verticalLayout_3->addWidget(receiveNewsCheckBox);

        iAcceptCheckBox = new QCheckBox(frame);
        iAcceptCheckBox->setObjectName(QString::fromUtf8("iAcceptCheckBox"));
        QSizePolicy sizePolicy1(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(iAcceptCheckBox->sizePolicy().hasHeightForWidth());
        iAcceptCheckBox->setSizePolicy(sizePolicy1);

        verticalLayout_3->addWidget(iAcceptCheckBox);

        verticalLayout_2 = new QVBoxLayout();
        verticalLayout_2->setSpacing(0);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        verticalLayout_2->setContentsMargins(-1, -1, -1, 0);
        horizontalLayout_6 = new QHBoxLayout();
        horizontalLayout_6->setSpacing(4);
        horizontalLayout_6->setObjectName(QString::fromUtf8("horizontalLayout_6"));
        horizontalLayout_6->setContentsMargins(0, -1, 0, -1);
        termOfUseButton = new QPushButton(frame);
        termOfUseButton->setObjectName(QString::fromUtf8("termOfUseButton"));
        sizePolicy.setHeightForWidth(termOfUseButton->sizePolicy().hasHeightForWidth());
        termOfUseButton->setSizePolicy(sizePolicy);
        QFont font;
        font.setFamily(QString::fromUtf8("MS Shell Dlg 2"));
        font.setPointSize(8);
        font.setBold(false);
        font.setItalic(false);
        font.setUnderline(true);
        font.setWeight(50);
        font.setStrikeOut(false);
        termOfUseButton->setFont(font);
        termOfUseButton->setCursor(QCursor(Qt::PointingHandCursor));
        termOfUseButton->setFlat(true);

        horizontalLayout_6->addWidget(termOfUseButton);

        andLabel = new QLabel(frame);
        andLabel->setObjectName(QString::fromUtf8("andLabel"));
        QSizePolicy sizePolicy2(QSizePolicy::Fixed, QSizePolicy::Preferred);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(andLabel->sizePolicy().hasHeightForWidth());
        andLabel->setSizePolicy(sizePolicy2);

        horizontalLayout_6->addWidget(andLabel);

        privacyPolicyButton = new QPushButton(frame);
        privacyPolicyButton->setObjectName(QString::fromUtf8("privacyPolicyButton"));
        QSizePolicy sizePolicy3(QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);
        sizePolicy3.setHorizontalStretch(0);
        sizePolicy3.setVerticalStretch(0);
        sizePolicy3.setHeightForWidth(privacyPolicyButton->sizePolicy().hasHeightForWidth());
        privacyPolicyButton->setSizePolicy(sizePolicy3);
        privacyPolicyButton->setFont(font);
        privacyPolicyButton->setCursor(QCursor(Qt::PointingHandCursor));
        privacyPolicyButton->setFlat(true);

        horizontalLayout_6->addWidget(privacyPolicyButton);

        horizontalSpacer_4 = new QSpacerItem(20, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_6->addItem(horizontalSpacer_4);


        verticalLayout_2->addLayout(horizontalLayout_6);


        verticalLayout_3->addLayout(verticalLayout_2);


        gridLayout->addLayout(verticalLayout_3, 5, 2, 1, 1);

        passwordEdit = new QLineEdit(frame);
        passwordEdit->setObjectName(QString::fromUtf8("passwordEdit"));
        passwordEdit->setMaxLength(32);
        passwordEdit->setEchoMode(QLineEdit::Password);

        gridLayout->addWidget(passwordEdit, 2, 2, 1, 1);

        loginEdit = new QLineEdit(frame);
        loginEdit->setObjectName(QString::fromUtf8("loginEdit"));
        loginEdit->setMaxLength(32);

        gridLayout->addWidget(loginEdit, 1, 2, 1, 1);

        nameLabel = new QLabel(frame);
        nameLabel->setObjectName(QString::fromUtf8("nameLabel"));
        nameLabel->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        gridLayout->addWidget(nameLabel, 0, 0, 1, 1);

        loginLabel = new QLabel(frame);
        loginLabel->setObjectName(QString::fromUtf8("loginLabel"));
        loginLabel->setTextFormat(Qt::AutoText);
        loginLabel->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        gridLayout->addWidget(loginLabel, 1, 0, 1, 1);

        labelRequest_8 = new QLabel(frame);
        labelRequest_8->setObjectName(QString::fromUtf8("labelRequest_8"));
        labelRequest_8->setTextFormat(Qt::AutoText);

        gridLayout->addWidget(labelRequest_8, 1, 1, 1, 1);

        passwordLabel = new QLabel(frame);
        passwordLabel->setObjectName(QString::fromUtf8("passwordLabel"));
        passwordLabel->setTextFormat(Qt::AutoText);
        passwordLabel->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        gridLayout->addWidget(passwordLabel, 2, 0, 1, 1);

        labelRequest_9 = new QLabel(frame);
        labelRequest_9->setObjectName(QString::fromUtf8("labelRequest_9"));
        labelRequest_9->setTextFormat(Qt::AutoText);

        gridLayout->addWidget(labelRequest_9, 2, 1, 1, 1);

        password2Label = new QLabel(frame);
        password2Label->setObjectName(QString::fromUtf8("password2Label"));
        password2Label->setTextFormat(Qt::AutoText);
        password2Label->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        gridLayout->addWidget(password2Label, 3, 0, 1, 1);

        labelRequest_7 = new QLabel(frame);
        labelRequest_7->setObjectName(QString::fromUtf8("labelRequest_7"));
        labelRequest_7->setTextFormat(Qt::AutoText);

        gridLayout->addWidget(labelRequest_7, 3, 1, 1, 1);

        emailLabel = new QLabel(frame);
        emailLabel->setObjectName(QString::fromUtf8("emailLabel"));
        emailLabel->setAlignment(Qt::AlignRight|Qt::AlignTop|Qt::AlignTrailing);

        gridLayout->addWidget(emailLabel, 4, 0, 1, 1);

        emailEdit = new QLineEdit(frame);
        emailEdit->setObjectName(QString::fromUtf8("emailEdit"));

        gridLayout->addWidget(emailEdit, 4, 2, 1, 1);

        labelRequest_11 = new QLabel(frame);
        labelRequest_11->setObjectName(QString::fromUtf8("labelRequest_11"));
        labelRequest_11->setTextFormat(Qt::AutoText);
        labelRequest_11->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignTop);

        gridLayout->addWidget(labelRequest_11, 4, 1, 1, 1);


        verticalLayout_4->addLayout(gridLayout);


        verticalLayout->addWidget(frame);

        verticalSpacer_5 = new QSpacerItem(20, 0, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout->addItem(verticalSpacer_5);

        bottomLinksFrame = new QFrame(frame_2);
        bottomLinksFrame->setObjectName(QString::fromUtf8("bottomLinksFrame"));
        sizePolicy.setHeightForWidth(bottomLinksFrame->sizePolicy().hasHeightForWidth());
        bottomLinksFrame->setSizePolicy(sizePolicy);
        bottomLinksFrame->setFrameShape(QFrame::NoFrame);
        bottomLinksFrame->setFrameShadow(QFrame::Plain);
        bottomLinksFrame->setLineWidth(0);
        horizontalLayout_3 = new QHBoxLayout(bottomLinksFrame);
        horizontalLayout_3->setSpacing(30);
        horizontalLayout_3->setMargin(0);
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        horizontalLayout_3->setContentsMargins(-1, 0, -1, -1);
        horizontalSpacer_2 = new QSpacerItem(10, 14, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_2);

        backButton = new QPushButton(bottomLinksFrame);
        backButton->setObjectName(QString::fromUtf8("backButton"));
        backButton->setFont(font);
        backButton->setCursor(QCursor(Qt::PointingHandCursor));
        backButton->setFlat(true);

        horizontalLayout_3->addWidget(backButton);

        nextButton = new QPushButton(bottomLinksFrame);
        nextButton->setObjectName(QString::fromUtf8("nextButton"));
        nextButton->setFont(font);
        nextButton->setCursor(QCursor(Qt::PointingHandCursor));
        nextButton->setDefault(true);
        nextButton->setFlat(true);

        horizontalLayout_3->addWidget(nextButton);

        horizontalSpacer_3 = new QSpacerItem(10, 14, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_3);


        verticalLayout->addWidget(bottomLinksFrame);

        transBorder_4 = new QFrame(RegistrationWidgetClass);
        transBorder_4->setObjectName(QString::fromUtf8("transBorder_4"));
        transBorder_4->setGeometry(QRect(0, 0, 401, 2));
        transBorder_4->setMinimumSize(QSize(0, 2));
        transBorder_4->setMaximumSize(QSize(16777215, 2));
        transBorder_4->setFrameShape(QFrame::Box);
        transBorder_4->setFrameShadow(QFrame::Plain);
        transBorder = new QFrame(RegistrationWidgetClass);
        transBorder->setObjectName(QString::fromUtf8("transBorder"));
        transBorder->setGeometry(QRect(386, 2, 2, 263));
        transBorder->setMinimumSize(QSize(2, 263));
        transBorder->setMaximumSize(QSize(2, 263));
        transBorder->setFrameShape(QFrame::Box);
        transBorder->setFrameShadow(QFrame::Plain);
        transBorder->setLineWidth(1);
        transBorder_2 = new QFrame(RegistrationWidgetClass);
        transBorder_2->setObjectName(QString::fromUtf8("transBorder_2"));
        transBorder_2->setGeometry(QRect(0, 2, 2, 263));
        transBorder_2->setMinimumSize(QSize(2, 263));
        transBorder_2->setMaximumSize(QSize(2, 263));
        transBorder_2->setFrameShape(QFrame::Box);
        transBorder_2->setFrameShadow(QFrame::Plain);
        transBorder_3 = new QFrame(RegistrationWidgetClass);
        transBorder_3->setObjectName(QString::fromUtf8("transBorder_3"));
        transBorder_3->setGeometry(QRect(0, 265, 401, 2));
        transBorder_3->setMinimumSize(QSize(0, 2));
        transBorder_3->setMaximumSize(QSize(16777215, 2));
        transBorder_3->setFrameShape(QFrame::Box);
        transBorder_3->setFrameShadow(QFrame::Plain);
#ifndef QT_NO_SHORTCUT
        nameLabel->setBuddy(nameEdit);
        loginLabel->setBuddy(loginEdit);
        passwordLabel->setBuddy(passwordEdit);
        password2Label->setBuddy(password2Edit);
        emailLabel->setBuddy(emailEdit);
#endif // QT_NO_SHORTCUT
        QWidget::setTabOrder(nameEdit, loginEdit);
        QWidget::setTabOrder(loginEdit, passwordEdit);
        QWidget::setTabOrder(passwordEdit, password2Edit);
        QWidget::setTabOrder(password2Edit, receiveNewsCheckBox);

        retranslateUi(RegistrationWidgetClass);

        QMetaObject::connectSlotsByName(RegistrationWidgetClass);
    } // setupUi

    void retranslateUi(QWidget *RegistrationWidgetClass)
    {
#ifndef QT_NO_ACCESSIBILITY
        titleLabel->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "redLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        titleLabel->setText(QApplication::translate("RegistrationWidgetClass", "Title", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        label_3->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "redLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        label_3->setText(QApplication::translate("RegistrationWidgetClass", "* ", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        label_2->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "greyLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        label_2->setText(QApplication::translate("RegistrationWidgetClass", "- ", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        labelMandatoryFields->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "whiteLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        labelMandatoryFields->setText(QApplication::translate("RegistrationWidgetClass", "MandatoryFields", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        receiveNewsCheckBox->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "whiteLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        receiveNewsCheckBox->setText(QApplication::translate("RegistrationWidgetClass", "NewsAndOffersCheckBox", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        iAcceptCheckBox->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "whiteLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        iAcceptCheckBox->setText(QApplication::translate("RegistrationWidgetClass", "I accept", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        termOfUseButton->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "smallRedButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        termOfUseButton->setText(QApplication::translate("RegistrationWidgetClass", "terms of use", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        andLabel->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "whiteLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        andLabel->setText(QApplication::translate("RegistrationWidgetClass", "and", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        privacyPolicyButton->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "smallRedButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        privacyPolicyButton->setText(QApplication::translate("RegistrationWidgetClass", "privacy policy", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        nameLabel->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "whiteLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        nameLabel->setText(QApplication::translate("RegistrationWidgetClass", "Name", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        loginLabel->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "whiteLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        loginLabel->setText(QApplication::translate("RegistrationWidgetClass", "Login", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        labelRequest_8->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "redLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        labelRequest_8->setText(QApplication::translate("RegistrationWidgetClass", "* ", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        passwordLabel->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "whiteLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        passwordLabel->setText(QApplication::translate("RegistrationWidgetClass", "Password", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        labelRequest_9->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "redLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        labelRequest_9->setText(QApplication::translate("RegistrationWidgetClass", "*", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        password2Label->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "whiteLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        password2Label->setText(QApplication::translate("RegistrationWidgetClass", "Password2", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        labelRequest_7->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "redLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        labelRequest_7->setText(QApplication::translate("RegistrationWidgetClass", "*", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        emailLabel->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "whiteLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        emailLabel->setText(QApplication::translate("RegistrationWidgetClass", "Email", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        labelRequest_11->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "redLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        labelRequest_11->setText(QApplication::translate("RegistrationWidgetClass", "*", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        backButton->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "blueButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        backButton->setText(QApplication::translate("RegistrationWidgetClass", "Back", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        nextButton->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "blueButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        nextButton->setText(QApplication::translate("RegistrationWidgetClass", "Next", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        transBorder_4->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder_2->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder_3->setAccessibleName(QApplication::translate("RegistrationWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        Q_UNUSED(RegistrationWidgetClass);
    } // retranslateUi

};

namespace Ui {
    class RegistrationWidgetClass: public Ui_RegistrationWidgetClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_REGISTRATIONWIDGET_H
