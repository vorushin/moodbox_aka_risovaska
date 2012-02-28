/********************************************************************************
** Form generated from reading ui file 'profileframe.ui'
**
** Created: Tue May 5 10:34:50 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_PROFILEFRAME_H
#define UI_PROFILEFRAME_H

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
#include <QtGui/QTextEdit>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include "popupcombobox.h"
#include "useravatarbutton.h"

QT_BEGIN_NAMESPACE

class Ui_ProfileFrameClass
{
public:
    QGridLayout *gridLayout_2;
    QFrame *mainFrame;
    QGridLayout *gridLayout;
    QGridLayout *mainGLayout;
    QLabel *nameLabel;
    QLabel *countryLabel;
    QComboBox *countryCombo;
    QLabel *cityLabel;
    QComboBox *genderCombo;
    QLabel *genderLabel;
    QLabel *aboutLabel;
    QTextEdit *aboutTextEdit;
    QLabel *emailLabel;
    QLineEdit *emailEdit;
    QFrame *frame;
    MoodBox::PopUpComboBox *yearCombo;
    QLabel *yearLabel;
    QComboBox *dayCombo;
    QLabel *dayLabel;
    QComboBox *monthCombo;
    QLabel *monthLabel;
    QLabel *birthdayLabel;
    QCheckBox *receiveNewsCheckBox;
    QFrame *line;
    QCheckBox *allowWebTranslationCheckBox;
    QCheckBox *showMyFriendsCheckBox;
    QFrame *line_2;
    MoodBox::UserAvatarButton *avatarToolButton;
    QHBoxLayout *horizontalLayout;
    QLineEdit *nameEdit;
    QToolButton *changePasswordButton;
    QLineEdit *cityEdit;
    QVBoxLayout *verticalLayout;
    QLabel *mottoLabel;
    QTextEdit *mottoTextEdit;

    void setupUi(QFrame *ProfileFrameClass)
    {
        if (ProfileFrameClass->objectName().isEmpty())
            ProfileFrameClass->setObjectName(QString::fromUtf8("ProfileFrameClass"));
        ProfileFrameClass->resize(440, 455);
        ProfileFrameClass->setMinimumSize(QSize(410, 0));
        ProfileFrameClass->setLineWidth(0);
        gridLayout_2 = new QGridLayout(ProfileFrameClass);
        gridLayout_2->setSpacing(6);
        gridLayout_2->setMargin(11);
        gridLayout_2->setObjectName(QString::fromUtf8("gridLayout_2"));
        mainFrame = new QFrame(ProfileFrameClass);
        mainFrame->setObjectName(QString::fromUtf8("mainFrame"));
        mainFrame->setFrameShape(QFrame::NoFrame);
        mainFrame->setFrameShadow(QFrame::Plain);
        mainFrame->setLineWidth(0);
        gridLayout = new QGridLayout(mainFrame);
        gridLayout->setSpacing(6);
        gridLayout->setMargin(0);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        mainGLayout = new QGridLayout();
        mainGLayout->setSpacing(6);
        mainGLayout->setObjectName(QString::fromUtf8("mainGLayout"));
        mainGLayout->setHorizontalSpacing(6);
        mainGLayout->setVerticalSpacing(8);
        mainGLayout->setContentsMargins(30, 8, 30, -1);
        nameLabel = new QLabel(mainFrame);
        nameLabel->setObjectName(QString::fromUtf8("nameLabel"));
        nameLabel->setMinimumSize(QSize(100, 0));

        mainGLayout->addWidget(nameLabel, 4, 0, 1, 1);

        countryLabel = new QLabel(mainFrame);
        countryLabel->setObjectName(QString::fromUtf8("countryLabel"));

        mainGLayout->addWidget(countryLabel, 5, 0, 1, 1);

        countryCombo = new QComboBox(mainFrame);
        countryCombo->setObjectName(QString::fromUtf8("countryCombo"));

        mainGLayout->addWidget(countryCombo, 5, 1, 1, 1);

        cityLabel = new QLabel(mainFrame);
        cityLabel->setObjectName(QString::fromUtf8("cityLabel"));

        mainGLayout->addWidget(cityLabel, 6, 0, 1, 1);

        genderCombo = new QComboBox(mainFrame);
        genderCombo->setObjectName(QString::fromUtf8("genderCombo"));

        mainGLayout->addWidget(genderCombo, 7, 1, 1, 1);

        genderLabel = new QLabel(mainFrame);
        genderLabel->setObjectName(QString::fromUtf8("genderLabel"));

        mainGLayout->addWidget(genderLabel, 7, 0, 1, 1);

        aboutLabel = new QLabel(mainFrame);
        aboutLabel->setObjectName(QString::fromUtf8("aboutLabel"));

        mainGLayout->addWidget(aboutLabel, 9, 0, 1, 1);

        aboutTextEdit = new QTextEdit(mainFrame);
        aboutTextEdit->setObjectName(QString::fromUtf8("aboutTextEdit"));
        aboutTextEdit->setMaximumSize(QSize(16777215, 50));

        mainGLayout->addWidget(aboutTextEdit, 9, 1, 1, 1);

        emailLabel = new QLabel(mainFrame);
        emailLabel->setObjectName(QString::fromUtf8("emailLabel"));

        mainGLayout->addWidget(emailLabel, 10, 0, 1, 1);

        emailEdit = new QLineEdit(mainFrame);
        emailEdit->setObjectName(QString::fromUtf8("emailEdit"));

        mainGLayout->addWidget(emailEdit, 10, 1, 1, 1);

        frame = new QFrame(mainFrame);
        frame->setObjectName(QString::fromUtf8("frame"));
        frame->setMinimumSize(QSize(0, 37));
        frame->setFrameShape(QFrame::Panel);
        frame->setFrameShadow(QFrame::Plain);
        frame->setLineWidth(0);
        yearCombo = new MoodBox::PopUpComboBox(frame);
        yearCombo->setObjectName(QString::fromUtf8("yearCombo"));
        yearCombo->setGeometry(QRect(295, 2, 64, 20));
        yearLabel = new QLabel(frame);
        yearLabel->setObjectName(QString::fromUtf8("yearLabel"));
        yearLabel->setGeometry(QRect(298, 22, 41, 16));
        dayCombo = new QComboBox(frame);
        dayCombo->setObjectName(QString::fromUtf8("dayCombo"));
        dayCombo->setGeometry(QRect(107, 1, 51, 20));
        dayLabel = new QLabel(frame);
        dayLabel->setObjectName(QString::fromUtf8("dayLabel"));
        dayLabel->setGeometry(QRect(110, 22, 31, 16));
        monthCombo = new QComboBox(frame);
        monthCombo->setObjectName(QString::fromUtf8("monthCombo"));
        monthCombo->setGeometry(QRect(164, 2, 125, 20));
        monthLabel = new QLabel(frame);
        monthLabel->setObjectName(QString::fromUtf8("monthLabel"));
        monthLabel->setGeometry(QRect(167, 22, 71, 16));
        birthdayLabel = new QLabel(frame);
        birthdayLabel->setObjectName(QString::fromUtf8("birthdayLabel"));
        birthdayLabel->setGeometry(QRect(0, 1, 101, 20));

        mainGLayout->addWidget(frame, 8, 0, 1, 2);

        receiveNewsCheckBox = new QCheckBox(mainFrame);
        receiveNewsCheckBox->setObjectName(QString::fromUtf8("receiveNewsCheckBox"));
        receiveNewsCheckBox->setChecked(true);

        mainGLayout->addWidget(receiveNewsCheckBox, 11, 1, 1, 1);

        line = new QFrame(mainFrame);
        line->setObjectName(QString::fromUtf8("line"));
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);

        mainGLayout->addWidget(line, 12, 0, 1, 2);

        allowWebTranslationCheckBox = new QCheckBox(mainFrame);
        allowWebTranslationCheckBox->setObjectName(QString::fromUtf8("allowWebTranslationCheckBox"));

        mainGLayout->addWidget(allowWebTranslationCheckBox, 13, 0, 1, 2);

        showMyFriendsCheckBox = new QCheckBox(mainFrame);
        showMyFriendsCheckBox->setObjectName(QString::fromUtf8("showMyFriendsCheckBox"));

        mainGLayout->addWidget(showMyFriendsCheckBox, 14, 0, 1, 2);

        line_2 = new QFrame(mainFrame);
        line_2->setObjectName(QString::fromUtf8("line_2"));
        line_2->setFrameShape(QFrame::HLine);
        line_2->setFrameShadow(QFrame::Sunken);

        mainGLayout->addWidget(line_2, 3, 0, 1, 2);

        avatarToolButton = new MoodBox::UserAvatarButton(mainFrame);
        avatarToolButton->setObjectName(QString::fromUtf8("avatarToolButton"));
        avatarToolButton->setIconSize(QSize(74, 74));
        avatarToolButton->setChecked(false);
        avatarToolButton->setArrowType(Qt::NoArrow);

        mainGLayout->addWidget(avatarToolButton, 1, 0, 1, 1);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        nameEdit = new QLineEdit(mainFrame);
        nameEdit->setObjectName(QString::fromUtf8("nameEdit"));

        horizontalLayout->addWidget(nameEdit);

        changePasswordButton = new QToolButton(mainFrame);
        changePasswordButton->setObjectName(QString::fromUtf8("changePasswordButton"));

        horizontalLayout->addWidget(changePasswordButton);


        mainGLayout->addLayout(horizontalLayout, 4, 1, 1, 1);

        cityEdit = new QLineEdit(mainFrame);
        cityEdit->setObjectName(QString::fromUtf8("cityEdit"));

        mainGLayout->addWidget(cityEdit, 6, 1, 1, 1);

        verticalLayout = new QVBoxLayout();
        verticalLayout->setSpacing(2);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        mottoLabel = new QLabel(mainFrame);
        mottoLabel->setObjectName(QString::fromUtf8("mottoLabel"));

        verticalLayout->addWidget(mottoLabel);

        mottoTextEdit = new QTextEdit(mainFrame);
        mottoTextEdit->setObjectName(QString::fromUtf8("mottoTextEdit"));
        mottoTextEdit->setMaximumSize(QSize(16777215, 50));

        verticalLayout->addWidget(mottoTextEdit);


        mainGLayout->addLayout(verticalLayout, 1, 1, 1, 1);


        gridLayout->addLayout(mainGLayout, 0, 0, 1, 1);


        gridLayout_2->addWidget(mainFrame, 0, 0, 1, 1);

#ifndef QT_NO_SHORTCUT
        nameLabel->setBuddy(nameEdit);
        countryLabel->setBuddy(countryCombo);
        genderLabel->setBuddy(genderCombo);
        aboutLabel->setBuddy(aboutTextEdit);
        emailLabel->setBuddy(emailEdit);
#endif // QT_NO_SHORTCUT
        QWidget::setTabOrder(avatarToolButton, nameEdit);
        QWidget::setTabOrder(nameEdit, countryCombo);
        QWidget::setTabOrder(countryCombo, cityEdit);
        QWidget::setTabOrder(cityEdit, genderCombo);
        QWidget::setTabOrder(genderCombo, dayCombo);
        QWidget::setTabOrder(dayCombo, monthCombo);
        QWidget::setTabOrder(monthCombo, yearCombo);
        QWidget::setTabOrder(yearCombo, aboutTextEdit);
        QWidget::setTabOrder(aboutTextEdit, emailEdit);
        QWidget::setTabOrder(emailEdit, receiveNewsCheckBox);
        QWidget::setTabOrder(receiveNewsCheckBox, allowWebTranslationCheckBox);
        QWidget::setTabOrder(allowWebTranslationCheckBox, showMyFriendsCheckBox);

        retranslateUi(ProfileFrameClass);

        QMetaObject::connectSlotsByName(ProfileFrameClass);
    } // setupUi

    void retranslateUi(QFrame *ProfileFrameClass)
    {
        nameLabel->setText(QApplication::translate("ProfileFrameClass", "Name", 0, QApplication::UnicodeUTF8));
        countryLabel->setText(QApplication::translate("ProfileFrameClass", "Country", 0, QApplication::UnicodeUTF8));
        cityLabel->setText(QApplication::translate("ProfileFrameClass", "City", 0, QApplication::UnicodeUTF8));
        genderLabel->setText(QApplication::translate("ProfileFrameClass", "Gender", 0, QApplication::UnicodeUTF8));
        aboutLabel->setText(QApplication::translate("ProfileFrameClass", "About", 0, QApplication::UnicodeUTF8));
        emailLabel->setText(QApplication::translate("ProfileFrameClass", "Email", 0, QApplication::UnicodeUTF8));
        yearLabel->setText(QApplication::translate("ProfileFrameClass", "Year", 0, QApplication::UnicodeUTF8));
        dayLabel->setText(QApplication::translate("ProfileFrameClass", "Day", 0, QApplication::UnicodeUTF8));
        monthLabel->setText(QApplication::translate("ProfileFrameClass", "Month", 0, QApplication::UnicodeUTF8));
        birthdayLabel->setText(QApplication::translate("ProfileFrameClass", "Birthday", 0, QApplication::UnicodeUTF8));
        receiveNewsCheckBox->setText(QApplication::translate("ProfileFrameClass", "NewsAndOffersCheckBox", 0, QApplication::UnicodeUTF8));
        allowWebTranslationCheckBox->setText(QApplication::translate("ProfileFrameClass", "MoodTranslationCheckBox", 0, QApplication::UnicodeUTF8));
        showMyFriendsCheckBox->setText(QApplication::translate("ProfileFrameClass", "ShowFriendsCheckBox", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        avatarToolButton->setToolTip(QApplication::translate("ProfileFrameClass", "AvatarButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        avatarToolButton->setAccessibleName(QApplication::translate("ProfileFrameClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        changePasswordButton->setAccessibleName(QApplication::translate("ProfileFrameClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        changePasswordButton->setText(QApplication::translate("ProfileFrameClass", "ChangePasswordButton", 0, QApplication::UnicodeUTF8));
        mottoLabel->setText(QApplication::translate("ProfileFrameClass", "Motto", 0, QApplication::UnicodeUTF8));
        Q_UNUSED(ProfileFrameClass);
    } // retranslateUi

};

namespace Ui {
    class ProfileFrameClass: public Ui_ProfileFrameClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_PROFILEFRAME_H
