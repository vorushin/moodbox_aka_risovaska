/********************************************************************************
** Form generated from reading ui file 'contactinfodialog.ui'
**
** Created: Tue May 5 10:34:39 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_CONTACTINFODIALOG_H
#define UI_CONTACTINFODIALOG_H

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
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include "contactavatarbutton.h"

QT_BEGIN_NAMESPACE

class Ui_ContactInfoDialogClass
{
public:
    QVBoxLayout *verticalLayout_3;
    QFrame *styledFrame;
    QVBoxLayout *verticalLayout_4;
    QHBoxLayout *horizontalLayout_2;
    QSpacerItem *horizontalSpacer_4;
    QToolButton *closeToolButton;
    QLabel *titleLabel;
    QFrame *mainDialogFrame;
    QVBoxLayout *verticalLayout_2;
    QHBoxLayout *topHLayout;
    MoodBox::ContactAvatarButton *contactAvatarToolButton;
    QSpacerItem *horizontalSpacer_2;
    QVBoxLayout *verticalLayout;
    QHBoxLayout *horizontalLayout;
    QPushButton *nameLinkButton;
    QSpacerItem *horizontalSpacer;
    QLabel *mottoLabel;
    QSpacerItem *verticalSpacer;
    QGridLayout *infoGLayout;
    QLabel *genderIcoLabel;
    QHBoxLayout *horizontalLayout_3;
    QLabel *cityLabel;
    QLabel *countryLabel;
    QSpacerItem *horizontalSpacer_5;
    QLabel *aboutLabel;
    QLabel *ageLabel;
    QSpacerItem *verticalSpacer_2;
    QHBoxLayout *closeHLayout;
    QSpacerItem *horizontalSpacer_3;
    QPushButton *addAsFriendButton;
    QPushButton *closeButton;

    void setupUi(QDialog *ContactInfoDialogClass)
    {
        if (ContactInfoDialogClass->objectName().isEmpty())
            ContactInfoDialogClass->setObjectName(QString::fromUtf8("ContactInfoDialogClass"));
        ContactInfoDialogClass->setWindowModality(Qt::NonModal);
        ContactInfoDialogClass->resize(256, 314);
        verticalLayout_3 = new QVBoxLayout(ContactInfoDialogClass);
        verticalLayout_3->setSpacing(6);
        verticalLayout_3->setMargin(0);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        styledFrame = new QFrame(ContactInfoDialogClass);
        styledFrame->setObjectName(QString::fromUtf8("styledFrame"));
        styledFrame->setFrameShape(QFrame::NoFrame);
        styledFrame->setFrameShadow(QFrame::Plain);
        styledFrame->setLineWidth(0);
        verticalLayout_4 = new QVBoxLayout(styledFrame);
        verticalLayout_4->setSpacing(0);
        verticalLayout_4->setMargin(11);
        verticalLayout_4->setObjectName(QString::fromUtf8("verticalLayout_4"));
        verticalLayout_4->setContentsMargins(1, 0, 1, 5);
        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setSpacing(6);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        horizontalLayout_2->setContentsMargins(-1, 5, 4, -1);
        horizontalSpacer_4 = new QSpacerItem(10, 5, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_4);

        closeToolButton = new QToolButton(styledFrame);
        closeToolButton->setObjectName(QString::fromUtf8("closeToolButton"));
        closeToolButton->setFocusPolicy(Qt::NoFocus);
        closeToolButton->setLayoutDirection(Qt::LeftToRight);
        closeToolButton->setPopupMode(QToolButton::DelayedPopup);

        horizontalLayout_2->addWidget(closeToolButton);


        verticalLayout_4->addLayout(horizontalLayout_2);

        titleLabel = new QLabel(styledFrame);
        titleLabel->setObjectName(QString::fromUtf8("titleLabel"));
        titleLabel->setMargin(6);
        titleLabel->setIndent(4);

        verticalLayout_4->addWidget(titleLabel);

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
        verticalLayout_2 = new QVBoxLayout(mainDialogFrame);
        verticalLayout_2->setSpacing(6);
        verticalLayout_2->setMargin(11);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        verticalLayout_2->setContentsMargins(-1, 0, -1, -1);
        topHLayout = new QHBoxLayout();
        topHLayout->setSpacing(6);
        topHLayout->setObjectName(QString::fromUtf8("topHLayout"));
        topHLayout->setContentsMargins(-1, 9, -1, -1);
        contactAvatarToolButton = new MoodBox::ContactAvatarButton(mainDialogFrame);
        contactAvatarToolButton->setObjectName(QString::fromUtf8("contactAvatarToolButton"));
        contactAvatarToolButton->setIconSize(QSize(74, 74));
        contactAvatarToolButton->setAutoRaise(true);

        topHLayout->addWidget(contactAvatarToolButton);

        horizontalSpacer_2 = new QSpacerItem(10, 20, QSizePolicy::Fixed, QSizePolicy::Minimum);

        topHLayout->addItem(horizontalSpacer_2);

        verticalLayout = new QVBoxLayout();
        verticalLayout->setSpacing(6);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        nameLinkButton = new QPushButton(mainDialogFrame);
        nameLinkButton->setObjectName(QString::fromUtf8("nameLinkButton"));
        QFont font;
        font.setFamily(QString::fromUtf8("MS Shell Dlg 2"));
        font.setPointSize(8);
        font.setBold(false);
        font.setItalic(false);
        font.setUnderline(true);
        font.setWeight(50);
        font.setStrikeOut(false);
        nameLinkButton->setFont(font);
        nameLinkButton->setCursor(QCursor(Qt::PointingHandCursor));
        nameLinkButton->setFlat(true);

        horizontalLayout->addWidget(nameLinkButton);

        horizontalSpacer = new QSpacerItem(40, 10, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);


        verticalLayout->addLayout(horizontalLayout);

        mottoLabel = new QLabel(mainDialogFrame);
        mottoLabel->setObjectName(QString::fromUtf8("mottoLabel"));
        QSizePolicy sizePolicy1(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mottoLabel->sizePolicy().hasHeightForWidth());
        mottoLabel->setSizePolicy(sizePolicy1);
        mottoLabel->setMinimumSize(QSize(0, 30));
        mottoLabel->setFrameShape(QFrame::NoFrame);
        mottoLabel->setLineWidth(0);
        mottoLabel->setWordWrap(true);

        verticalLayout->addWidget(mottoLabel);


        topHLayout->addLayout(verticalLayout);


        verticalLayout_2->addLayout(topHLayout);

        verticalSpacer = new QSpacerItem(236, 13, QSizePolicy::Minimum, QSizePolicy::Fixed);

        verticalLayout_2->addItem(verticalSpacer);

        infoGLayout = new QGridLayout();
        infoGLayout->setSpacing(6);
        infoGLayout->setObjectName(QString::fromUtf8("infoGLayout"));
        infoGLayout->setContentsMargins(-1, -1, -1, 5);
        genderIcoLabel = new QLabel(mainDialogFrame);
        genderIcoLabel->setObjectName(QString::fromUtf8("genderIcoLabel"));
        genderIcoLabel->setMinimumSize(QSize(16, 16));
        genderIcoLabel->setMaximumSize(QSize(16, 16));
        genderIcoLabel->setAlignment(Qt::AlignCenter|Qt::AlignHCenter|Qt::AlignLeading|Qt::AlignLeft|Qt::AlignVCenter);

        infoGLayout->addWidget(genderIcoLabel, 0, 0, 1, 1);

        horizontalLayout_3 = new QHBoxLayout();
        horizontalLayout_3->setSpacing(6);
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        cityLabel = new QLabel(mainDialogFrame);
        cityLabel->setObjectName(QString::fromUtf8("cityLabel"));

        horizontalLayout_3->addWidget(cityLabel);

        countryLabel = new QLabel(mainDialogFrame);
        countryLabel->setObjectName(QString::fromUtf8("countryLabel"));

        horizontalLayout_3->addWidget(countryLabel);

        horizontalSpacer_5 = new QSpacerItem(0, 16, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_5);


        infoGLayout->addLayout(horizontalLayout_3, 1, 1, 1, 2);

        aboutLabel = new QLabel(mainDialogFrame);
        aboutLabel->setObjectName(QString::fromUtf8("aboutLabel"));
        sizePolicy1.setHeightForWidth(aboutLabel->sizePolicy().hasHeightForWidth());
        aboutLabel->setSizePolicy(sizePolicy1);
        aboutLabel->setFrameShape(QFrame::NoFrame);
        aboutLabel->setLineWidth(0);
        aboutLabel->setWordWrap(true);

        infoGLayout->addWidget(aboutLabel, 2, 1, 1, 1);

        ageLabel = new QLabel(mainDialogFrame);
        ageLabel->setObjectName(QString::fromUtf8("ageLabel"));

        infoGLayout->addWidget(ageLabel, 0, 1, 1, 2);


        verticalLayout_2->addLayout(infoGLayout);

        verticalSpacer_2 = new QSpacerItem(20, 0, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_2->addItem(verticalSpacer_2);


        verticalLayout_4->addWidget(mainDialogFrame);

        closeHLayout = new QHBoxLayout();
        closeHLayout->setSpacing(6);
        closeHLayout->setObjectName(QString::fromUtf8("closeHLayout"));
        closeHLayout->setContentsMargins(-1, 9, 9, 6);
        horizontalSpacer_3 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        closeHLayout->addItem(horizontalSpacer_3);

        addAsFriendButton = new QPushButton(styledFrame);
        addAsFriendButton->setObjectName(QString::fromUtf8("addAsFriendButton"));

        closeHLayout->addWidget(addAsFriendButton);

        closeButton = new QPushButton(styledFrame);
        closeButton->setObjectName(QString::fromUtf8("closeButton"));

        closeHLayout->addWidget(closeButton);


        verticalLayout_4->addLayout(closeHLayout);


        verticalLayout_3->addWidget(styledFrame);

        QWidget::setTabOrder(closeButton, contactAvatarToolButton);

        retranslateUi(ContactInfoDialogClass);

        QMetaObject::connectSlotsByName(ContactInfoDialogClass);
    } // setupUi

    void retranslateUi(QDialog *ContactInfoDialogClass)
    {
#ifndef QT_NO_ACCESSIBILITY
        closeToolButton->setAccessibleName(QApplication::translate("ContactInfoDialogClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        titleLabel->setText(QApplication::translate("ContactInfoDialogClass", "Title_DT", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        contactAvatarToolButton->setAccessibleName(QApplication::translate("ContactInfoDialogClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        nameLinkButton->setAccessibleName(QApplication::translate("ContactInfoDialogClass", "blueButton14", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        nameLinkButton->setText(QApplication::translate("ContactInfoDialogClass", "UserName_DT", 0, QApplication::UnicodeUTF8));
        mottoLabel->setText(QApplication::translate("ContactInfoDialogClass", "Motto_DT", 0, QApplication::UnicodeUTF8));
        cityLabel->setText(QApplication::translate("ContactInfoDialogClass", "City_DT", 0, QApplication::UnicodeUTF8));
        countryLabel->setText(QApplication::translate("ContactInfoDialogClass", "Country_DT", 0, QApplication::UnicodeUTF8));
        aboutLabel->setText(QApplication::translate("ContactInfoDialogClass", "About_DT", 0, QApplication::UnicodeUTF8));
        ageLabel->setText(QApplication::translate("ContactInfoDialogClass", "Age_DT", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        addAsFriendButton->setAccessibleName(QApplication::translate("ContactInfoDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        addAsFriendButton->setText(QApplication::translate("ContactInfoDialogClass", "AddAsFriendButton", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        closeButton->setAccessibleName(QApplication::translate("ContactInfoDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        closeButton->setText(QApplication::translate("ContactInfoDialogClass", "CloseButton", 0, QApplication::UnicodeUTF8));
        Q_UNUSED(ContactInfoDialogClass);
    } // retranslateUi

};

namespace Ui {
    class ContactInfoDialogClass: public Ui_ContactInfoDialogClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CONTACTINFODIALOG_H
