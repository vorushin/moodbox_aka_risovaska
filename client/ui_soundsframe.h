/********************************************************************************
** Form generated from reading ui file 'soundsframe.ui'
**
** Created: Tue May 5 10:34:54 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_SOUNDSFRAME_H
#define UI_SOUNDSFRAME_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QSlider>
#include <QtGui/QSpacerItem>
#include <QtGui/QToolButton>

QT_BEGIN_NAMESPACE

class Ui_SoundsFrameClass
{
public:
    QGridLayout *gridLayout_2;
    QGridLayout *gridLayout;
    QSlider *volumeHSlider;
    QCheckBox *muteAllCheckBox;
    QLabel *label;
    QToolButton *signInToolButton;
    QLabel *label_2;
    QLabel *label_3;
    QLabel *label_4;
    QLabel *label_5;
    QLabel *label_7;
    QLabel *label_10;
    QLabel *label_9;
    QLabel *label_8;
    QToolButton *signOutToolButton;
    QToolButton *onlineToolButton;
    QToolButton *offlineToolButton;
    QToolButton *updateToolButton;
    QToolButton *receivedToolButton;
    QToolButton *sentToolButton;
    QToolButton *authRequestToolButton;
    QToolButton *authReceivedToolButton;
    QToolButton *contactsReceivedToolButton;
    QSpacerItem *verticalSpacer;
    QSpacerItem *verticalSpacer_2;
    QSpacerItem *verticalSpacer_3;
    QSpacerItem *horizontalSpacer;
    QLabel *label_6;
    QSpacerItem *verticalSpacer_4;
    QSpacerItem *verticalSpacer_5;

    void setupUi(QFrame *SoundsFrameClass)
    {
        if (SoundsFrameClass->objectName().isEmpty())
            SoundsFrameClass->setObjectName(QString::fromUtf8("SoundsFrameClass"));
        SoundsFrameClass->resize(388, 445);
        gridLayout_2 = new QGridLayout(SoundsFrameClass);
        gridLayout_2->setSpacing(6);
        gridLayout_2->setMargin(11);
        gridLayout_2->setObjectName(QString::fromUtf8("gridLayout_2"));
        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setMargin(6);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        volumeHSlider = new QSlider(SoundsFrameClass);
        volumeHSlider->setObjectName(QString::fromUtf8("volumeHSlider"));
        volumeHSlider->setOrientation(Qt::Horizontal);

        gridLayout->addWidget(volumeHSlider, 0, 1, 1, 1);

        muteAllCheckBox = new QCheckBox(SoundsFrameClass);
        muteAllCheckBox->setObjectName(QString::fromUtf8("muteAllCheckBox"));

        gridLayout->addWidget(muteAllCheckBox, 0, 2, 1, 1);

        label = new QLabel(SoundsFrameClass);
        label->setObjectName(QString::fromUtf8("label"));

        gridLayout->addWidget(label, 2, 0, 1, 2);

        signInToolButton = new QToolButton(SoundsFrameClass);
        signInToolButton->setObjectName(QString::fromUtf8("signInToolButton"));
        signInToolButton->setCheckable(true);

        gridLayout->addWidget(signInToolButton, 2, 2, 1, 1);

        label_2 = new QLabel(SoundsFrameClass);
        label_2->setObjectName(QString::fromUtf8("label_2"));

        gridLayout->addWidget(label_2, 3, 0, 1, 2);

        label_3 = new QLabel(SoundsFrameClass);
        label_3->setObjectName(QString::fromUtf8("label_3"));

        gridLayout->addWidget(label_3, 5, 0, 1, 2);

        label_4 = new QLabel(SoundsFrameClass);
        label_4->setObjectName(QString::fromUtf8("label_4"));

        gridLayout->addWidget(label_4, 6, 0, 1, 2);

        label_5 = new QLabel(SoundsFrameClass);
        label_5->setObjectName(QString::fromUtf8("label_5"));

        gridLayout->addWidget(label_5, 8, 0, 1, 2);

        label_7 = new QLabel(SoundsFrameClass);
        label_7->setObjectName(QString::fromUtf8("label_7"));

        gridLayout->addWidget(label_7, 10, 0, 1, 2);

        label_10 = new QLabel(SoundsFrameClass);
        label_10->setObjectName(QString::fromUtf8("label_10"));

        gridLayout->addWidget(label_10, 12, 0, 1, 2);

        label_9 = new QLabel(SoundsFrameClass);
        label_9->setObjectName(QString::fromUtf8("label_9"));

        gridLayout->addWidget(label_9, 13, 0, 1, 2);

        label_8 = new QLabel(SoundsFrameClass);
        label_8->setObjectName(QString::fromUtf8("label_8"));

        gridLayout->addWidget(label_8, 14, 0, 1, 2);

        signOutToolButton = new QToolButton(SoundsFrameClass);
        signOutToolButton->setObjectName(QString::fromUtf8("signOutToolButton"));

        gridLayout->addWidget(signOutToolButton, 3, 2, 1, 1);

        onlineToolButton = new QToolButton(SoundsFrameClass);
        onlineToolButton->setObjectName(QString::fromUtf8("onlineToolButton"));

        gridLayout->addWidget(onlineToolButton, 5, 2, 1, 1);

        offlineToolButton = new QToolButton(SoundsFrameClass);
        offlineToolButton->setObjectName(QString::fromUtf8("offlineToolButton"));

        gridLayout->addWidget(offlineToolButton, 6, 2, 1, 1);

        updateToolButton = new QToolButton(SoundsFrameClass);
        updateToolButton->setObjectName(QString::fromUtf8("updateToolButton"));

        gridLayout->addWidget(updateToolButton, 8, 2, 1, 1);

        receivedToolButton = new QToolButton(SoundsFrameClass);
        receivedToolButton->setObjectName(QString::fromUtf8("receivedToolButton"));

        gridLayout->addWidget(receivedToolButton, 9, 2, 1, 1);

        sentToolButton = new QToolButton(SoundsFrameClass);
        sentToolButton->setObjectName(QString::fromUtf8("sentToolButton"));

        gridLayout->addWidget(sentToolButton, 10, 2, 1, 1);

        authRequestToolButton = new QToolButton(SoundsFrameClass);
        authRequestToolButton->setObjectName(QString::fromUtf8("authRequestToolButton"));

        gridLayout->addWidget(authRequestToolButton, 12, 2, 1, 1);

        authReceivedToolButton = new QToolButton(SoundsFrameClass);
        authReceivedToolButton->setObjectName(QString::fromUtf8("authReceivedToolButton"));

        gridLayout->addWidget(authReceivedToolButton, 13, 2, 1, 1);

        contactsReceivedToolButton = new QToolButton(SoundsFrameClass);
        contactsReceivedToolButton->setObjectName(QString::fromUtf8("contactsReceivedToolButton"));

        gridLayout->addWidget(contactsReceivedToolButton, 14, 2, 1, 1);

        verticalSpacer = new QSpacerItem(20, 16, QSizePolicy::Minimum, QSizePolicy::Fixed);

        gridLayout->addItem(verticalSpacer, 4, 1, 1, 1);

        verticalSpacer_2 = new QSpacerItem(20, 16, QSizePolicy::Minimum, QSizePolicy::Fixed);

        gridLayout->addItem(verticalSpacer_2, 7, 1, 1, 1);

        verticalSpacer_3 = new QSpacerItem(20, 16, QSizePolicy::Minimum, QSizePolicy::Fixed);

        gridLayout->addItem(verticalSpacer_3, 11, 1, 1, 1);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout->addItem(horizontalSpacer, 0, 0, 1, 1);

        label_6 = new QLabel(SoundsFrameClass);
        label_6->setObjectName(QString::fromUtf8("label_6"));

        gridLayout->addWidget(label_6, 9, 0, 1, 2);

        verticalSpacer_4 = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        gridLayout->addItem(verticalSpacer_4, 15, 1, 1, 1);

        verticalSpacer_5 = new QSpacerItem(20, 16, QSizePolicy::Minimum, QSizePolicy::Fixed);

        gridLayout->addItem(verticalSpacer_5, 1, 1, 1, 1);


        gridLayout_2->addLayout(gridLayout, 0, 0, 1, 1);


        retranslateUi(SoundsFrameClass);

        QMetaObject::connectSlotsByName(SoundsFrameClass);
    } // setupUi

    void retranslateUi(QFrame *SoundsFrameClass)
    {
        SoundsFrameClass->setWindowTitle(QApplication::translate("SoundsFrameClass", "SoundsFrame", 0, QApplication::UnicodeUTF8));
        muteAllCheckBox->setText(QApplication::translate("SoundsFrameClass", "Mute All", 0, QApplication::UnicodeUTF8));
        label->setText(QApplication::translate("SoundsFrameClass", "Sign in", 0, QApplication::UnicodeUTF8));
        signInToolButton->setText(QApplication::translate("SoundsFrameClass", "...", 0, QApplication::UnicodeUTF8));
        label_2->setText(QApplication::translate("SoundsFrameClass", "Sign out", 0, QApplication::UnicodeUTF8));
        label_3->setText(QApplication::translate("SoundsFrameClass", "Friends goes Online", 0, QApplication::UnicodeUTF8));
        label_4->setText(QApplication::translate("SoundsFrameClass", "Friends goes Offline", 0, QApplication::UnicodeUTF8));
        label_5->setText(QApplication::translate("SoundsFrameClass", "MoodBox update", 0, QApplication::UnicodeUTF8));
        label_7->setText(QApplication::translate("SoundsFrameClass", "Message sent", 0, QApplication::UnicodeUTF8));
        label_10->setText(QApplication::translate("SoundsFrameClass", "Authorization request", 0, QApplication::UnicodeUTF8));
        label_9->setText(QApplication::translate("SoundsFrameClass", "Authorization received", 0, QApplication::UnicodeUTF8));
        label_8->setText(QApplication::translate("SoundsFrameClass", "Contacts received", 0, QApplication::UnicodeUTF8));
        signOutToolButton->setText(QApplication::translate("SoundsFrameClass", "...", 0, QApplication::UnicodeUTF8));
        onlineToolButton->setText(QApplication::translate("SoundsFrameClass", "...", 0, QApplication::UnicodeUTF8));
        offlineToolButton->setText(QApplication::translate("SoundsFrameClass", "...", 0, QApplication::UnicodeUTF8));
        updateToolButton->setText(QApplication::translate("SoundsFrameClass", "...", 0, QApplication::UnicodeUTF8));
        receivedToolButton->setText(QApplication::translate("SoundsFrameClass", "...", 0, QApplication::UnicodeUTF8));
        sentToolButton->setText(QApplication::translate("SoundsFrameClass", "...", 0, QApplication::UnicodeUTF8));
        authRequestToolButton->setText(QApplication::translate("SoundsFrameClass", "...", 0, QApplication::UnicodeUTF8));
        authReceivedToolButton->setText(QApplication::translate("SoundsFrameClass", "...", 0, QApplication::UnicodeUTF8));
        contactsReceivedToolButton->setText(QApplication::translate("SoundsFrameClass", "...", 0, QApplication::UnicodeUTF8));
        label_6->setText(QApplication::translate("SoundsFrameClass", "Message received", 0, QApplication::UnicodeUTF8));
        Q_UNUSED(SoundsFrameClass);
    } // retranslateUi

};

namespace Ui {
    class SoundsFrameClass: public Ui_SoundsFrameClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_SOUNDSFRAME_H
