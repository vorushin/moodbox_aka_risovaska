/********************************************************************************
** Form generated from reading ui file 'settingsframe.ui'
**
** Created: Tue May 5 10:34:53 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_SETTINGSFRAME_H
#define UI_SETTINGSFRAME_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QComboBox>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_SettingsFrameClass
{
public:
    QVBoxLayout *verticalLayout;
    QGridLayout *gridLayout;
    QCheckBox *launchOnStartupCheckBox;
    QCheckBox *autoUpdateCheckBox;
    QSpacerItem *horizontalSpacer;
    QPushButton *checkForNewVersionButton;
    QCheckBox *playSoundCheckBox;
    QPushButton *clearHistoryButton;
    QSpacerItem *verticalSpacer;
    QFrame *line;
    QGridLayout *gridLayout_3;
    QLabel *serverConnectionPortLabel;
    QComboBox *serverPortCombo;
    QSpacerItem *verticalSpacer_2;
    QGridLayout *gridLayout_4;
    QCheckBox *useProxyCheckBox;
    QLabel *proxyTypeLabel;
    QLabel *proxyHostLabel;
    QComboBox *proxyTypeCombo;
    QLineEdit *proxyHostEdit;
    QLabel *proxyPortLabel;
    QLineEdit *proxyPortEdit;
    QCheckBox *proxyRequiresAuthenticationCheckBox;
    QLabel *proxyLoginLabel;
    QLabel *proxyPasswordLabel;
    QLineEdit *proxyPasswordEdit;
    QPushButton *setStandardProxyPortButton;
    QLineEdit *proxyLoginEdit;
    QSpacerItem *verticalSpacer_3;

    void setupUi(QFrame *SettingsFrameClass)
    {
        if (SettingsFrameClass->objectName().isEmpty())
            SettingsFrameClass->setObjectName(QString::fromUtf8("SettingsFrameClass"));
        SettingsFrameClass->resize(460, 413);
        SettingsFrameClass->setLineWidth(0);
        verticalLayout = new QVBoxLayout(SettingsFrameClass);
        verticalLayout->setSpacing(6);
        verticalLayout->setMargin(11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        gridLayout->setVerticalSpacing(14);
        launchOnStartupCheckBox = new QCheckBox(SettingsFrameClass);
        launchOnStartupCheckBox->setObjectName(QString::fromUtf8("launchOnStartupCheckBox"));

        gridLayout->addWidget(launchOnStartupCheckBox, 0, 0, 1, 1);

        autoUpdateCheckBox = new QCheckBox(SettingsFrameClass);
        autoUpdateCheckBox->setObjectName(QString::fromUtf8("autoUpdateCheckBox"));
        autoUpdateCheckBox->setMinimumSize(QSize(260, 0));

        gridLayout->addWidget(autoUpdateCheckBox, 1, 0, 1, 1);

        horizontalSpacer = new QSpacerItem(20, 20, QSizePolicy::MinimumExpanding, QSizePolicy::Minimum);

        gridLayout->addItem(horizontalSpacer, 1, 1, 1, 1);

        checkForNewVersionButton = new QPushButton(SettingsFrameClass);
        checkForNewVersionButton->setObjectName(QString::fromUtf8("checkForNewVersionButton"));

        gridLayout->addWidget(checkForNewVersionButton, 1, 2, 1, 1);

        playSoundCheckBox = new QCheckBox(SettingsFrameClass);
        playSoundCheckBox->setObjectName(QString::fromUtf8("playSoundCheckBox"));
        playSoundCheckBox->setChecked(true);

        gridLayout->addWidget(playSoundCheckBox, 2, 0, 1, 1);

        clearHistoryButton = new QPushButton(SettingsFrameClass);
        clearHistoryButton->setObjectName(QString::fromUtf8("clearHistoryButton"));

        gridLayout->addWidget(clearHistoryButton, 3, 2, 1, 1);


        verticalLayout->addLayout(gridLayout);

        verticalSpacer = new QSpacerItem(20, 1, QSizePolicy::Minimum, QSizePolicy::Fixed);

        verticalLayout->addItem(verticalSpacer);

        line = new QFrame(SettingsFrameClass);
        line->setObjectName(QString::fromUtf8("line"));
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(line);

        gridLayout_3 = new QGridLayout();
        gridLayout_3->setSpacing(6);
        gridLayout_3->setObjectName(QString::fromUtf8("gridLayout_3"));
        gridLayout_3->setHorizontalSpacing(30);
        gridLayout_3->setVerticalSpacing(8);
        gridLayout_3->setContentsMargins(30, 8, 30, -1);
        serverConnectionPortLabel = new QLabel(SettingsFrameClass);
        serverConnectionPortLabel->setObjectName(QString::fromUtf8("serverConnectionPortLabel"));

        gridLayout_3->addWidget(serverConnectionPortLabel, 1, 0, 1, 1);

        serverPortCombo = new QComboBox(SettingsFrameClass);
        serverPortCombo->setObjectName(QString::fromUtf8("serverPortCombo"));

        gridLayout_3->addWidget(serverPortCombo, 1, 1, 1, 1);

        verticalSpacer_2 = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        gridLayout_3->addItem(verticalSpacer_2, 0, 1, 1, 1);


        verticalLayout->addLayout(gridLayout_3);

        gridLayout_4 = new QGridLayout();
        gridLayout_4->setSpacing(6);
        gridLayout_4->setObjectName(QString::fromUtf8("gridLayout_4"));
        gridLayout_4->setVerticalSpacing(8);
        gridLayout_4->setContentsMargins(30, 8, 30, -1);
        useProxyCheckBox = new QCheckBox(SettingsFrameClass);
        useProxyCheckBox->setObjectName(QString::fromUtf8("useProxyCheckBox"));

        gridLayout_4->addWidget(useProxyCheckBox, 0, 0, 1, 3);

        proxyTypeLabel = new QLabel(SettingsFrameClass);
        proxyTypeLabel->setObjectName(QString::fromUtf8("proxyTypeLabel"));
        proxyTypeLabel->setMinimumSize(QSize(100, 0));

        gridLayout_4->addWidget(proxyTypeLabel, 1, 0, 1, 1);

        proxyHostLabel = new QLabel(SettingsFrameClass);
        proxyHostLabel->setObjectName(QString::fromUtf8("proxyHostLabel"));

        gridLayout_4->addWidget(proxyHostLabel, 2, 0, 1, 1);

        proxyTypeCombo = new QComboBox(SettingsFrameClass);
        proxyTypeCombo->setObjectName(QString::fromUtf8("proxyTypeCombo"));

        gridLayout_4->addWidget(proxyTypeCombo, 1, 1, 1, 2);

        proxyHostEdit = new QLineEdit(SettingsFrameClass);
        proxyHostEdit->setObjectName(QString::fromUtf8("proxyHostEdit"));

        gridLayout_4->addWidget(proxyHostEdit, 2, 1, 1, 2);

        proxyPortLabel = new QLabel(SettingsFrameClass);
        proxyPortLabel->setObjectName(QString::fromUtf8("proxyPortLabel"));

        gridLayout_4->addWidget(proxyPortLabel, 3, 0, 1, 1);

        proxyPortEdit = new QLineEdit(SettingsFrameClass);
        proxyPortEdit->setObjectName(QString::fromUtf8("proxyPortEdit"));

        gridLayout_4->addWidget(proxyPortEdit, 3, 1, 1, 1);

        proxyRequiresAuthenticationCheckBox = new QCheckBox(SettingsFrameClass);
        proxyRequiresAuthenticationCheckBox->setObjectName(QString::fromUtf8("proxyRequiresAuthenticationCheckBox"));

        gridLayout_4->addWidget(proxyRequiresAuthenticationCheckBox, 4, 0, 1, 3);

        proxyLoginLabel = new QLabel(SettingsFrameClass);
        proxyLoginLabel->setObjectName(QString::fromUtf8("proxyLoginLabel"));

        gridLayout_4->addWidget(proxyLoginLabel, 5, 0, 1, 1);

        proxyPasswordLabel = new QLabel(SettingsFrameClass);
        proxyPasswordLabel->setObjectName(QString::fromUtf8("proxyPasswordLabel"));

        gridLayout_4->addWidget(proxyPasswordLabel, 6, 0, 1, 1);

        proxyPasswordEdit = new QLineEdit(SettingsFrameClass);
        proxyPasswordEdit->setObjectName(QString::fromUtf8("proxyPasswordEdit"));
        proxyPasswordEdit->setEchoMode(QLineEdit::Password);

        gridLayout_4->addWidget(proxyPasswordEdit, 6, 1, 1, 2);

        setStandardProxyPortButton = new QPushButton(SettingsFrameClass);
        setStandardProxyPortButton->setObjectName(QString::fromUtf8("setStandardProxyPortButton"));

        gridLayout_4->addWidget(setStandardProxyPortButton, 3, 2, 1, 1);

        proxyLoginEdit = new QLineEdit(SettingsFrameClass);
        proxyLoginEdit->setObjectName(QString::fromUtf8("proxyLoginEdit"));

        gridLayout_4->addWidget(proxyLoginEdit, 5, 1, 1, 2);

        verticalSpacer_3 = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        gridLayout_4->addItem(verticalSpacer_3, 7, 1, 1, 1);


        verticalLayout->addLayout(gridLayout_4);

#ifndef QT_NO_SHORTCUT
        serverConnectionPortLabel->setBuddy(serverPortCombo);
        proxyTypeLabel->setBuddy(proxyTypeCombo);
        proxyHostLabel->setBuddy(proxyHostEdit);
        proxyPortLabel->setBuddy(proxyPortEdit);
        proxyLoginLabel->setBuddy(proxyLoginEdit);
        proxyPasswordLabel->setBuddy(proxyPasswordEdit);
#endif // QT_NO_SHORTCUT
        QWidget::setTabOrder(launchOnStartupCheckBox, autoUpdateCheckBox);
        QWidget::setTabOrder(autoUpdateCheckBox, checkForNewVersionButton);
        QWidget::setTabOrder(checkForNewVersionButton, serverPortCombo);
        QWidget::setTabOrder(serverPortCombo, useProxyCheckBox);
        QWidget::setTabOrder(useProxyCheckBox, proxyTypeCombo);
        QWidget::setTabOrder(proxyTypeCombo, proxyHostEdit);
        QWidget::setTabOrder(proxyHostEdit, proxyPortEdit);
        QWidget::setTabOrder(proxyPortEdit, setStandardProxyPortButton);
        QWidget::setTabOrder(setStandardProxyPortButton, proxyRequiresAuthenticationCheckBox);
        QWidget::setTabOrder(proxyRequiresAuthenticationCheckBox, proxyLoginEdit);
        QWidget::setTabOrder(proxyLoginEdit, proxyPasswordEdit);

        retranslateUi(SettingsFrameClass);

        QMetaObject::connectSlotsByName(SettingsFrameClass);
    } // setupUi

    void retranslateUi(QFrame *SettingsFrameClass)
    {
        launchOnStartupCheckBox->setText(QApplication::translate("SettingsFrameClass", "LaunchOnStartupCheckBox", 0, QApplication::UnicodeUTF8));
        autoUpdateCheckBox->setText(QApplication::translate("SettingsFrameClass", "AutoCheckNewVersionsCheckBox", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        checkForNewVersionButton->setAccessibleName(QApplication::translate("SettingsFrameClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        checkForNewVersionButton->setText(QApplication::translate("SettingsFrameClass", "CheckNowButton", 0, QApplication::UnicodeUTF8));
        playSoundCheckBox->setText(QApplication::translate("SettingsFrameClass", "PlaySoundCheckBox", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        clearHistoryButton->setAccessibleName(QApplication::translate("SettingsFrameClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        clearHistoryButton->setText(QApplication::translate("SettingsFrameClass", "ClearHistoryButton", 0, QApplication::UnicodeUTF8));
        serverConnectionPortLabel->setText(QApplication::translate("SettingsFrameClass", "ServerConnectionPort", 0, QApplication::UnicodeUTF8));
        useProxyCheckBox->setText(QApplication::translate("SettingsFrameClass", "UseProxyCheckBox", 0, QApplication::UnicodeUTF8));
        proxyTypeLabel->setText(QApplication::translate("SettingsFrameClass", "ProxyType", 0, QApplication::UnicodeUTF8));
        proxyHostLabel->setText(QApplication::translate("SettingsFrameClass", "Address", 0, QApplication::UnicodeUTF8));
        proxyPortLabel->setText(QApplication::translate("SettingsFrameClass", "Port", 0, QApplication::UnicodeUTF8));
        proxyRequiresAuthenticationCheckBox->setText(QApplication::translate("SettingsFrameClass", "ProxyRequiresAuthorizationCheckBox", 0, QApplication::UnicodeUTF8));
        proxyLoginLabel->setText(QApplication::translate("SettingsFrameClass", "Login", 0, QApplication::UnicodeUTF8));
        proxyPasswordLabel->setText(QApplication::translate("SettingsFrameClass", "Password", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        setStandardProxyPortButton->setAccessibleName(QApplication::translate("SettingsFrameClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        setStandardProxyPortButton->setText(QApplication::translate("SettingsFrameClass", "StandardButton", 0, QApplication::UnicodeUTF8));
        Q_UNUSED(SettingsFrameClass);
    } // retranslateUi

};

namespace Ui {
    class SettingsFrameClass: public Ui_SettingsFrameClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_SETTINGSFRAME_H
