/********************************************************************************
** Form generated from reading ui file 'setupdialog.ui'
**
** Created: Tue May 5 10:34:53 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_SETUPDIALOG_H
#define UI_SETUPDIALOG_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialog>
#include <QtGui/QFrame>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QTabWidget>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include "profileframe.h"
#include "settingsframe.h"

QT_BEGIN_NAMESPACE

class Ui_SetupDialogClass
{
public:
    QVBoxLayout *verticalLayout_3;
    QFrame *styledFrame;
    QVBoxLayout *verticalLayout_2;
    QHBoxLayout *horizontalLayout_2;
    QSpacerItem *horizontalSpacer_4;
    QToolButton *closeToolButton;
    QVBoxLayout *verticalLayout;
    QTabWidget *setupTabWidget;
    QWidget *profileTab;
    QVBoxLayout *verticalLayout_5;
    MoodBox::ProfileFrame *profileFrame;
    QWidget *settingsTab;
    QVBoxLayout *verticalLayout_4;
    MoodBox::SettingsFrame *settingsFrame;
    QFrame *frameBottom;
    QHBoxLayout *_4;
    QSpacerItem *spacerItem;
    QPushButton *okButton;
    QPushButton *cancelButton;

    void setupUi(QDialog *SetupDialogClass)
    {
        if (SetupDialogClass->objectName().isEmpty())
            SetupDialogClass->setObjectName(QString::fromUtf8("SetupDialogClass"));
        SetupDialogClass->setWindowModality(Qt::NonModal);
        SetupDialogClass->resize(483, 506);
        verticalLayout_3 = new QVBoxLayout(SetupDialogClass);
        verticalLayout_3->setSpacing(0);
        verticalLayout_3->setMargin(0);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        verticalLayout_3->setSizeConstraint(QLayout::SetFixedSize);
        styledFrame = new QFrame(SetupDialogClass);
        styledFrame->setObjectName(QString::fromUtf8("styledFrame"));
        styledFrame->setFrameShape(QFrame::NoFrame);
        styledFrame->setFrameShadow(QFrame::Plain);
        styledFrame->setLineWidth(0);
        verticalLayout_2 = new QVBoxLayout(styledFrame);
        verticalLayout_2->setSpacing(0);
        verticalLayout_2->setMargin(0);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setSpacing(0);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        horizontalLayout_2->setContentsMargins(-1, 5, 5, -1);
        horizontalSpacer_4 = new QSpacerItem(10, 5, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_4);

        closeToolButton = new QToolButton(styledFrame);
        closeToolButton->setObjectName(QString::fromUtf8("closeToolButton"));
        closeToolButton->setFocusPolicy(Qt::NoFocus);
        closeToolButton->setLayoutDirection(Qt::LeftToRight);
        closeToolButton->setPopupMode(QToolButton::DelayedPopup);

        horizontalLayout_2->addWidget(closeToolButton);


        verticalLayout_2->addLayout(horizontalLayout_2);

        verticalLayout = new QVBoxLayout();
        verticalLayout->setSpacing(0);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        verticalLayout->setContentsMargins(1, 9, 1, 6);
        setupTabWidget = new QTabWidget(styledFrame);
        setupTabWidget->setObjectName(QString::fromUtf8("setupTabWidget"));
        setupTabWidget->setTabShape(QTabWidget::Rounded);
        profileTab = new QWidget();
        profileTab->setObjectName(QString::fromUtf8("profileTab"));
        verticalLayout_5 = new QVBoxLayout(profileTab);
        verticalLayout_5->setSpacing(0);
        verticalLayout_5->setMargin(0);
        verticalLayout_5->setObjectName(QString::fromUtf8("verticalLayout_5"));
        profileFrame = new MoodBox::ProfileFrame(profileTab);
        profileFrame->setObjectName(QString::fromUtf8("profileFrame"));
        profileFrame->setFrameShape(QFrame::NoFrame);
        profileFrame->setFrameShadow(QFrame::Plain);
        profileFrame->setLineWidth(0);

        verticalLayout_5->addWidget(profileFrame);

        setupTabWidget->addTab(profileTab, QString());
        settingsTab = new QWidget();
        settingsTab->setObjectName(QString::fromUtf8("settingsTab"));
        verticalLayout_4 = new QVBoxLayout(settingsTab);
        verticalLayout_4->setSpacing(0);
        verticalLayout_4->setMargin(0);
        verticalLayout_4->setObjectName(QString::fromUtf8("verticalLayout_4"));
        settingsFrame = new MoodBox::SettingsFrame(settingsTab);
        settingsFrame->setObjectName(QString::fromUtf8("settingsFrame"));
        settingsFrame->setFrameShape(QFrame::NoFrame);
        settingsFrame->setFrameShadow(QFrame::Plain);
        settingsFrame->setLineWidth(0);

        verticalLayout_4->addWidget(settingsFrame);

        setupTabWidget->addTab(settingsTab, QString());

        verticalLayout->addWidget(setupTabWidget);

        frameBottom = new QFrame(styledFrame);
        frameBottom->setObjectName(QString::fromUtf8("frameBottom"));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(frameBottom->sizePolicy().hasHeightForWidth());
        frameBottom->setSizePolicy(sizePolicy);
        frameBottom->setMinimumSize(QSize(16, 46));
        frameBottom->setFrameShape(QFrame::NoFrame);
        frameBottom->setFrameShadow(QFrame::Plain);
        frameBottom->setLineWidth(0);
        _4 = new QHBoxLayout(frameBottom);
        _4->setSpacing(0);
        _4->setMargin(0);
        _4->setObjectName(QString::fromUtf8("_4"));
        _4->setContentsMargins(-1, -1, 16, 5);
        spacerItem = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        _4->addItem(spacerItem);

        okButton = new QPushButton(frameBottom);
        okButton->setObjectName(QString::fromUtf8("okButton"));
        okButton->setMinimumSize(QSize(80, 0));
        okButton->setAutoDefault(false);
        okButton->setDefault(true);

        _4->addWidget(okButton);

        cancelButton = new QPushButton(frameBottom);
        cancelButton->setObjectName(QString::fromUtf8("cancelButton"));
        cancelButton->setMinimumSize(QSize(80, 0));
        cancelButton->setAutoDefault(false);

        _4->addWidget(cancelButton);


        verticalLayout->addWidget(frameBottom);


        verticalLayout_2->addLayout(verticalLayout);


        verticalLayout_3->addWidget(styledFrame);


        retranslateUi(SetupDialogClass);

        setupTabWidget->setCurrentIndex(0);


        QMetaObject::connectSlotsByName(SetupDialogClass);
    } // setupUi

    void retranslateUi(QDialog *SetupDialogClass)
    {
#ifndef QT_NO_ACCESSIBILITY
        closeToolButton->setAccessibleName(QApplication::translate("SetupDialogClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        setupTabWidget->setTabText(setupTabWidget->indexOf(profileTab), QApplication::translate("SetupDialogClass", "ProfileTab", 0, QApplication::UnicodeUTF8));
        setupTabWidget->setTabText(setupTabWidget->indexOf(settingsTab), QApplication::translate("SetupDialogClass", "SettingsTab", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        okButton->setAccessibleName(QApplication::translate("SetupDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        okButton->setText(QApplication::translate("SetupDialogClass", "OkButton", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        cancelButton->setAccessibleName(QApplication::translate("SetupDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        cancelButton->setText(QApplication::translate("SetupDialogClass", "CancelButton", 0, QApplication::UnicodeUTF8));
        Q_UNUSED(SetupDialogClass);
    } // retranslateUi

};

namespace Ui {
    class SetupDialogClass: public Ui_SetupDialogClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_SETUPDIALOG_H
