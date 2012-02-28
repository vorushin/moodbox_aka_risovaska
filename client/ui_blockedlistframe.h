/********************************************************************************
** Form generated from reading ui file 'blockedlistframe.ui'
**
** Created: Tue May 5 10:34:34 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_BLOCKEDLISTFRAME_H
#define UI_BLOCKEDLISTFRAME_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QComboBox>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QListWidget>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_BlockedListFrameClass
{
public:
    QGridLayout *gridLayout_2;
    QGridLayout *gridLayout;
    QVBoxLayout *verticalLayout;
    QPushButton *unblockButton;
    QSpacerItem *verticalSpacer_2;
    QLabel *blockedLabel;
    QLabel *personToBlockLabel;
    QComboBox *personToBlockCombo;
    QPushButton *blockButton;
    QSpacerItem *verticalSpacer;
    QSpacerItem *horizontalSpacer;
    QListWidget *contactListWidget;

    void setupUi(QFrame *BlockedListFrameClass)
    {
        if (BlockedListFrameClass->objectName().isEmpty())
            BlockedListFrameClass->setObjectName(QString::fromUtf8("BlockedListFrameClass"));
        BlockedListFrameClass->resize(452, 410);
        gridLayout_2 = new QGridLayout(BlockedListFrameClass);
        gridLayout_2->setSpacing(6);
        gridLayout_2->setMargin(11);
        gridLayout_2->setObjectName(QString::fromUtf8("gridLayout_2"));
        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        gridLayout->setHorizontalSpacing(15);
        gridLayout->setVerticalSpacing(8);
        gridLayout->setContentsMargins(30, 8, 30, -1);
        verticalLayout = new QVBoxLayout();
        verticalLayout->setSpacing(6);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        unblockButton = new QPushButton(BlockedListFrameClass);
        unblockButton->setObjectName(QString::fromUtf8("unblockButton"));

        verticalLayout->addWidget(unblockButton);

        verticalSpacer_2 = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout->addItem(verticalSpacer_2);


        gridLayout->addLayout(verticalLayout, 1, 1, 1, 1);

        blockedLabel = new QLabel(BlockedListFrameClass);
        blockedLabel->setObjectName(QString::fromUtf8("blockedLabel"));

        gridLayout->addWidget(blockedLabel, 0, 0, 1, 1);

        personToBlockLabel = new QLabel(BlockedListFrameClass);
        personToBlockLabel->setObjectName(QString::fromUtf8("personToBlockLabel"));
        personToBlockLabel->setWordWrap(true);

        gridLayout->addWidget(personToBlockLabel, 3, 0, 1, 1);

        personToBlockCombo = new QComboBox(BlockedListFrameClass);
        personToBlockCombo->setObjectName(QString::fromUtf8("personToBlockCombo"));
        personToBlockCombo->setEditable(true);

        gridLayout->addWidget(personToBlockCombo, 4, 0, 1, 1);

        blockButton = new QPushButton(BlockedListFrameClass);
        blockButton->setObjectName(QString::fromUtf8("blockButton"));

        gridLayout->addWidget(blockButton, 4, 1, 1, 1);

        verticalSpacer = new QSpacerItem(20, 80, QSizePolicy::Minimum, QSizePolicy::MinimumExpanding);

        gridLayout->addItem(verticalSpacer, 2, 0, 1, 1);

        horizontalSpacer = new QSpacerItem(100, 20, QSizePolicy::MinimumExpanding, QSizePolicy::Minimum);

        gridLayout->addItem(horizontalSpacer, 2, 1, 1, 1);

        contactListWidget = new QListWidget(BlockedListFrameClass);
        contactListWidget->setObjectName(QString::fromUtf8("contactListWidget"));

        gridLayout->addWidget(contactListWidget, 1, 0, 1, 1);


        gridLayout_2->addLayout(gridLayout, 0, 0, 1, 1);


        retranslateUi(BlockedListFrameClass);

        QMetaObject::connectSlotsByName(BlockedListFrameClass);
    } // setupUi

    void retranslateUi(QFrame *BlockedListFrameClass)
    {
        BlockedListFrameClass->setWindowTitle(QApplication::translate("BlockedListFrameClass", "BlockedListFrame", 0, QApplication::UnicodeUTF8));
        unblockButton->setText(QApplication::translate("BlockedListFrameClass", "Unblock this Person", 0, QApplication::UnicodeUTF8));
        blockedLabel->setText(QApplication::translate("BlockedListFrameClass", "Blocked people:", 0, QApplication::UnicodeUTF8));
        personToBlockLabel->setText(QApplication::translate("BlockedListFrameClass", "You can prevent a person from contacting you by selecting their name from the list below, or by entering their MoodBox Name", 0, QApplication::UnicodeUTF8));
        blockButton->setText(QApplication::translate("BlockedListFrameClass", "Block this Person", 0, QApplication::UnicodeUTF8));
        Q_UNUSED(BlockedListFrameClass);
    } // retranslateUi

};

namespace Ui {
    class BlockedListFrameClass: public Ui_BlockedListFrameClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_BLOCKEDLISTFRAME_H
