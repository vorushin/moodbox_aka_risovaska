/********************************************************************************
** Form generated from reading ui file 'findpeopleframe.ui'
**
** Created: Tue May 5 10:34:41 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_FINDPEOPLEFRAME_H
#define UI_FINDPEOPLEFRAME_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QComboBox>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QGroupBox>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>
#include "uitools.h"

QT_BEGIN_NAMESPACE

class Ui_FindPeopleFrameClass
{
public:
    QVBoxLayout *verticalLayout;
    QSpacerItem *verticalSpacer;
    QLabel *descriptionLabel;
    QSpacerItem *verticalSpacer_2;
    QGridLayout *gridLayout;
    QLabel *searchNameLabel;
    QLineEdit *searchNameEdit;
    QPushButton *findPeopleButton;
    QCheckBox *advancedSearchCheckBox;
    QGroupBox *advancedSearchGroup;
    QGridLayout *gridLayout_2;
    QGridLayout *advancedSearchGLayout;
    QLabel *countryLabel;
    QComboBox *countryCombo;
    QLabel *genderLabel;
    QComboBox *genderCombo;
    QLabel *cityLabel;
    QLineEdit *cityEdit;
    QLabel *ageRangeLlabel;
    QComboBox *ageRangeCombo;
    QSpacerItem *horizontalSpacer;
    QSpacerItem *horizontalSpacer_5;
    QLabel *foundUsersLabel;
    QFrame *frameMid;
    MoodBox::ContactTable *contactTable;

    void setupUi(QFrame *FindPeopleFrameClass)
    {
        if (FindPeopleFrameClass->objectName().isEmpty())
            FindPeopleFrameClass->setObjectName(QString::fromUtf8("FindPeopleFrameClass"));
        FindPeopleFrameClass->resize(489, 461);
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Expanding);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(FindPeopleFrameClass->sizePolicy().hasHeightForWidth());
        FindPeopleFrameClass->setSizePolicy(sizePolicy);
        FindPeopleFrameClass->setFrameShape(QFrame::NoFrame);
        FindPeopleFrameClass->setFrameShadow(QFrame::Plain);
        FindPeopleFrameClass->setLineWidth(0);
        verticalLayout = new QVBoxLayout(FindPeopleFrameClass);
        verticalLayout->setSpacing(6);
        verticalLayout->setMargin(11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        verticalLayout->setContentsMargins(30, -1, 30, 30);
        verticalSpacer = new QSpacerItem(20, 15, QSizePolicy::Minimum, QSizePolicy::Fixed);

        verticalLayout->addItem(verticalSpacer);

        descriptionLabel = new QLabel(FindPeopleFrameClass);
        descriptionLabel->setObjectName(QString::fromUtf8("descriptionLabel"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(descriptionLabel->sizePolicy().hasHeightForWidth());
        descriptionLabel->setSizePolicy(sizePolicy1);

        verticalLayout->addWidget(descriptionLabel);

        verticalSpacer_2 = new QSpacerItem(429, 13, QSizePolicy::Minimum, QSizePolicy::Fixed);

        verticalLayout->addItem(verticalSpacer_2);

        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        gridLayout->setVerticalSpacing(1);
        searchNameLabel = new QLabel(FindPeopleFrameClass);
        searchNameLabel->setObjectName(QString::fromUtf8("searchNameLabel"));

        gridLayout->addWidget(searchNameLabel, 0, 0, 1, 1);

        searchNameEdit = new QLineEdit(FindPeopleFrameClass);
        searchNameEdit->setObjectName(QString::fromUtf8("searchNameEdit"));

        gridLayout->addWidget(searchNameEdit, 0, 1, 1, 1);

        findPeopleButton = new QPushButton(FindPeopleFrameClass);
        findPeopleButton->setObjectName(QString::fromUtf8("findPeopleButton"));

        gridLayout->addWidget(findPeopleButton, 0, 2, 1, 1);

        advancedSearchCheckBox = new QCheckBox(FindPeopleFrameClass);
        advancedSearchCheckBox->setObjectName(QString::fromUtf8("advancedSearchCheckBox"));

        gridLayout->addWidget(advancedSearchCheckBox, 1, 1, 1, 1);


        verticalLayout->addLayout(gridLayout);

        advancedSearchGroup = new QGroupBox(FindPeopleFrameClass);
        advancedSearchGroup->setObjectName(QString::fromUtf8("advancedSearchGroup"));
        sizePolicy1.setHeightForWidth(advancedSearchGroup->sizePolicy().hasHeightForWidth());
        advancedSearchGroup->setSizePolicy(sizePolicy1);
        advancedSearchGroup->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignVCenter);
        advancedSearchGroup->setChecked(false);
        gridLayout_2 = new QGridLayout(advancedSearchGroup);
        gridLayout_2->setSpacing(6);
        gridLayout_2->setMargin(11);
        gridLayout_2->setObjectName(QString::fromUtf8("gridLayout_2"));
        advancedSearchGLayout = new QGridLayout();
        advancedSearchGLayout->setSpacing(6);
        advancedSearchGLayout->setMargin(5);
        advancedSearchGLayout->setObjectName(QString::fromUtf8("advancedSearchGLayout"));
        countryLabel = new QLabel(advancedSearchGroup);
        countryLabel->setObjectName(QString::fromUtf8("countryLabel"));

        advancedSearchGLayout->addWidget(countryLabel, 0, 0, 1, 1);

        countryCombo = new QComboBox(advancedSearchGroup);
        countryCombo->setObjectName(QString::fromUtf8("countryCombo"));
        QSizePolicy sizePolicy2(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(countryCombo->sizePolicy().hasHeightForWidth());
        countryCombo->setSizePolicy(sizePolicy2);
        countryCombo->setMinimumSize(QSize(180, 0));
        countryCombo->setMaximumSize(QSize(180, 16777215));

        advancedSearchGLayout->addWidget(countryCombo, 0, 1, 1, 1);

        genderLabel = new QLabel(advancedSearchGroup);
        genderLabel->setObjectName(QString::fromUtf8("genderLabel"));

        advancedSearchGLayout->addWidget(genderLabel, 0, 3, 1, 1);

        genderCombo = new QComboBox(advancedSearchGroup);
        genderCombo->setObjectName(QString::fromUtf8("genderCombo"));
        sizePolicy2.setHeightForWidth(genderCombo->sizePolicy().hasHeightForWidth());
        genderCombo->setSizePolicy(sizePolicy2);
        genderCombo->setMinimumSize(QSize(89, 0));
        genderCombo->setMaximumSize(QSize(89, 16777215));

        advancedSearchGLayout->addWidget(genderCombo, 0, 4, 1, 1);

        cityLabel = new QLabel(advancedSearchGroup);
        cityLabel->setObjectName(QString::fromUtf8("cityLabel"));

        advancedSearchGLayout->addWidget(cityLabel, 1, 0, 1, 1);

        cityEdit = new QLineEdit(advancedSearchGroup);
        cityEdit->setObjectName(QString::fromUtf8("cityEdit"));
        sizePolicy2.setHeightForWidth(cityEdit->sizePolicy().hasHeightForWidth());
        cityEdit->setSizePolicy(sizePolicy2);
        cityEdit->setMinimumSize(QSize(180, 0));
        cityEdit->setMaximumSize(QSize(180, 16777215));

        advancedSearchGLayout->addWidget(cityEdit, 1, 1, 1, 1);

        ageRangeLlabel = new QLabel(advancedSearchGroup);
        ageRangeLlabel->setObjectName(QString::fromUtf8("ageRangeLlabel"));

        advancedSearchGLayout->addWidget(ageRangeLlabel, 1, 3, 1, 1);

        ageRangeCombo = new QComboBox(advancedSearchGroup);
        ageRangeCombo->setObjectName(QString::fromUtf8("ageRangeCombo"));
        sizePolicy2.setHeightForWidth(ageRangeCombo->sizePolicy().hasHeightForWidth());
        ageRangeCombo->setSizePolicy(sizePolicy2);
        ageRangeCombo->setMinimumSize(QSize(89, 0));
        ageRangeCombo->setMaximumSize(QSize(89, 16777215));

        advancedSearchGLayout->addWidget(ageRangeCombo, 1, 4, 1, 1);

        horizontalSpacer = new QSpacerItem(20, 20, QSizePolicy::Fixed, QSizePolicy::Minimum);

        advancedSearchGLayout->addItem(horizontalSpacer, 0, 2, 1, 1);

        horizontalSpacer_5 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        advancedSearchGLayout->addItem(horizontalSpacer_5, 0, 5, 1, 1);


        gridLayout_2->addLayout(advancedSearchGLayout, 0, 0, 1, 1);


        verticalLayout->addWidget(advancedSearchGroup);

        foundUsersLabel = new QLabel(FindPeopleFrameClass);
        foundUsersLabel->setObjectName(QString::fromUtf8("foundUsersLabel"));
        sizePolicy1.setHeightForWidth(foundUsersLabel->sizePolicy().hasHeightForWidth());
        foundUsersLabel->setSizePolicy(sizePolicy1);
        QFont font;
        font.setFamily(QString::fromUtf8("MS Shell Dlg 2"));
        font.setPointSize(9);
        font.setBold(true);
        font.setItalic(false);
        font.setUnderline(false);
        font.setWeight(75);
        font.setStrikeOut(false);
        foundUsersLabel->setFont(font);

        verticalLayout->addWidget(foundUsersLabel);

        frameMid = new QFrame(FindPeopleFrameClass);
        frameMid->setObjectName(QString::fromUtf8("frameMid"));
        frameMid->setFrameShape(QFrame::NoFrame);
        frameMid->setFrameShadow(QFrame::Raised);

        verticalLayout->addWidget(frameMid);

        contactTable = new MoodBox::ContactTable(FindPeopleFrameClass);
        if (contactTable->columnCount() < 4)
            contactTable->setColumnCount(4);
        contactTable->setObjectName(QString::fromUtf8("contactTable"));
        contactTable->setFrameShape(QFrame::Box);
        contactTable->setFrameShadow(QFrame::Plain);
        contactTable->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        contactTable->setEditTriggers(QAbstractItemView::NoEditTriggers);
        contactTable->setAlternatingRowColors(true);
        contactTable->setSelectionMode(QAbstractItemView::SingleSelection);
        contactTable->setSelectionBehavior(QAbstractItemView::SelectRows);
        contactTable->setVerticalScrollMode(QAbstractItemView::ScrollPerPixel);
        contactTable->setShowGrid(false);
        contactTable->setGridStyle(Qt::NoPen);
        contactTable->setWordWrap(false);
        contactTable->setCornerButtonEnabled(false);
        contactTable->setColumnCount(4);

        verticalLayout->addWidget(contactTable);

        QWidget::setTabOrder(searchNameEdit, findPeopleButton);
        QWidget::setTabOrder(findPeopleButton, advancedSearchCheckBox);
        QWidget::setTabOrder(advancedSearchCheckBox, countryCombo);
        QWidget::setTabOrder(countryCombo, cityEdit);
        QWidget::setTabOrder(cityEdit, genderCombo);
        QWidget::setTabOrder(genderCombo, ageRangeCombo);
        QWidget::setTabOrder(ageRangeCombo, contactTable);

        retranslateUi(FindPeopleFrameClass);

        QMetaObject::connectSlotsByName(FindPeopleFrameClass);
    } // setupUi

    void retranslateUi(QFrame *FindPeopleFrameClass)
    {
        descriptionLabel->setText(QApplication::translate("FindPeopleFrameClass", "TopText", 0, QApplication::UnicodeUTF8));
        searchNameLabel->setText(QApplication::translate("FindPeopleFrameClass", "NameLoginOrEmail", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        findPeopleButton->setToolTip(QApplication::translate("FindPeopleFrameClass", "FindButtonTip", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        findPeopleButton->setAccessibleName(QApplication::translate("FindPeopleFrameClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        findPeopleButton->setText(QApplication::translate("FindPeopleFrameClass", "FindButton", 0, QApplication::UnicodeUTF8));
        advancedSearchCheckBox->setText(QApplication::translate("FindPeopleFrameClass", "AdvancedSearchCheckBox", 0, QApplication::UnicodeUTF8));
        countryLabel->setText(QApplication::translate("FindPeopleFrameClass", "Country", 0, QApplication::UnicodeUTF8));
        genderLabel->setText(QApplication::translate("FindPeopleFrameClass", "Gender", 0, QApplication::UnicodeUTF8));
        cityLabel->setText(QApplication::translate("FindPeopleFrameClass", "City", 0, QApplication::UnicodeUTF8));
        ageRangeLlabel->setText(QApplication::translate("FindPeopleFrameClass", "Age", 0, QApplication::UnicodeUTF8));
        foundUsersLabel->setText(QApplication::translate("FindPeopleFrameClass", "FoundUsersCount_DT", 0, QApplication::UnicodeUTF8));
        Q_UNUSED(FindPeopleFrameClass);
    } // retranslateUi

};

namespace Ui {
    class FindPeopleFrameClass: public Ui_FindPeopleFrameClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_FINDPEOPLEFRAME_H
