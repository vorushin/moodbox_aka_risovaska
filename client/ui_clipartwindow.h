/********************************************************************************
** Form generated from reading ui file 'clipartwindow.ui'
**
** Created: Tue May 5 10:34:37 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_CLIPARTWINDOW_H
#define UI_CLIPARTWINDOW_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QFrame>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QTabWidget>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include "clipartcategorieslist.h"
#include "clipartentrieslist.h"

QT_BEGIN_NAMESPACE

class Ui_ClipartWindowClass
{
public:
    QVBoxLayout *verticalLayout;
    QFrame *styledFrame;
    QVBoxLayout *verticalLayout_4;
    QHBoxLayout *horizontalLayout_2;
    QSpacerItem *horizontalSpacer_4;
    QToolButton *closeToolButton;
    QTabWidget *tabWidget;
    QWidget *tab;
    QVBoxLayout *verticalLayout_2;
    QHBoxLayout *horizontalLayout_20;
    MoodBox::ClipartCategoriesList *clipartCategorySelector;
    MoodBox::ClipartEntriesList *clipartEntriesList;
    QWidget *tab_2;
    QVBoxLayout *verticalLayout_3;
    QHBoxLayout *horizontalLayout;
    QLineEdit *folderLabel;
    QPushButton *browseButton;
    MoodBox::ClipartEntriesList *photoList;
    QWidget *tab_3;
    QVBoxLayout *verticalLayout_40;
    MoodBox::ClipartEntriesList *tileList;
    QHBoxLayout *horizontalLayout_3;
    QWidget *widget_2;
    QWidget *widget;

    void setupUi(QWidget *ClipartWindowClass)
    {
        if (ClipartWindowClass->objectName().isEmpty())
            ClipartWindowClass->setObjectName(QString::fromUtf8("ClipartWindowClass"));
        ClipartWindowClass->resize(420, 353);
        verticalLayout = new QVBoxLayout(ClipartWindowClass);
        verticalLayout->setSpacing(0);
        verticalLayout->setMargin(0);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        styledFrame = new QFrame(ClipartWindowClass);
        styledFrame->setObjectName(QString::fromUtf8("styledFrame"));
        styledFrame->setAcceptDrops(true);
        styledFrame->setFrameShape(QFrame::NoFrame);
        styledFrame->setFrameShadow(QFrame::Plain);
        styledFrame->setLineWidth(0);
        verticalLayout_4 = new QVBoxLayout(styledFrame);
        verticalLayout_4->setSpacing(0);
        verticalLayout_4->setMargin(11);
        verticalLayout_4->setObjectName(QString::fromUtf8("verticalLayout_4"));
        verticalLayout_4->setContentsMargins(1, 0, 1, 1);
        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setSpacing(6);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        horizontalLayout_2->setContentsMargins(-1, 5, 4, -1);
        horizontalSpacer_4 = new QSpacerItem(10, 5, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_4);

        closeToolButton = new QToolButton(styledFrame);
        closeToolButton->setObjectName(QString::fromUtf8("closeToolButton"));
        closeToolButton->setFocusPolicy(Qt::NoFocus);

        horizontalLayout_2->addWidget(closeToolButton);


        verticalLayout_4->addLayout(horizontalLayout_2);

        tabWidget = new QTabWidget(styledFrame);
        tabWidget->setObjectName(QString::fromUtf8("tabWidget"));
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy.setHorizontalStretch(1);
        sizePolicy.setVerticalStretch(1);
        sizePolicy.setHeightForWidth(tabWidget->sizePolicy().hasHeightForWidth());
        tabWidget->setSizePolicy(sizePolicy);
        tab = new QWidget();
        tab->setObjectName(QString::fromUtf8("tab"));
        verticalLayout_2 = new QVBoxLayout(tab);
        verticalLayout_2->setSpacing(6);
        verticalLayout_2->setMargin(11);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        horizontalLayout_20 = new QHBoxLayout();
        horizontalLayout_20->setSpacing(6);
        horizontalLayout_20->setObjectName(QString::fromUtf8("horizontalLayout_20"));
        horizontalLayout_20->setContentsMargins(-1, -1, -1, 0);
        clipartCategorySelector = new MoodBox::ClipartCategoriesList(tab);
        clipartCategorySelector->setObjectName(QString::fromUtf8("clipartCategorySelector"));

        horizontalLayout_20->addWidget(clipartCategorySelector);


        verticalLayout_2->addLayout(horizontalLayout_20);

        clipartEntriesList = new MoodBox::ClipartEntriesList(tab);
        clipartEntriesList->setObjectName(QString::fromUtf8("clipartEntriesList"));
        clipartEntriesList->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);
        clipartEntriesList->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        clipartEntriesList->setAutoScroll(false);
        clipartEntriesList->setEditTriggers(QAbstractItemView::NoEditTriggers);
        clipartEntriesList->setDragDropMode(QAbstractItemView::DragDrop);
        clipartEntriesList->setIconSize(QSize(96, 96));
        clipartEntriesList->setVerticalScrollMode(QAbstractItemView::ScrollPerPixel);
        clipartEntriesList->setFlow(QListView::LeftToRight);
        clipartEntriesList->setProperty("isWrapping", QVariant(true));
        clipartEntriesList->setResizeMode(QListView::Adjust);
        clipartEntriesList->setViewMode(QListView::IconMode);
        clipartEntriesList->setUniformItemSizes(true);

        verticalLayout_2->addWidget(clipartEntriesList);

        tabWidget->addTab(tab, QString());
        tab_2 = new QWidget();
        tab_2->setObjectName(QString::fromUtf8("tab_2"));
        verticalLayout_3 = new QVBoxLayout(tab_2);
        verticalLayout_3->setSpacing(6);
        verticalLayout_3->setMargin(11);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalLayout->setContentsMargins(-1, 0, -1, -1);
        folderLabel = new QLineEdit(tab_2);
        folderLabel->setObjectName(QString::fromUtf8("folderLabel"));
        folderLabel->setEnabled(false);
        folderLabel->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignVCenter);
        folderLabel->setReadOnly(true);

        horizontalLayout->addWidget(folderLabel);

        browseButton = new QPushButton(tab_2);
        browseButton->setObjectName(QString::fromUtf8("browseButton"));
        QSizePolicy sizePolicy1(QSizePolicy::Maximum, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(browseButton->sizePolicy().hasHeightForWidth());
        browseButton->setSizePolicy(sizePolicy1);

        horizontalLayout->addWidget(browseButton);


        verticalLayout_3->addLayout(horizontalLayout);

        photoList = new MoodBox::ClipartEntriesList(tab_2);
        photoList->setObjectName(QString::fromUtf8("photoList"));
        photoList->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);
        photoList->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        photoList->setAutoScroll(false);
        photoList->setEditTriggers(QAbstractItemView::NoEditTriggers);
        photoList->setDragDropMode(QAbstractItemView::DragOnly);
        photoList->setIconSize(QSize(96, 96));
        photoList->setVerticalScrollMode(QAbstractItemView::ScrollPerPixel);
        photoList->setFlow(QListView::LeftToRight);
        photoList->setProperty("isWrapping", QVariant(true));
        photoList->setResizeMode(QListView::Adjust);
        photoList->setUniformItemSizes(true);

        verticalLayout_3->addWidget(photoList);

        tabWidget->addTab(tab_2, QString());
        tab_3 = new QWidget();
        tab_3->setObjectName(QString::fromUtf8("tab_3"));
        verticalLayout_40 = new QVBoxLayout(tab_3);
        verticalLayout_40->setSpacing(6);
        verticalLayout_40->setMargin(11);
        verticalLayout_40->setObjectName(QString::fromUtf8("verticalLayout_40"));
        tileList = new MoodBox::ClipartEntriesList(tab_3);
        tileList->setObjectName(QString::fromUtf8("tileList"));
        tileList->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);
        tileList->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        tileList->setAutoScroll(false);
        tileList->setEditTriggers(QAbstractItemView::NoEditTriggers);
        tileList->setDragDropMode(QAbstractItemView::DragOnly);
        tileList->setIconSize(QSize(96, 96));
        tileList->setVerticalScrollMode(QAbstractItemView::ScrollPerPixel);
        tileList->setFlow(QListView::LeftToRight);
        tileList->setProperty("isWrapping", QVariant(true));
        tileList->setResizeMode(QListView::Adjust);
        tileList->setUniformItemSizes(true);

        verticalLayout_40->addWidget(tileList);

        tabWidget->addTab(tab_3, QString());

        verticalLayout_4->addWidget(tabWidget);

        horizontalLayout_3 = new QHBoxLayout();
        horizontalLayout_3->setSpacing(0);
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        widget_2 = new QWidget(styledFrame);
        widget_2->setObjectName(QString::fromUtf8("widget_2"));
        QSizePolicy sizePolicy2(QSizePolicy::Preferred, QSizePolicy::Minimum);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(widget_2->sizePolicy().hasHeightForWidth());
        widget_2->setSizePolicy(sizePolicy2);

        horizontalLayout_3->addWidget(widget_2);

        widget = new QWidget(styledFrame);
        widget->setObjectName(QString::fromUtf8("widget"));
        QSizePolicy sizePolicy3(QSizePolicy::Fixed, QSizePolicy::Minimum);
        sizePolicy3.setHorizontalStretch(0);
        sizePolicy3.setVerticalStretch(0);
        sizePolicy3.setHeightForWidth(widget->sizePolicy().hasHeightForWidth());
        widget->setSizePolicy(sizePolicy3);
        widget->setMinimumSize(QSize(16, 16));

        horizontalLayout_3->addWidget(widget);


        verticalLayout_4->addLayout(horizontalLayout_3);


        verticalLayout->addWidget(styledFrame);


        retranslateUi(ClipartWindowClass);

        tabWidget->setCurrentIndex(1);


        QMetaObject::connectSlotsByName(ClipartWindowClass);
    } // setupUi

    void retranslateUi(QWidget *ClipartWindowClass)
    {
#ifndef QT_NO_TOOLTIP
        closeToolButton->setToolTip(QApplication::translate("ClipartWindowClass", "CloseButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        closeToolButton->setAccessibleName(QApplication::translate("ClipartWindowClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        tabWidget->setTabText(tabWidget->indexOf(tab), QApplication::translate("ClipartWindowClass", "ClipartTabText", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        browseButton->setAccessibleName(QApplication::translate("ClipartWindowClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        browseButton->setText(QApplication::translate("ClipartWindowClass", "BrowseButtonText", 0, QApplication::UnicodeUTF8));
        tabWidget->setTabText(tabWidget->indexOf(tab_2), QApplication::translate("ClipartWindowClass", "PhotoTabText", 0, QApplication::UnicodeUTF8));
        tabWidget->setTabText(tabWidget->indexOf(tab_3), QApplication::translate("ClipartWindowClass", "BackgroundTabText", 0, QApplication::UnicodeUTF8));
        Q_UNUSED(ClipartWindowClass);
    } // retranslateUi

};

namespace Ui {
    class ClipartWindowClass: public Ui_ClipartWindowClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CLIPARTWINDOW_H
