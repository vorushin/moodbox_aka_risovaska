/********************************************************************************
** Form generated from reading ui file 'historywindow.ui'
**
** Created: Tue May 5 10:34:43 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_HISTORYWINDOW_H
#define UI_HISTORYWINDOW_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QFrame>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include "historyitemlist.h"

QT_BEGIN_NAMESPACE

class Ui_HistoryWindowClass
{
public:
    QVBoxLayout *verticalLayout;
    QFrame *styledHistoryFrame;
    QVBoxLayout *verticalLayout_2;
    QFrame *frameTop;
    QVBoxLayout *verticalLayout_4;
    QLabel *historyTopLabel;
    MoodBox::HistoryItemList *historyItemsList;
    QFrame *frame;
    QVBoxLayout *verticalLayout_3;
    QLabel *label;
    QPushButton *publishButton;

    void setupUi(QWidget *HistoryWindowClass)
    {
        if (HistoryWindowClass->objectName().isEmpty())
            HistoryWindowClass->setObjectName(QString::fromUtf8("HistoryWindowClass"));
        HistoryWindowClass->resize(182, 372);
        verticalLayout = new QVBoxLayout(HistoryWindowClass);
        verticalLayout->setSpacing(0);
        verticalLayout->setMargin(0);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        styledHistoryFrame = new QFrame(HistoryWindowClass);
        styledHistoryFrame->setObjectName(QString::fromUtf8("styledHistoryFrame"));
        styledHistoryFrame->setFrameShape(QFrame::NoFrame);
        styledHistoryFrame->setFrameShadow(QFrame::Plain);
        styledHistoryFrame->setLineWidth(0);
        verticalLayout_2 = new QVBoxLayout(styledHistoryFrame);
        verticalLayout_2->setSpacing(0);
        verticalLayout_2->setMargin(11);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        verticalLayout_2->setContentsMargins(26, 11, -1, -1);
        frameTop = new QFrame(styledHistoryFrame);
        frameTop->setObjectName(QString::fromUtf8("frameTop"));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Minimum);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(frameTop->sizePolicy().hasHeightForWidth());
        frameTop->setSizePolicy(sizePolicy);
        frameTop->setFrameShape(QFrame::NoFrame);
        frameTop->setFrameShadow(QFrame::Raised);
        verticalLayout_4 = new QVBoxLayout(frameTop);
        verticalLayout_4->setSpacing(0);
        verticalLayout_4->setMargin(11);
        verticalLayout_4->setObjectName(QString::fromUtf8("verticalLayout_4"));
        verticalLayout_4->setContentsMargins(3, 0, 9, 0);
        historyTopLabel = new QLabel(frameTop);
        historyTopLabel->setObjectName(QString::fromUtf8("historyTopLabel"));
        sizePolicy.setHeightForWidth(historyTopLabel->sizePolicy().hasHeightForWidth());
        historyTopLabel->setSizePolicy(sizePolicy);
        historyTopLabel->setPixmap(QPixmap(QString::fromUtf8(":/MoodBox/Resources/history_top_line.png")));
        historyTopLabel->setAlignment(Qt::AlignBottom|Qt::AlignLeading|Qt::AlignLeft);

        verticalLayout_4->addWidget(historyTopLabel);


        verticalLayout_2->addWidget(frameTop);

        historyItemsList = new MoodBox::HistoryItemList(styledHistoryFrame);
        historyItemsList->setObjectName(QString::fromUtf8("historyItemsList"));
        historyItemsList->setFrameShape(QFrame::NoFrame);
        historyItemsList->setFrameShadow(QFrame::Plain);
        historyItemsList->setLineWidth(0);
        historyItemsList->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);
        historyItemsList->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        historyItemsList->setEditTriggers(QAbstractItemView::NoEditTriggers);
        historyItemsList->setDragDropMode(QAbstractItemView::NoDragDrop);
        historyItemsList->setSelectionMode(QAbstractItemView::ExtendedSelection);
        historyItemsList->setIconSize(QSize(123, 96));
        historyItemsList->setVerticalScrollMode(QAbstractItemView::ScrollPerPixel);
        historyItemsList->setMovement(QListView::Static);
        historyItemsList->setProperty("isWrapping", QVariant(true));
        historyItemsList->setLayoutMode(QListView::SinglePass);
        historyItemsList->setSpacing(2);
        historyItemsList->setViewMode(QListView::IconMode);
        historyItemsList->setUniformItemSizes(true);

        verticalLayout_2->addWidget(historyItemsList);

        frame = new QFrame(styledHistoryFrame);
        frame->setObjectName(QString::fromUtf8("frame"));
        frame->setMinimumSize(QSize(0, 40));
        frame->setFrameShape(QFrame::NoFrame);
        frame->setFrameShadow(QFrame::Raised);
        verticalLayout_3 = new QVBoxLayout(frame);
        verticalLayout_3->setSpacing(11);
        verticalLayout_3->setMargin(11);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        verticalLayout_3->setContentsMargins(3, 0, -1, 0);
        label = new QLabel(frame);
        label->setObjectName(QString::fromUtf8("label"));
        label->setPixmap(QPixmap(QString::fromUtf8(":/MoodBox/Resources/history_bottom_line.png")));

        verticalLayout_3->addWidget(label);

        publishButton = new QPushButton(frame);
        publishButton->setObjectName(QString::fromUtf8("publishButton"));
        publishButton->setEnabled(false);

        verticalLayout_3->addWidget(publishButton);


        verticalLayout_2->addWidget(frame);


        verticalLayout->addWidget(styledHistoryFrame);


        retranslateUi(HistoryWindowClass);

        QMetaObject::connectSlotsByName(HistoryWindowClass);
    } // setupUi

    void retranslateUi(QWidget *HistoryWindowClass)
    {
#ifndef QT_NO_TOOLTIP
        publishButton->setToolTip(QApplication::translate("HistoryWindowClass", "PublishButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        publishButton->setAccessibleName(QApplication::translate("HistoryWindowClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        Q_UNUSED(HistoryWindowClass);
    } // retranslateUi

};

namespace Ui {
    class HistoryWindowClass: public Ui_HistoryWindowClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_HISTORYWINDOW_H
