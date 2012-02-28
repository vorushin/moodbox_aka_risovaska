/********************************************************************************
** Form generated from reading ui file 'findchannelframe.ui'
**
** Created: Tue May 5 10:34:40 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_FINDCHANNELFRAME_H
#define UI_FINDCHANNELFRAME_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QFrame>
#include <QtGui/QHeaderView>
#include <QtGui/QScrollArea>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_FindChannelFrameClass
{
public:
    QVBoxLayout *verticalLayout;
    QScrollArea *channelList;
    QWidget *scrollAreaWidgetContents;

    void setupUi(QFrame *FindChannelFrameClass)
    {
        if (FindChannelFrameClass->objectName().isEmpty())
            FindChannelFrameClass->setObjectName(QString::fromUtf8("FindChannelFrameClass"));
        FindChannelFrameClass->resize(489, 461);
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Expanding);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(FindChannelFrameClass->sizePolicy().hasHeightForWidth());
        FindChannelFrameClass->setSizePolicy(sizePolicy);
        verticalLayout = new QVBoxLayout(FindChannelFrameClass);
        verticalLayout->setSpacing(0);
        verticalLayout->setMargin(0);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        channelList = new QScrollArea(FindChannelFrameClass);
        channelList->setObjectName(QString::fromUtf8("channelList"));
        sizePolicy.setHeightForWidth(channelList->sizePolicy().hasHeightForWidth());
        channelList->setSizePolicy(sizePolicy);
        channelList->setFrameShadow(QFrame::Plain);
        channelList->setWidgetResizable(true);
        scrollAreaWidgetContents = new QWidget();
        scrollAreaWidgetContents->setObjectName(QString::fromUtf8("scrollAreaWidgetContents"));
        scrollAreaWidgetContents->setGeometry(QRect(0, 0, 487, 459));
        channelList->setWidget(scrollAreaWidgetContents);

        verticalLayout->addWidget(channelList);


        retranslateUi(FindChannelFrameClass);

        QMetaObject::connectSlotsByName(FindChannelFrameClass);
    } // setupUi

    void retranslateUi(QFrame *FindChannelFrameClass)
    {
        Q_UNUSED(FindChannelFrameClass);
    } // retranslateUi

};

namespace Ui {
    class FindChannelFrameClass: public Ui_FindChannelFrameClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_FINDCHANNELFRAME_H
