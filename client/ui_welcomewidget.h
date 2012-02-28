/********************************************************************************
** Form generated from reading ui file 'welcomewidget.ui'
**
** Created: Tue May 5 10:34:57 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_WELCOMEWIDGET_H
#define UI_WELCOMEWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_WelcomeWidgetClass
{
public:
    QFrame *frame_2;
    QVBoxLayout *verticalLayout_2;
    QFrame *topTitleFrame;
    QHBoxLayout *horizontalLayout;
    QLabel *titleLabel;
    QSpacerItem *verticalSpacer_5;
    QFrame *frame;
    QVBoxLayout *verticalLayout;
    QGridLayout *gridLayout;
    QLabel *findFriendsCommentLabel;
    QLabel *profileCommentLabel;
    QSpacerItem *verticalSpacer_3;
    QHBoxLayout *horizontalLayout_3;
    QPushButton *profileButton;
    QSpacerItem *horizontalSpacer_3;
    QHBoxLayout *horizontalLayout_4;
    QPushButton *findFriendsButton;
    QSpacerItem *horizontalSpacer_4;
    QSpacerItem *verticalSpacer_4;
    QFrame *bottomLinksFrame;
    QHBoxLayout *horizontalLayout_2;
    QSpacerItem *horizontalSpacer_2;
    QPushButton *finishButton;
    QSpacerItem *horizontalSpacer;
    QFrame *transBorder_4;
    QFrame *transBorder;
    QFrame *transBorder_2;
    QFrame *transBorder_3;

    void setupUi(QWidget *WelcomeWidgetClass)
    {
        if (WelcomeWidgetClass->objectName().isEmpty())
            WelcomeWidgetClass->setObjectName(QString::fromUtf8("WelcomeWidgetClass"));
        WelcomeWidgetClass->resize(388, 267);
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(WelcomeWidgetClass->sizePolicy().hasHeightForWidth());
        WelcomeWidgetClass->setSizePolicy(sizePolicy);
        frame_2 = new QFrame(WelcomeWidgetClass);
        frame_2->setObjectName(QString::fromUtf8("frame_2"));
        frame_2->setGeometry(QRect(9, 9, 370, 249));
        frame_2->setFrameShape(QFrame::NoFrame);
        frame_2->setFrameShadow(QFrame::Plain);
        frame_2->setLineWidth(0);
        verticalLayout_2 = new QVBoxLayout(frame_2);
        verticalLayout_2->setSpacing(6);
        verticalLayout_2->setMargin(0);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        topTitleFrame = new QFrame(frame_2);
        topTitleFrame->setObjectName(QString::fromUtf8("topTitleFrame"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(topTitleFrame->sizePolicy().hasHeightForWidth());
        topTitleFrame->setSizePolicy(sizePolicy1);
        topTitleFrame->setFrameShape(QFrame::NoFrame);
        topTitleFrame->setFrameShadow(QFrame::Plain);
        topTitleFrame->setLineWidth(0);
        horizontalLayout = new QHBoxLayout(topTitleFrame);
        horizontalLayout->setSpacing(0);
        horizontalLayout->setMargin(0);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalLayout->setContentsMargins(-1, -1, -1, 0);
        titleLabel = new QLabel(topTitleFrame);
        titleLabel->setObjectName(QString::fromUtf8("titleLabel"));
        titleLabel->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignVCenter);

        horizontalLayout->addWidget(titleLabel);


        verticalLayout_2->addWidget(topTitleFrame);

        verticalSpacer_5 = new QSpacerItem(20, 0, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_2->addItem(verticalSpacer_5);

        frame = new QFrame(frame_2);
        frame->setObjectName(QString::fromUtf8("frame"));
        frame->setFrameShape(QFrame::NoFrame);
        frame->setFrameShadow(QFrame::Plain);
        frame->setLineWidth(0);
        verticalLayout = new QVBoxLayout(frame);
        verticalLayout->setSpacing(0);
        verticalLayout->setMargin(0);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        verticalLayout->setContentsMargins(40, -1, 40, -1);
        gridLayout = new QGridLayout();
        gridLayout->setSpacing(0);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        gridLayout->setVerticalSpacing(0);
        gridLayout->setContentsMargins(9, 9, 9, 0);
        findFriendsCommentLabel = new QLabel(frame);
        findFriendsCommentLabel->setObjectName(QString::fromUtf8("findFriendsCommentLabel"));
        findFriendsCommentLabel->setTextFormat(Qt::PlainText);
        findFriendsCommentLabel->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignTop);
        findFriendsCommentLabel->setWordWrap(true);

        gridLayout->addWidget(findFriendsCommentLabel, 7, 0, 1, 1);

        profileCommentLabel = new QLabel(frame);
        profileCommentLabel->setObjectName(QString::fromUtf8("profileCommentLabel"));
        profileCommentLabel->setTextFormat(Qt::PlainText);
        profileCommentLabel->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignTop);
        profileCommentLabel->setWordWrap(true);

        gridLayout->addWidget(profileCommentLabel, 2, 0, 1, 1);

        verticalSpacer_3 = new QSpacerItem(20, 10, QSizePolicy::Minimum, QSizePolicy::Fixed);

        gridLayout->addItem(verticalSpacer_3, 3, 0, 1, 1);

        horizontalLayout_3 = new QHBoxLayout();
        horizontalLayout_3->setSpacing(0);
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        horizontalLayout_3->setContentsMargins(-1, -1, -1, 0);
        profileButton = new QPushButton(frame);
        profileButton->setObjectName(QString::fromUtf8("profileButton"));
        QFont font;
        font.setFamily(QString::fromUtf8("MS Shell Dlg 2"));
        font.setPointSize(8);
        font.setBold(false);
        font.setItalic(false);
        font.setUnderline(true);
        font.setWeight(50);
        font.setStrikeOut(false);
        profileButton->setFont(font);
        profileButton->setCursor(QCursor(Qt::PointingHandCursor));
        profileButton->setFlat(true);

        horizontalLayout_3->addWidget(profileButton);

        horizontalSpacer_3 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_3);


        gridLayout->addLayout(horizontalLayout_3, 1, 0, 1, 1);

        horizontalLayout_4 = new QHBoxLayout();
        horizontalLayout_4->setSpacing(0);
        horizontalLayout_4->setObjectName(QString::fromUtf8("horizontalLayout_4"));
        horizontalLayout_4->setContentsMargins(-1, -1, -1, 0);
        findFriendsButton = new QPushButton(frame);
        findFriendsButton->setObjectName(QString::fromUtf8("findFriendsButton"));
        findFriendsButton->setFont(font);
        findFriendsButton->setCursor(QCursor(Qt::PointingHandCursor));
        findFriendsButton->setFlat(true);

        horizontalLayout_4->addWidget(findFriendsButton);

        horizontalSpacer_4 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_4->addItem(horizontalSpacer_4);


        gridLayout->addLayout(horizontalLayout_4, 5, 0, 1, 1);


        verticalLayout->addLayout(gridLayout);


        verticalLayout_2->addWidget(frame);

        verticalSpacer_4 = new QSpacerItem(20, 0, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_2->addItem(verticalSpacer_4);

        bottomLinksFrame = new QFrame(frame_2);
        bottomLinksFrame->setObjectName(QString::fromUtf8("bottomLinksFrame"));
        sizePolicy1.setHeightForWidth(bottomLinksFrame->sizePolicy().hasHeightForWidth());
        bottomLinksFrame->setSizePolicy(sizePolicy1);
        bottomLinksFrame->setFrameShape(QFrame::NoFrame);
        bottomLinksFrame->setFrameShadow(QFrame::Plain);
        bottomLinksFrame->setLineWidth(0);
        horizontalLayout_2 = new QHBoxLayout(bottomLinksFrame);
        horizontalLayout_2->setSpacing(30);
        horizontalLayout_2->setMargin(0);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        horizontalLayout_2->setContentsMargins(-1, 0, -1, -1);
        horizontalSpacer_2 = new QSpacerItem(87, 11, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_2);

        finishButton = new QPushButton(bottomLinksFrame);
        finishButton->setObjectName(QString::fromUtf8("finishButton"));
        finishButton->setFont(font);
        finishButton->setCursor(QCursor(Qt::PointingHandCursor));
        finishButton->setDefault(true);
        finishButton->setFlat(true);

        horizontalLayout_2->addWidget(finishButton);

        horizontalSpacer = new QSpacerItem(10, 14, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer);


        verticalLayout_2->addWidget(bottomLinksFrame);

        transBorder_4 = new QFrame(WelcomeWidgetClass);
        transBorder_4->setObjectName(QString::fromUtf8("transBorder_4"));
        transBorder_4->setGeometry(QRect(0, 0, 401, 2));
        transBorder_4->setMinimumSize(QSize(0, 2));
        transBorder_4->setMaximumSize(QSize(16777215, 2));
        transBorder_4->setFrameShape(QFrame::Box);
        transBorder_4->setFrameShadow(QFrame::Plain);
        transBorder = new QFrame(WelcomeWidgetClass);
        transBorder->setObjectName(QString::fromUtf8("transBorder"));
        transBorder->setGeometry(QRect(386, 2, 2, 263));
        transBorder->setMinimumSize(QSize(2, 263));
        transBorder->setMaximumSize(QSize(2, 263));
        transBorder->setFrameShape(QFrame::Box);
        transBorder->setFrameShadow(QFrame::Plain);
        transBorder->setLineWidth(1);
        transBorder_2 = new QFrame(WelcomeWidgetClass);
        transBorder_2->setObjectName(QString::fromUtf8("transBorder_2"));
        transBorder_2->setGeometry(QRect(0, 2, 2, 263));
        transBorder_2->setMinimumSize(QSize(2, 263));
        transBorder_2->setMaximumSize(QSize(2, 263));
        transBorder_2->setFrameShape(QFrame::Box);
        transBorder_2->setFrameShadow(QFrame::Plain);
        transBorder_3 = new QFrame(WelcomeWidgetClass);
        transBorder_3->setObjectName(QString::fromUtf8("transBorder_3"));
        transBorder_3->setGeometry(QRect(0, 265, 401, 2));
        transBorder_3->setMinimumSize(QSize(0, 2));
        transBorder_3->setMaximumSize(QSize(16777215, 2));
        transBorder_3->setFrameShape(QFrame::Box);
        transBorder_3->setFrameShadow(QFrame::Plain);

        retranslateUi(WelcomeWidgetClass);

        QMetaObject::connectSlotsByName(WelcomeWidgetClass);
    } // setupUi

    void retranslateUi(QWidget *WelcomeWidgetClass)
    {
#ifndef QT_NO_ACCESSIBILITY
        titleLabel->setAccessibleName(QApplication::translate("WelcomeWidgetClass", "redLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        titleLabel->setText(QApplication::translate("WelcomeWidgetClass", "Title", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        findFriendsCommentLabel->setAccessibleName(QApplication::translate("WelcomeWidgetClass", "greyLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        findFriendsCommentLabel->setText(QApplication::translate("WelcomeWidgetClass", "FindFriendsText", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        profileCommentLabel->setAccessibleName(QApplication::translate("WelcomeWidgetClass", "greyLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        profileCommentLabel->setText(QApplication::translate("WelcomeWidgetClass", "FillProfileText", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        profileButton->setAccessibleName(QApplication::translate("WelcomeWidgetClass", "blueButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        profileButton->setText(QApplication::translate("WelcomeWidgetClass", "Profile", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        findFriendsButton->setAccessibleName(QApplication::translate("WelcomeWidgetClass", "blueButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        findFriendsButton->setText(QApplication::translate("WelcomeWidgetClass", "FindFriends", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        finishButton->setAccessibleName(QApplication::translate("WelcomeWidgetClass", "blueButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        finishButton->setText(QApplication::translate("WelcomeWidgetClass", "FinishButton", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        transBorder_4->setAccessibleName(QApplication::translate("WelcomeWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder->setAccessibleName(QApplication::translate("WelcomeWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder_2->setAccessibleName(QApplication::translate("WelcomeWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder_3->setAccessibleName(QApplication::translate("WelcomeWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        Q_UNUSED(WelcomeWidgetClass);
    } // retranslateUi

};

namespace Ui {
    class WelcomeWidgetClass: public Ui_WelcomeWidgetClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_WELCOMEWIDGET_H
