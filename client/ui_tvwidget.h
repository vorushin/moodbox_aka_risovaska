/********************************************************************************
** Form generated from reading ui file 'tvwidget.ui'
**
** Created: Wed Jun 24 21:01:11 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_TVWIDGET_H
#define UI_TVWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QFrame>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include "elidedlabel.h"
#include "elidedpushbutton.h"
#include "tvpreviewlabel.h"

QT_BEGIN_NAMESPACE

class Ui_TVWidgetClass
{
public:
    QFrame *tvBarFrame;
    QHBoxLayout *horizontalLayout;
    QVBoxLayout *verticalLayout;
    QLabel *tvIconLabel;
    QSpacerItem *verticalSpacer;
    MoodBox::ElidedLabel *chatLabel;
    QLabel *messageDateLabel;
    MoodBox::ElidedPushButton *userNameButton;
    QLabel *sentLabel;
    QSpacerItem *horizontalSpacer;
    QPushButton *replyRightArrowButton;
    MoodBox::TVPreviewLabel *previewLabel;
    QFrame *tvBarShadow;
    QFrame *transBorder_4;
    QFrame *transBorder;
    QFrame *transBorder_2;
    QFrame *transBorder_3;
    QPushButton *obsceneButton;
    QPushButton *deleteMessageButton;

    void setupUi(QWidget *TVWidgetClass)
    {
        if (TVWidgetClass->objectName().isEmpty())
            TVWidgetClass->setObjectName(QString::fromUtf8("TVWidgetClass"));
        TVWidgetClass->resize(473, 434);
        tvBarFrame = new QFrame(TVWidgetClass);
        tvBarFrame->setObjectName(QString::fromUtf8("tvBarFrame"));
        tvBarFrame->setGeometry(QRect(0, 0, 388, 27));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(tvBarFrame->sizePolicy().hasHeightForWidth());
        tvBarFrame->setSizePolicy(sizePolicy);
        tvBarFrame->setMinimumSize(QSize(0, 27));
        tvBarFrame->setFrameShape(QFrame::NoFrame);
        tvBarFrame->setFrameShadow(QFrame::Plain);
        tvBarFrame->setLineWidth(0);
        horizontalLayout = new QHBoxLayout(tvBarFrame);
        horizontalLayout->setSpacing(8);
        horizontalLayout->setMargin(0);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalLayout->setContentsMargins(10, 0, -1, 0);
        verticalLayout = new QVBoxLayout();
        verticalLayout->setSpacing(0);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        verticalLayout->setContentsMargins(-1, 4, -1, -1);
        tvIconLabel = new QLabel(tvBarFrame);
        tvIconLabel->setObjectName(QString::fromUtf8("tvIconLabel"));
        QSizePolicy sizePolicy1(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(tvIconLabel->sizePolicy().hasHeightForWidth());
        tvIconLabel->setSizePolicy(sizePolicy1);
        tvIconLabel->setMinimumSize(QSize(10, 14));
        tvIconLabel->setFrameShape(QFrame::NoFrame);
        tvIconLabel->setLineWidth(0);
        tvIconLabel->setPixmap(QPixmap(QString::fromUtf8(":/MoodBox/Resources/tv_black.png")));
        tvIconLabel->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignTop);

        verticalLayout->addWidget(tvIconLabel);

        verticalSpacer = new QSpacerItem(5, 3, QSizePolicy::Minimum, QSizePolicy::Preferred);

        verticalLayout->addItem(verticalSpacer);


        horizontalLayout->addLayout(verticalLayout);

        chatLabel = new MoodBox::ElidedLabel(tvBarFrame);
        chatLabel->setObjectName(QString::fromUtf8("chatLabel"));

        horizontalLayout->addWidget(chatLabel);

        messageDateLabel = new QLabel(tvBarFrame);
        messageDateLabel->setObjectName(QString::fromUtf8("messageDateLabel"));

        horizontalLayout->addWidget(messageDateLabel);

        userNameButton = new MoodBox::ElidedPushButton(tvBarFrame);
        userNameButton->setObjectName(QString::fromUtf8("userNameButton"));
        QSizePolicy sizePolicy2(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(userNameButton->sizePolicy().hasHeightForWidth());
        userNameButton->setSizePolicy(sizePolicy2);
        QFont font;
        font.setFamily(QString::fromUtf8("MS Shell Dlg 2"));
        font.setPointSize(8);
        font.setBold(false);
        font.setItalic(false);
        font.setUnderline(true);
        font.setWeight(50);
        font.setStrikeOut(false);
        userNameButton->setFont(font);
        userNameButton->setCursor(QCursor(Qt::PointingHandCursor));
        userNameButton->setFlat(true);

        horizontalLayout->addWidget(userNameButton);

        sentLabel = new QLabel(tvBarFrame);
        sentLabel->setObjectName(QString::fromUtf8("sentLabel"));

        horizontalLayout->addWidget(sentLabel);

        horizontalSpacer = new QSpacerItem(0, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);

        replyRightArrowButton = new QPushButton(tvBarFrame);
        replyRightArrowButton->setObjectName(QString::fromUtf8("replyRightArrowButton"));
        replyRightArrowButton->setMaximumSize(QSize(90, 16777215));
        replyRightArrowButton->setFont(font);
        replyRightArrowButton->setCursor(QCursor(Qt::PointingHandCursor));
        replyRightArrowButton->setLayoutDirection(Qt::RightToLeft);
        QIcon icon;
        icon.addPixmap(QPixmap(QString::fromUtf8(":/MoodBox/Resources/cl_right_arrow_red.png")), QIcon::Normal, QIcon::Off);
        replyRightArrowButton->setIcon(icon);

        horizontalLayout->addWidget(replyRightArrowButton);

        previewLabel = new MoodBox::TVPreviewLabel(TVWidgetClass);
        previewLabel->setObjectName(QString::fromUtf8("previewLabel"));
        previewLabel->setGeometry(QRect(0, 27, 388, 240));
        sizePolicy1.setHeightForWidth(previewLabel->sizePolicy().hasHeightForWidth());
        previewLabel->setSizePolicy(sizePolicy1);
        previewLabel->setMinimumSize(QSize(388, 240));
        previewLabel->setMaximumSize(QSize(388, 240));
        previewLabel->setFrameShape(QFrame::Box);
        previewLabel->setAlignment(Qt::AlignCenter);
        tvBarShadow = new QFrame(TVWidgetClass);
        tvBarShadow->setObjectName(QString::fromUtf8("tvBarShadow"));
        tvBarShadow->setGeometry(QRect(0, 27, 388, 4));
        tvBarShadow->setMinimumSize(QSize(388, 4));
        tvBarShadow->setMaximumSize(QSize(388, 4));
        tvBarShadow->setFrameShape(QFrame::Box);
        tvBarShadow->setFrameShadow(QFrame::Plain);
        transBorder_4 = new QFrame(TVWidgetClass);
        transBorder_4->setObjectName(QString::fromUtf8("transBorder_4"));
        transBorder_4->setGeometry(QRect(0, 0, 401, 2));
        transBorder_4->setMinimumSize(QSize(0, 2));
        transBorder_4->setMaximumSize(QSize(16777215, 2));
        transBorder_4->setFrameShape(QFrame::Box);
        transBorder_4->setFrameShadow(QFrame::Plain);
        transBorder = new QFrame(TVWidgetClass);
        transBorder->setObjectName(QString::fromUtf8("transBorder"));
        transBorder->setGeometry(QRect(386, 2, 2, 263));
        transBorder->setMinimumSize(QSize(2, 263));
        transBorder->setMaximumSize(QSize(2, 263));
        transBorder->setFrameShape(QFrame::Box);
        transBorder->setFrameShadow(QFrame::Plain);
        transBorder->setLineWidth(1);
        transBorder_2 = new QFrame(TVWidgetClass);
        transBorder_2->setObjectName(QString::fromUtf8("transBorder_2"));
        transBorder_2->setGeometry(QRect(0, 2, 2, 263));
        transBorder_2->setMinimumSize(QSize(2, 263));
        transBorder_2->setMaximumSize(QSize(2, 263));
        transBorder_2->setFrameShape(QFrame::Box);
        transBorder_2->setFrameShadow(QFrame::Plain);
        transBorder_3 = new QFrame(TVWidgetClass);
        transBorder_3->setObjectName(QString::fromUtf8("transBorder_3"));
        transBorder_3->setGeometry(QRect(0, 265, 401, 2));
        transBorder_3->setMinimumSize(QSize(0, 2));
        transBorder_3->setMaximumSize(QSize(16777215, 2));
        transBorder_3->setFrameShape(QFrame::Box);
        transBorder_3->setFrameShadow(QFrame::Plain);
        obsceneButton = new QPushButton(TVWidgetClass);
        obsceneButton->setObjectName(QString::fromUtf8("obsceneButton"));
        obsceneButton->setGeometry(QRect(340, 165, 40, 40));
        sizePolicy1.setHeightForWidth(obsceneButton->sizePolicy().hasHeightForWidth());
        obsceneButton->setSizePolicy(sizePolicy1);
        deleteMessageButton = new QPushButton(TVWidgetClass);
        deleteMessageButton->setObjectName(QString::fromUtf8("deleteMessageButton"));
        deleteMessageButton->setGeometry(QRect(340, 91, 40, 40));
        sizePolicy1.setHeightForWidth(deleteMessageButton->sizePolicy().hasHeightForWidth());
        deleteMessageButton->setSizePolicy(sizePolicy1);

        retranslateUi(TVWidgetClass);

        QMetaObject::connectSlotsByName(TVWidgetClass);
    } // setupUi

    void retranslateUi(QWidget *TVWidgetClass)
    {
        tvIconLabel->setText(QString());
#ifndef QT_NO_ACCESSIBILITY
        messageDateLabel->setAccessibleName(QApplication::translate("TVWidgetClass", "barGrayLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        sentLabel->setAccessibleName(QApplication::translate("TVWidgetClass", "barGrayLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        replyRightArrowButton->setAccessibleName(QApplication::translate("TVWidgetClass", "redButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        replyRightArrowButton->setText(QApplication::translate("TVWidgetClass", "Reply", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        tvBarShadow->setAccessibleName(QApplication::translate("TVWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder_4->setAccessibleName(QApplication::translate("TVWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder->setAccessibleName(QApplication::translate("TVWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder_2->setAccessibleName(QApplication::translate("TVWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder_3->setAccessibleName(QApplication::translate("TVWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_TOOLTIP
        obsceneButton->setToolTip(QApplication::translate("TVWidgetClass", "ObsceneButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        obsceneButton->setAccessibleName(QApplication::translate("TVWidgetClass", "obscenePushButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_TOOLTIP
        deleteMessageButton->setToolTip(QApplication::translate("TVWidgetClass", "DeleteMessageButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        deleteMessageButton->setAccessibleName(QApplication::translate("TVWidgetClass", "deleteMessagePushButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        Q_UNUSED(TVWidgetClass);
    } // retranslateUi

};

namespace Ui {
    class TVWidgetClass: public Ui_TVWidgetClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_TVWIDGET_H
