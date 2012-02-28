/********************************************************************************
** Form generated from reading ui file 'invitecodewidget.ui'
**
** Created: Tue May 5 10:34:44 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_INVITECODEWIDGET_H
#define UI_INVITECODEWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QFrame>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_InviteCodeWidgetClass
{
public:
    QFrame *frame_;
    QVBoxLayout *verticalLayout_2;
    QFrame *topTitleFrame;
    QHBoxLayout *horizontalLayout;
    QLabel *titleLabel;
    QSpacerItem *verticalSpacer_4;
    QFrame *frame;
    QVBoxLayout *verticalLayout_3;
    QLabel *inviteCodeLabel;
    QLineEdit *inviteCodeEdit;
    QSpacerItem *verticalSpacer_5;
    QFrame *bottomLinksFrame;
    QHBoxLayout *horizontalLayout_2;
    QSpacerItem *horizontalSpacer_2;
    QPushButton *backButton;
    QPushButton *nextButton;
    QSpacerItem *horizontalSpacer;
    QFrame *transBorder_2;
    QFrame *transBorder;
    QFrame *transBorder_3;
    QFrame *transBorder_4;

    void setupUi(QWidget *InviteCodeWidgetClass)
    {
        if (InviteCodeWidgetClass->objectName().isEmpty())
            InviteCodeWidgetClass->setObjectName(QString::fromUtf8("InviteCodeWidgetClass"));
        InviteCodeWidgetClass->resize(388, 267);
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(InviteCodeWidgetClass->sizePolicy().hasHeightForWidth());
        InviteCodeWidgetClass->setSizePolicy(sizePolicy);
        frame_ = new QFrame(InviteCodeWidgetClass);
        frame_->setObjectName(QString::fromUtf8("frame_"));
        frame_->setGeometry(QRect(9, 9, 370, 249));
        frame_->setFrameShape(QFrame::NoFrame);
        frame_->setFrameShadow(QFrame::Plain);
        frame_->setLineWidth(0);
        verticalLayout_2 = new QVBoxLayout(frame_);
        verticalLayout_2->setSpacing(6);
        verticalLayout_2->setMargin(0);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        topTitleFrame = new QFrame(frame_);
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

        verticalSpacer_4 = new QSpacerItem(20, 10, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_2->addItem(verticalSpacer_4);

        frame = new QFrame(frame_);
        frame->setObjectName(QString::fromUtf8("frame"));
        QSizePolicy sizePolicy2(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(frame->sizePolicy().hasHeightForWidth());
        frame->setSizePolicy(sizePolicy2);
        frame->setFrameShape(QFrame::NoFrame);
        frame->setFrameShadow(QFrame::Plain);
        frame->setLineWidth(0);
        verticalLayout_3 = new QVBoxLayout(frame);
        verticalLayout_3->setSpacing(0);
        verticalLayout_3->setMargin(0);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        verticalLayout_3->setContentsMargins(40, 0, 40, 0);
        inviteCodeLabel = new QLabel(frame);
        inviteCodeLabel->setObjectName(QString::fromUtf8("inviteCodeLabel"));
        sizePolicy1.setHeightForWidth(inviteCodeLabel->sizePolicy().hasHeightForWidth());
        inviteCodeLabel->setSizePolicy(sizePolicy1);
        inviteCodeLabel->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignVCenter);

        verticalLayout_3->addWidget(inviteCodeLabel);

        inviteCodeEdit = new QLineEdit(frame);
        inviteCodeEdit->setObjectName(QString::fromUtf8("inviteCodeEdit"));

        verticalLayout_3->addWidget(inviteCodeEdit);


        verticalLayout_2->addWidget(frame);

        verticalSpacer_5 = new QSpacerItem(20, 10, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_2->addItem(verticalSpacer_5);

        bottomLinksFrame = new QFrame(frame_);
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
        horizontalSpacer_2 = new QSpacerItem(20, 11, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_2);

        backButton = new QPushButton(bottomLinksFrame);
        backButton->setObjectName(QString::fromUtf8("backButton"));
        QFont font;
        font.setFamily(QString::fromUtf8("MS Shell Dlg 2"));
        font.setPointSize(8);
        font.setBold(false);
        font.setItalic(false);
        font.setUnderline(true);
        font.setWeight(50);
        font.setStrikeOut(false);
        backButton->setFont(font);
        backButton->setCursor(QCursor(Qt::PointingHandCursor));
        backButton->setFlat(true);

        horizontalLayout_2->addWidget(backButton);

        nextButton = new QPushButton(bottomLinksFrame);
        nextButton->setObjectName(QString::fromUtf8("nextButton"));
        nextButton->setFont(font);
        nextButton->setCursor(QCursor(Qt::PointingHandCursor));
        nextButton->setDefault(true);
        nextButton->setFlat(true);

        horizontalLayout_2->addWidget(nextButton);

        horizontalSpacer = new QSpacerItem(20, 14, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer);


        verticalLayout_2->addWidget(bottomLinksFrame);

        topTitleFrame->raise();
        bottomLinksFrame->raise();
        frame->raise();
        transBorder_2 = new QFrame(InviteCodeWidgetClass);
        transBorder_2->setObjectName(QString::fromUtf8("transBorder_2"));
        transBorder_2->setGeometry(QRect(0, 2, 2, 263));
        transBorder_2->setMinimumSize(QSize(2, 263));
        transBorder_2->setMaximumSize(QSize(2, 263));
        transBorder_2->setFrameShape(QFrame::Box);
        transBorder_2->setFrameShadow(QFrame::Plain);
        transBorder = new QFrame(InviteCodeWidgetClass);
        transBorder->setObjectName(QString::fromUtf8("transBorder"));
        transBorder->setGeometry(QRect(386, 2, 2, 263));
        transBorder->setMinimumSize(QSize(2, 263));
        transBorder->setMaximumSize(QSize(2, 263));
        transBorder->setFrameShape(QFrame::Box);
        transBorder->setFrameShadow(QFrame::Plain);
        transBorder->setLineWidth(1);
        transBorder_3 = new QFrame(InviteCodeWidgetClass);
        transBorder_3->setObjectName(QString::fromUtf8("transBorder_3"));
        transBorder_3->setGeometry(QRect(0, 265, 401, 2));
        transBorder_3->setMinimumSize(QSize(0, 2));
        transBorder_3->setMaximumSize(QSize(16777215, 2));
        transBorder_3->setFrameShape(QFrame::Box);
        transBorder_3->setFrameShadow(QFrame::Plain);
        transBorder_4 = new QFrame(InviteCodeWidgetClass);
        transBorder_4->setObjectName(QString::fromUtf8("transBorder_4"));
        transBorder_4->setGeometry(QRect(0, 0, 401, 2));
        transBorder_4->setMinimumSize(QSize(0, 2));
        transBorder_4->setMaximumSize(QSize(16777215, 2));
        transBorder_4->setFrameShape(QFrame::Box);
        transBorder_4->setFrameShadow(QFrame::Plain);
#ifndef QT_NO_SHORTCUT
        inviteCodeLabel->setBuddy(inviteCodeEdit);
#endif // QT_NO_SHORTCUT

        retranslateUi(InviteCodeWidgetClass);

        QMetaObject::connectSlotsByName(InviteCodeWidgetClass);
    } // setupUi

    void retranslateUi(QWidget *InviteCodeWidgetClass)
    {
#ifndef QT_NO_ACCESSIBILITY
        titleLabel->setAccessibleName(QApplication::translate("InviteCodeWidgetClass", "redLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        titleLabel->setText(QApplication::translate("InviteCodeWidgetClass", "Title", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        inviteCodeLabel->setAccessibleName(QApplication::translate("InviteCodeWidgetClass", "whiteLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        inviteCodeLabel->setText(QApplication::translate("InviteCodeWidgetClass", "EnterYourInviteCode", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        backButton->setAccessibleName(QApplication::translate("InviteCodeWidgetClass", "blueButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        backButton->setText(QApplication::translate("InviteCodeWidgetClass", "Back", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        nextButton->setAccessibleName(QApplication::translate("InviteCodeWidgetClass", "blueButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        nextButton->setText(QApplication::translate("InviteCodeWidgetClass", "Next", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        transBorder_2->setAccessibleName(QApplication::translate("InviteCodeWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder->setAccessibleName(QApplication::translate("InviteCodeWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder_3->setAccessibleName(QApplication::translate("InviteCodeWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder_4->setAccessibleName(QApplication::translate("InviteCodeWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        Q_UNUSED(InviteCodeWidgetClass);
    } // retranslateUi

};

namespace Ui {
    class InviteCodeWidgetClass: public Ui_InviteCodeWidgetClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_INVITECODEWIDGET_H
