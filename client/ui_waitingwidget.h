/********************************************************************************
** Form generated from reading ui file 'waitingwidget.ui'
**
** Created: Tue May 5 10:34:56 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_WAITINGWIDGET_H
#define UI_WAITINGWIDGET_H

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

QT_BEGIN_NAMESPACE

class Ui_WaitingWidgetClass
{
public:
    QFrame *frame_2;
    QVBoxLayout *verticalLayout_2;
    QFrame *frame;
    QVBoxLayout *verticalLayout_3;
    QSpacerItem *verticalSpacer_4;
    QLabel *waitingLabel;
    QSpacerItem *verticalSpacer_5;
    QFrame *bottomLinksFrame;
    QHBoxLayout *horizontalLayout_2;
    QSpacerItem *horizontalSpacer_2;
    QPushButton *cancelButton;
    QSpacerItem *horizontalSpacer;
    QFrame *transBorder_4;
    QFrame *transBorder;
    QFrame *transBorder_2;
    QFrame *transBorder_3;

    void setupUi(QWidget *WaitingWidgetClass)
    {
        if (WaitingWidgetClass->objectName().isEmpty())
            WaitingWidgetClass->setObjectName(QString::fromUtf8("WaitingWidgetClass"));
        WaitingWidgetClass->setWindowModality(Qt::NonModal);
        WaitingWidgetClass->resize(388, 267);
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(WaitingWidgetClass->sizePolicy().hasHeightForWidth());
        WaitingWidgetClass->setSizePolicy(sizePolicy);
        frame_2 = new QFrame(WaitingWidgetClass);
        frame_2->setObjectName(QString::fromUtf8("frame_2"));
        frame_2->setGeometry(QRect(9, 9, 370, 249));
        frame_2->setFrameShape(QFrame::NoFrame);
        frame_2->setFrameShadow(QFrame::Plain);
        frame_2->setLineWidth(0);
        verticalLayout_2 = new QVBoxLayout(frame_2);
        verticalLayout_2->setSpacing(6);
        verticalLayout_2->setMargin(0);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        frame = new QFrame(frame_2);
        frame->setObjectName(QString::fromUtf8("frame"));
        frame->setFrameShape(QFrame::NoFrame);
        frame->setFrameShadow(QFrame::Plain);
        frame->setLineWidth(0);
        verticalLayout_3 = new QVBoxLayout(frame);
        verticalLayout_3->setSpacing(10);
        verticalLayout_3->setMargin(11);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        verticalLayout_3->setContentsMargins(40, -1, 40, -1);
        verticalSpacer_4 = new QSpacerItem(20, 10, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_3->addItem(verticalSpacer_4);

        waitingLabel = new QLabel(frame);
        waitingLabel->setObjectName(QString::fromUtf8("waitingLabel"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(waitingLabel->sizePolicy().hasHeightForWidth());
        waitingLabel->setSizePolicy(sizePolicy1);
        waitingLabel->setPixmap(QPixmap(QString::fromUtf8(":/MoodBox/Resources/waiting_circle_1.png")));
        waitingLabel->setAlignment(Qt::AlignCenter|Qt::AlignHCenter|Qt::AlignLeading|Qt::AlignLeft|Qt::AlignVCenter);

        verticalLayout_3->addWidget(waitingLabel);

        verticalSpacer_5 = new QSpacerItem(20, 10, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_3->addItem(verticalSpacer_5);


        verticalLayout_2->addWidget(frame);

        bottomLinksFrame = new QFrame(frame_2);
        bottomLinksFrame->setObjectName(QString::fromUtf8("bottomLinksFrame"));
        sizePolicy1.setHeightForWidth(bottomLinksFrame->sizePolicy().hasHeightForWidth());
        bottomLinksFrame->setSizePolicy(sizePolicy1);
        bottomLinksFrame->setMaximumSize(QSize(16777215, 32));
        bottomLinksFrame->setFrameShape(QFrame::NoFrame);
        bottomLinksFrame->setFrameShadow(QFrame::Plain);
        bottomLinksFrame->setLineWidth(0);
        horizontalLayout_2 = new QHBoxLayout(bottomLinksFrame);
        horizontalLayout_2->setSpacing(30);
        horizontalLayout_2->setMargin(11);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        horizontalLayout_2->setContentsMargins(-1, 0, -1, -1);
        horizontalSpacer_2 = new QSpacerItem(10, 14, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_2);

        cancelButton = new QPushButton(bottomLinksFrame);
        cancelButton->setObjectName(QString::fromUtf8("cancelButton"));
        QFont font;
        font.setFamily(QString::fromUtf8("MS Shell Dlg 2"));
        font.setPointSize(8);
        font.setBold(false);
        font.setItalic(false);
        font.setUnderline(true);
        font.setWeight(50);
        font.setStrikeOut(false);
        cancelButton->setFont(font);
        cancelButton->setCursor(QCursor(Qt::PointingHandCursor));
        cancelButton->setFlat(true);

        horizontalLayout_2->addWidget(cancelButton);

        horizontalSpacer = new QSpacerItem(10, 14, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer);


        verticalLayout_2->addWidget(bottomLinksFrame);

        transBorder_4 = new QFrame(WaitingWidgetClass);
        transBorder_4->setObjectName(QString::fromUtf8("transBorder_4"));
        transBorder_4->setGeometry(QRect(0, 0, 401, 2));
        transBorder_4->setMinimumSize(QSize(0, 2));
        transBorder_4->setMaximumSize(QSize(16777215, 2));
        transBorder_4->setFrameShape(QFrame::Box);
        transBorder_4->setFrameShadow(QFrame::Plain);
        transBorder = new QFrame(WaitingWidgetClass);
        transBorder->setObjectName(QString::fromUtf8("transBorder"));
        transBorder->setGeometry(QRect(386, 2, 2, 263));
        transBorder->setMinimumSize(QSize(2, 263));
        transBorder->setMaximumSize(QSize(2, 263));
        transBorder->setFrameShape(QFrame::Box);
        transBorder->setFrameShadow(QFrame::Plain);
        transBorder->setLineWidth(1);
        transBorder_2 = new QFrame(WaitingWidgetClass);
        transBorder_2->setObjectName(QString::fromUtf8("transBorder_2"));
        transBorder_2->setGeometry(QRect(0, 2, 2, 263));
        transBorder_2->setMinimumSize(QSize(2, 263));
        transBorder_2->setMaximumSize(QSize(2, 263));
        transBorder_2->setFrameShape(QFrame::Box);
        transBorder_2->setFrameShadow(QFrame::Plain);
        transBorder_3 = new QFrame(WaitingWidgetClass);
        transBorder_3->setObjectName(QString::fromUtf8("transBorder_3"));
        transBorder_3->setGeometry(QRect(0, 265, 401, 2));
        transBorder_3->setMinimumSize(QSize(0, 2));
        transBorder_3->setMaximumSize(QSize(16777215, 2));
        transBorder_3->setFrameShape(QFrame::Box);
        transBorder_3->setFrameShadow(QFrame::Plain);

        retranslateUi(WaitingWidgetClass);

        QMetaObject::connectSlotsByName(WaitingWidgetClass);
    } // setupUi

    void retranslateUi(QWidget *WaitingWidgetClass)
    {
#ifndef QT_NO_ACCESSIBILITY
        waitingLabel->setAccessibleName(QApplication::translate("WaitingWidgetClass", "whiteLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        cancelButton->setAccessibleName(QApplication::translate("WaitingWidgetClass", "redButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        cancelButton->setText(QApplication::translate("WaitingWidgetClass", "Cancel", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        transBorder_4->setAccessibleName(QApplication::translate("WaitingWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder->setAccessibleName(QApplication::translate("WaitingWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder_2->setAccessibleName(QApplication::translate("WaitingWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder_3->setAccessibleName(QApplication::translate("WaitingWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        Q_UNUSED(WaitingWidgetClass);
    } // retranslateUi

};

namespace Ui {
    class WaitingWidgetClass: public Ui_WaitingWidgetClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_WAITINGWIDGET_H
