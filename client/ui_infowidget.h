/********************************************************************************
** Form generated from reading ui file 'infowidget.ui'
**
** Created: Tue May 5 10:34:44 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_INFOWIDGET_H
#define UI_INFOWIDGET_H

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

class Ui_InfoWidgetClass
{
public:
    QFrame *frame_2;
    QVBoxLayout *verticalLayout_2;
    QFrame *topTitleFrame;
    QHBoxLayout *horizontalLayout;
    QLabel *titleLabel;
    QSpacerItem *verticalSpacer_4;
    QFrame *frame;
    QVBoxLayout *verticalLayout_3;
    QLabel *desriptionLabel;
    QSpacerItem *verticalSpacer_5;
    QFrame *bottomLinksFrame;
    QHBoxLayout *horizontalLayout_2;
    QSpacerItem *horizontalSpacer_2;
    QPushButton *finishButton;
    QSpacerItem *horizontalSpacer;
    QFrame *transBorder_4;
    QFrame *transBorder;
    QFrame *transBorder_2;
    QFrame *transBorder_3;

    void setupUi(QWidget *InfoWidgetClass)
    {
        if (InfoWidgetClass->objectName().isEmpty())
            InfoWidgetClass->setObjectName(QString::fromUtf8("InfoWidgetClass"));
        InfoWidgetClass->resize(388, 267);
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(InfoWidgetClass->sizePolicy().hasHeightForWidth());
        InfoWidgetClass->setSizePolicy(sizePolicy);
        frame_2 = new QFrame(InfoWidgetClass);
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

        verticalSpacer_4 = new QSpacerItem(20, 10, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_2->addItem(verticalSpacer_4);

        frame = new QFrame(frame_2);
        frame->setObjectName(QString::fromUtf8("frame"));
        frame->setFrameShape(QFrame::NoFrame);
        frame->setFrameShadow(QFrame::Plain);
        frame->setLineWidth(0);
        verticalLayout_3 = new QVBoxLayout(frame);
        verticalLayout_3->setSpacing(0);
        verticalLayout_3->setMargin(0);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        verticalLayout_3->setContentsMargins(20, 0, 20, 0);
        desriptionLabel = new QLabel(frame);
        desriptionLabel->setObjectName(QString::fromUtf8("desriptionLabel"));
        sizePolicy1.setHeightForWidth(desriptionLabel->sizePolicy().hasHeightForWidth());
        desriptionLabel->setSizePolicy(sizePolicy1);
        desriptionLabel->setAlignment(Qt::AlignCenter);
        desriptionLabel->setWordWrap(true);

        verticalLayout_3->addWidget(desriptionLabel);


        verticalLayout_2->addWidget(frame);

        verticalSpacer_5 = new QSpacerItem(20, 10, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_2->addItem(verticalSpacer_5);

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
        QFont font;
        font.setFamily(QString::fromUtf8("MS Shell Dlg 2"));
        font.setPointSize(8);
        font.setBold(false);
        font.setItalic(false);
        font.setUnderline(true);
        font.setWeight(50);
        font.setStrikeOut(false);
        finishButton->setFont(font);
        finishButton->setCursor(QCursor(Qt::PointingHandCursor));
        finishButton->setFlat(true);

        horizontalLayout_2->addWidget(finishButton);

        horizontalSpacer = new QSpacerItem(10, 14, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer);


        verticalLayout_2->addWidget(bottomLinksFrame);

        transBorder_4 = new QFrame(InfoWidgetClass);
        transBorder_4->setObjectName(QString::fromUtf8("transBorder_4"));
        transBorder_4->setGeometry(QRect(0, 0, 401, 2));
        transBorder_4->setMinimumSize(QSize(0, 2));
        transBorder_4->setMaximumSize(QSize(16777215, 2));
        transBorder_4->setFrameShape(QFrame::Box);
        transBorder_4->setFrameShadow(QFrame::Plain);
        transBorder = new QFrame(InfoWidgetClass);
        transBorder->setObjectName(QString::fromUtf8("transBorder"));
        transBorder->setGeometry(QRect(386, 2, 2, 263));
        transBorder->setMinimumSize(QSize(2, 263));
        transBorder->setMaximumSize(QSize(2, 263));
        transBorder->setFrameShape(QFrame::Box);
        transBorder->setFrameShadow(QFrame::Plain);
        transBorder->setLineWidth(1);
        transBorder_2 = new QFrame(InfoWidgetClass);
        transBorder_2->setObjectName(QString::fromUtf8("transBorder_2"));
        transBorder_2->setGeometry(QRect(0, 2, 2, 263));
        transBorder_2->setMinimumSize(QSize(2, 263));
        transBorder_2->setMaximumSize(QSize(2, 263));
        transBorder_2->setFrameShape(QFrame::Box);
        transBorder_2->setFrameShadow(QFrame::Plain);
        transBorder_3 = new QFrame(InfoWidgetClass);
        transBorder_3->setObjectName(QString::fromUtf8("transBorder_3"));
        transBorder_3->setGeometry(QRect(0, 265, 401, 2));
        transBorder_3->setMinimumSize(QSize(0, 2));
        transBorder_3->setMaximumSize(QSize(16777215, 2));
        transBorder_3->setFrameShape(QFrame::Box);
        transBorder_3->setFrameShadow(QFrame::Plain);

        retranslateUi(InfoWidgetClass);

        QMetaObject::connectSlotsByName(InfoWidgetClass);
    } // setupUi

    void retranslateUi(QWidget *InfoWidgetClass)
    {
#ifndef QT_NO_ACCESSIBILITY
        titleLabel->setAccessibleName(QApplication::translate("InfoWidgetClass", "redLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        titleLabel->setText(QApplication::translate("InfoWidgetClass", "Title_DT", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        desriptionLabel->setAccessibleName(QApplication::translate("InfoWidgetClass", "whiteLabel", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        desriptionLabel->setText(QApplication::translate("InfoWidgetClass", "Description_DT", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        finishButton->setAccessibleName(QApplication::translate("InfoWidgetClass", "blueButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        finishButton->setText(QApplication::translate("InfoWidgetClass", "Finish", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        transBorder_4->setAccessibleName(QApplication::translate("InfoWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder->setAccessibleName(QApplication::translate("InfoWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder_2->setAccessibleName(QApplication::translate("InfoWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        transBorder_3->setAccessibleName(QApplication::translate("InfoWidgetClass", "transBorder", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        Q_UNUSED(InfoWidgetClass);
    } // retranslateUi

};

namespace Ui {
    class InfoWidgetClass: public Ui_InfoWidgetClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_INFOWIDGET_H
