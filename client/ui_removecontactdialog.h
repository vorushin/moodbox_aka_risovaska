/********************************************************************************
** Form generated from reading ui file 'removecontactdialog.ui'
**
** Created: Tue May 5 10:34:51 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_REMOVECONTACTDIALOG_H
#define UI_REMOVECONTACTDIALOG_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialog>
#include <QtGui/QFrame>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_RemoveContactDialogClass
{
public:
    QVBoxLayout *verticalLayout_3;
    QFrame *styledFrame;
    QVBoxLayout *verticalLayout;
    QHBoxLayout *horizontalLayout_3;
    QSpacerItem *horizontalSpacer_4;
    QToolButton *closeToolButton;
    QFrame *mainDialogFrame;
    QVBoxLayout *verticalLayout_2;
    QSpacerItem *verticalSpacer;
    QLabel *removeLabel;
    QSpacerItem *verticalSpacer_2;
    QHBoxLayout *horizontalLayout;
    QSpacerItem *horizontalSpacer;
    QPushButton *removeButton;
    QPushButton *cancelButton;
    QSpacerItem *horizontalSpacer_2;

    void setupUi(QDialog *RemoveContactDialogClass)
    {
        if (RemoveContactDialogClass->objectName().isEmpty())
            RemoveContactDialogClass->setObjectName(QString::fromUtf8("RemoveContactDialogClass"));
        RemoveContactDialogClass->setWindowModality(Qt::WindowModal);
        RemoveContactDialogClass->resize(323, 172);
        verticalLayout_3 = new QVBoxLayout(RemoveContactDialogClass);
        verticalLayout_3->setSpacing(6);
        verticalLayout_3->setMargin(0);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        styledFrame = new QFrame(RemoveContactDialogClass);
        styledFrame->setObjectName(QString::fromUtf8("styledFrame"));
        styledFrame->setFrameShape(QFrame::NoFrame);
        styledFrame->setFrameShadow(QFrame::Plain);
        verticalLayout = new QVBoxLayout(styledFrame);
        verticalLayout->setSpacing(5);
        verticalLayout->setMargin(11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        verticalLayout->setContentsMargins(1, 0, 1, 0);
        horizontalLayout_3 = new QHBoxLayout();
        horizontalLayout_3->setSpacing(6);
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        horizontalLayout_3->setContentsMargins(-1, 5, 4, 9);
        horizontalSpacer_4 = new QSpacerItem(10, 5, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_4);

        closeToolButton = new QToolButton(styledFrame);
        closeToolButton->setObjectName(QString::fromUtf8("closeToolButton"));
        closeToolButton->setFocusPolicy(Qt::NoFocus);
        closeToolButton->setLayoutDirection(Qt::LeftToRight);
        closeToolButton->setPopupMode(QToolButton::DelayedPopup);

        horizontalLayout_3->addWidget(closeToolButton);


        verticalLayout->addLayout(horizontalLayout_3);

        mainDialogFrame = new QFrame(styledFrame);
        mainDialogFrame->setObjectName(QString::fromUtf8("mainDialogFrame"));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Expanding);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mainDialogFrame->sizePolicy().hasHeightForWidth());
        mainDialogFrame->setSizePolicy(sizePolicy);
        mainDialogFrame->setFrameShape(QFrame::NoFrame);
        mainDialogFrame->setFrameShadow(QFrame::Plain);
        mainDialogFrame->setLineWidth(0);
        verticalLayout_2 = new QVBoxLayout(mainDialogFrame);
        verticalLayout_2->setSpacing(4);
        verticalLayout_2->setMargin(0);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        verticalSpacer = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_2->addItem(verticalSpacer);

        removeLabel = new QLabel(mainDialogFrame);
        removeLabel->setObjectName(QString::fromUtf8("removeLabel"));
        removeLabel->setAlignment(Qt::AlignCenter|Qt::AlignHCenter|Qt::AlignLeading|Qt::AlignLeft|Qt::AlignVCenter);
        removeLabel->setMargin(9);

        verticalLayout_2->addWidget(removeLabel);

        verticalSpacer_2 = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_2->addItem(verticalSpacer_2);


        verticalLayout->addWidget(mainDialogFrame);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalLayout->setContentsMargins(-1, 4, -1, 9);
        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);

        removeButton = new QPushButton(styledFrame);
        removeButton->setObjectName(QString::fromUtf8("removeButton"));

        horizontalLayout->addWidget(removeButton);

        cancelButton = new QPushButton(styledFrame);
        cancelButton->setObjectName(QString::fromUtf8("cancelButton"));

        horizontalLayout->addWidget(cancelButton);

        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer_2);


        verticalLayout->addLayout(horizontalLayout);


        verticalLayout_3->addWidget(styledFrame);


        retranslateUi(RemoveContactDialogClass);

        QMetaObject::connectSlotsByName(RemoveContactDialogClass);
    } // setupUi

    void retranslateUi(QDialog *RemoveContactDialogClass)
    {
#ifndef QT_NO_TOOLTIP
        closeToolButton->setToolTip(QApplication::translate("RemoveContactDialogClass", "CloseButtonHint", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_ACCESSIBILITY
        closeToolButton->setAccessibleName(QApplication::translate("RemoveContactDialogClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        removeLabel->setText(QApplication::translate("RemoveContactDialogClass", "RemoveLabel_DT", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        removeButton->setAccessibleName(QApplication::translate("RemoveContactDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        removeButton->setText(QApplication::translate("RemoveContactDialogClass", "RemoveButton", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        cancelButton->setAccessibleName(QApplication::translate("RemoveContactDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        cancelButton->setText(QApplication::translate("RemoveContactDialogClass", "CancelButton", 0, QApplication::UnicodeUTF8));
        Q_UNUSED(RemoveContactDialogClass);
    } // retranslateUi

};

namespace Ui {
    class RemoveContactDialogClass: public Ui_RemoveContactDialogClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_REMOVECONTACTDIALOG_H
