/********************************************************************************
** Form generated from reading ui file 'selectavatardialog.ui'
**
** Created: Tue May 5 10:34:52 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_SELECTAVATARDIALOG_H
#define UI_SELECTAVATARDIALOG_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialog>
#include <QtGui/QFrame>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QListWidget>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_SelectAvatarDialogClass
{
public:
    QVBoxLayout *verticalLayout_2;
    QFrame *styledFrame;
    QVBoxLayout *frameVLayout;
    QHBoxLayout *horizontalLayout_3;
    QSpacerItem *horizontalSpacer_4;
    QToolButton *closeToolButton;
    QFrame *mainDialogFrame;
    QVBoxLayout *verticalLayout_3;
    QHBoxLayout *horizontalLayout_2;
    QLabel *topMessageLabel;
    QSpacerItem *horizontalSpacer_2;
    QPushButton *addAvatarButton;
    QListWidget *avatarListWidget;
    QFrame *frameBottom;
    QHBoxLayout *horizontalLayout_4;
    QSpacerItem *horizontalSpacer;
    QPushButton *okButton;
    QPushButton *cancelButton;

    void setupUi(QDialog *SelectAvatarDialogClass)
    {
        if (SelectAvatarDialogClass->objectName().isEmpty())
            SelectAvatarDialogClass->setObjectName(QString::fromUtf8("SelectAvatarDialogClass"));
        SelectAvatarDialogClass->setWindowModality(Qt::WindowModal);
        SelectAvatarDialogClass->resize(421, 412);
        verticalLayout_2 = new QVBoxLayout(SelectAvatarDialogClass);
        verticalLayout_2->setSpacing(0);
        verticalLayout_2->setMargin(0);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        verticalLayout_2->setSizeConstraint(QLayout::SetDefaultConstraint);
        styledFrame = new QFrame(SelectAvatarDialogClass);
        styledFrame->setObjectName(QString::fromUtf8("styledFrame"));
        styledFrame->setFrameShape(QFrame::NoFrame);
        styledFrame->setFrameShadow(QFrame::Plain);
        styledFrame->setLineWidth(0);
        frameVLayout = new QVBoxLayout(styledFrame);
        frameVLayout->setSpacing(0);
        frameVLayout->setMargin(0);
        frameVLayout->setObjectName(QString::fromUtf8("frameVLayout"));
        frameVLayout->setContentsMargins(1, 0, 1, 0);
        horizontalLayout_3 = new QHBoxLayout();
        horizontalLayout_3->setSpacing(0);
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        horizontalLayout_3->setContentsMargins(-1, 5, 5, 16);
        horizontalSpacer_4 = new QSpacerItem(10, 5, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_4);

        closeToolButton = new QToolButton(styledFrame);
        closeToolButton->setObjectName(QString::fromUtf8("closeToolButton"));
        closeToolButton->setFocusPolicy(Qt::NoFocus);
        closeToolButton->setLayoutDirection(Qt::LeftToRight);
        closeToolButton->setPopupMode(QToolButton::DelayedPopup);

        horizontalLayout_3->addWidget(closeToolButton);


        frameVLayout->addLayout(horizontalLayout_3);

        mainDialogFrame = new QFrame(styledFrame);
        mainDialogFrame->setObjectName(QString::fromUtf8("mainDialogFrame"));
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mainDialogFrame->sizePolicy().hasHeightForWidth());
        mainDialogFrame->setSizePolicy(sizePolicy);
        mainDialogFrame->setFrameShape(QFrame::NoFrame);
        mainDialogFrame->setFrameShadow(QFrame::Plain);
        verticalLayout_3 = new QVBoxLayout(mainDialogFrame);
        verticalLayout_3->setSpacing(0);
        verticalLayout_3->setMargin(0);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        verticalLayout_3->setContentsMargins(12, -1, 12, -1);
        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setSpacing(0);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        horizontalLayout_2->setContentsMargins(-1, 2, -1, 11);
        topMessageLabel = new QLabel(mainDialogFrame);
        topMessageLabel->setObjectName(QString::fromUtf8("topMessageLabel"));
        topMessageLabel->setWordWrap(true);

        horizontalLayout_2->addWidget(topMessageLabel);

        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_2);

        addAvatarButton = new QPushButton(mainDialogFrame);
        addAvatarButton->setObjectName(QString::fromUtf8("addAvatarButton"));
        addAvatarButton->setMinimumSize(QSize(130, 0));

        horizontalLayout_2->addWidget(addAvatarButton);


        verticalLayout_3->addLayout(horizontalLayout_2);

        avatarListWidget = new QListWidget(mainDialogFrame);
        avatarListWidget->setObjectName(QString::fromUtf8("avatarListWidget"));
        avatarListWidget->setFrameShape(QFrame::NoFrame);
        avatarListWidget->setFrameShadow(QFrame::Plain);
        avatarListWidget->setLineWidth(0);
        avatarListWidget->setProperty("showDropIndicator", QVariant(false));
        avatarListWidget->setDragDropMode(QAbstractItemView::NoDragDrop);
        avatarListWidget->setIconSize(QSize(72, 72));
        avatarListWidget->setVerticalScrollMode(QAbstractItemView::ScrollPerPixel);
        avatarListWidget->setMovement(QListView::Static);
        avatarListWidget->setFlow(QListView::LeftToRight);
        avatarListWidget->setProperty("isWrapping", QVariant(true));
        avatarListWidget->setResizeMode(QListView::Adjust);
        avatarListWidget->setLayoutMode(QListView::SinglePass);
        avatarListWidget->setSpacing(4);
        avatarListWidget->setViewMode(QListView::IconMode);
        avatarListWidget->setUniformItemSizes(true);
        avatarListWidget->setSelectionRectVisible(true);

        verticalLayout_3->addWidget(avatarListWidget);


        frameVLayout->addWidget(mainDialogFrame);

        frameBottom = new QFrame(styledFrame);
        frameBottom->setObjectName(QString::fromUtf8("frameBottom"));
        frameBottom->setFrameShape(QFrame::NoFrame);
        frameBottom->setFrameShadow(QFrame::Raised);
        horizontalLayout_4 = new QHBoxLayout(frameBottom);
        horizontalLayout_4->setSpacing(0);
        horizontalLayout_4->setMargin(0);
        horizontalLayout_4->setObjectName(QString::fromUtf8("horizontalLayout_4"));
        horizontalLayout_4->setContentsMargins(0, 9, 9, -1);
        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_4->addItem(horizontalSpacer);

        okButton = new QPushButton(frameBottom);
        okButton->setObjectName(QString::fromUtf8("okButton"));

        horizontalLayout_4->addWidget(okButton);

        cancelButton = new QPushButton(frameBottom);
        cancelButton->setObjectName(QString::fromUtf8("cancelButton"));

        horizontalLayout_4->addWidget(cancelButton);


        frameVLayout->addWidget(frameBottom);


        verticalLayout_2->addWidget(styledFrame);


        retranslateUi(SelectAvatarDialogClass);

        QMetaObject::connectSlotsByName(SelectAvatarDialogClass);
    } // setupUi

    void retranslateUi(QDialog *SelectAvatarDialogClass)
    {
#ifndef QT_NO_ACCESSIBILITY
        closeToolButton->setAccessibleName(QApplication::translate("SelectAvatarDialogClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        topMessageLabel->setText(QApplication::translate("SelectAvatarDialogClass", "TopLabel", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        addAvatarButton->setAccessibleName(QApplication::translate("SelectAvatarDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        addAvatarButton->setText(QApplication::translate("SelectAvatarDialogClass", "AddAvatarButton", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        okButton->setAccessibleName(QApplication::translate("SelectAvatarDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        okButton->setText(QApplication::translate("SelectAvatarDialogClass", "OkButton", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        cancelButton->setAccessibleName(QApplication::translate("SelectAvatarDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        cancelButton->setText(QApplication::translate("SelectAvatarDialogClass", "CancelButton", 0, QApplication::UnicodeUTF8));
        Q_UNUSED(SelectAvatarDialogClass);
    } // retranslateUi

};

namespace Ui {
    class SelectAvatarDialogClass: public Ui_SelectAvatarDialogClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_SELECTAVATARDIALOG_H
