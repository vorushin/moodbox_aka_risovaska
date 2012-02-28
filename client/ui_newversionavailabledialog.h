/********************************************************************************
** Form generated from reading ui file 'newversionavailabledialog.ui'
**
** Created: Tue May 5 10:34:33 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_NEWVERSIONAVAILABLEDIALOG_H
#define UI_NEWVERSIONAVAILABLEDIALOG_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialog>
#include <QtGui/QFrame>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QTextBrowser>
#include <QtGui/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_NewVersionAvailableDialogClass
{
public:
    QVBoxLayout *verticalLayout_2;
    QFrame *styledFrame;
    QVBoxLayout *frameVLayout;
    QVBoxLayout *verticalLayout;
    QTextBrowser *descriptionBox;
    QHBoxLayout *hboxLayout;
    QSpacerItem *spacerItem;
    QPushButton *downloadButton;
    QPushButton *closeButton;

    void setupUi(QDialog *NewVersionAvailableDialogClass)
    {
        if (NewVersionAvailableDialogClass->objectName().isEmpty())
            NewVersionAvailableDialogClass->setObjectName(QString::fromUtf8("NewVersionAvailableDialogClass"));
        NewVersionAvailableDialogClass->setWindowModality(Qt::ApplicationModal);
        NewVersionAvailableDialogClass->resize(424, 377);
        verticalLayout_2 = new QVBoxLayout(NewVersionAvailableDialogClass);
        verticalLayout_2->setSpacing(0);
        verticalLayout_2->setMargin(0);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        verticalLayout_2->setSizeConstraint(QLayout::SetFixedSize);
        styledFrame = new QFrame(NewVersionAvailableDialogClass);
        styledFrame->setObjectName(QString::fromUtf8("styledFrame"));
        styledFrame->setFrameShape(QFrame::NoFrame);
        styledFrame->setFrameShadow(QFrame::Plain);
        styledFrame->setLineWidth(0);
        frameVLayout = new QVBoxLayout(styledFrame);
        frameVLayout->setSpacing(0);
        frameVLayout->setMargin(0);
        frameVLayout->setObjectName(QString::fromUtf8("frameVLayout"));
        verticalLayout = new QVBoxLayout();
        verticalLayout->setSpacing(6);
        verticalLayout->setMargin(9);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        descriptionBox = new QTextBrowser(styledFrame);
        descriptionBox->setObjectName(QString::fromUtf8("descriptionBox"));
        descriptionBox->setTabChangesFocus(true);
        descriptionBox->setOpenExternalLinks(true);

        verticalLayout->addWidget(descriptionBox);

        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        spacerItem = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(spacerItem);

        downloadButton = new QPushButton(styledFrame);
        downloadButton->setObjectName(QString::fromUtf8("downloadButton"));
        downloadButton->setDefault(true);

        hboxLayout->addWidget(downloadButton);

        closeButton = new QPushButton(styledFrame);
        closeButton->setObjectName(QString::fromUtf8("closeButton"));

        hboxLayout->addWidget(closeButton);


        verticalLayout->addLayout(hboxLayout);


        frameVLayout->addLayout(verticalLayout);


        verticalLayout_2->addWidget(styledFrame);


        retranslateUi(NewVersionAvailableDialogClass);

        QMetaObject::connectSlotsByName(NewVersionAvailableDialogClass);
    } // setupUi

    void retranslateUi(QDialog *NewVersionAvailableDialogClass)
    {
        NewVersionAvailableDialogClass->setWindowTitle(QApplication::translate("NewVersionAvailableDialogClass", "WindowTitle", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        downloadButton->setAccessibleName(QApplication::translate("NewVersionAvailableDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        downloadButton->setText(QApplication::translate("NewVersionAvailableDialogClass", "DownloadButton", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        closeButton->setAccessibleName(QApplication::translate("NewVersionAvailableDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        closeButton->setText(QApplication::translate("NewVersionAvailableDialogClass", "CloseButton", 0, QApplication::UnicodeUTF8));
        Q_UNUSED(NewVersionAvailableDialogClass);
    } // retranslateUi

};

namespace Ui {
    class NewVersionAvailableDialogClass: public Ui_NewVersionAvailableDialogClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_NEWVERSIONAVAILABLEDIALOG_H
