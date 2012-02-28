/********************************************************************************
** Form generated from reading ui file 'channellistitem.ui'
**
** Created: Tue May 5 10:34:36 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_CHANNELLISTITEM_H
#define UI_CHANNELLISTITEM_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QFrame>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_ChannelListItemClass
{
public:
    QHBoxLayout *horizontalLayout;
    QLabel *imageLabel;
    QVBoxLayout *verticalLayout;
    QLabel *titleLabel;
    QLabel *descriptionLabel;
    QPushButton *addChannelButton;
    QPushButton *removeChannelButton;

    void setupUi(QFrame *ChannelListItemClass)
    {
        if (ChannelListItemClass->objectName().isEmpty())
            ChannelListItemClass->setObjectName(QString::fromUtf8("ChannelListItemClass"));
        ChannelListItemClass->resize(421, 67);
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(ChannelListItemClass->sizePolicy().hasHeightForWidth());
        ChannelListItemClass->setSizePolicy(sizePolicy);
        ChannelListItemClass->setMaximumSize(QSize(16777215, 67));
        ChannelListItemClass->setFrameShape(QFrame::NoFrame);
        ChannelListItemClass->setFrameShadow(QFrame::Plain);
        ChannelListItemClass->setLineWidth(0);
        horizontalLayout = new QHBoxLayout(ChannelListItemClass);
        horizontalLayout->setSpacing(6);
        horizontalLayout->setMargin(11);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalLayout->setContentsMargins(7, 6, 7, 5);
        imageLabel = new QLabel(ChannelListItemClass);
        imageLabel->setObjectName(QString::fromUtf8("imageLabel"));
        QSizePolicy sizePolicy1(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(imageLabel->sizePolicy().hasHeightForWidth());
        imageLabel->setSizePolicy(sizePolicy1);
        imageLabel->setMinimumSize(QSize(51, 51));
        imageLabel->setMaximumSize(QSize(51, 51));
        imageLabel->setScaledContents(true);

        horizontalLayout->addWidget(imageLabel);

        verticalLayout = new QVBoxLayout();
        verticalLayout->setSpacing(1);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        titleLabel = new QLabel(ChannelListItemClass);
        titleLabel->setObjectName(QString::fromUtf8("titleLabel"));
        titleLabel->setMaximumSize(QSize(16777215, 12));
        QFont font;
        font.setFamily(QString::fromUtf8("MS Shell Dlg 2"));
        font.setPointSize(8);
        font.setBold(false);
        font.setItalic(false);
        font.setUnderline(false);
        font.setWeight(50);
        font.setStrikeOut(false);
        titleLabel->setFont(font);

        verticalLayout->addWidget(titleLabel);

        descriptionLabel = new QLabel(ChannelListItemClass);
        descriptionLabel->setObjectName(QString::fromUtf8("descriptionLabel"));
        descriptionLabel->setMaximumSize(QSize(16777215, 44));
        descriptionLabel->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignTop);
        descriptionLabel->setWordWrap(true);

        verticalLayout->addWidget(descriptionLabel);


        horizontalLayout->addLayout(verticalLayout);

        addChannelButton = new QPushButton(ChannelListItemClass);
        addChannelButton->setObjectName(QString::fromUtf8("addChannelButton"));
        sizePolicy1.setHeightForWidth(addChannelButton->sizePolicy().hasHeightForWidth());
        addChannelButton->setSizePolicy(sizePolicy1);
        addChannelButton->setMinimumSize(QSize(80, 25));

        horizontalLayout->addWidget(addChannelButton);

        removeChannelButton = new QPushButton(ChannelListItemClass);
        removeChannelButton->setObjectName(QString::fromUtf8("removeChannelButton"));
        sizePolicy1.setHeightForWidth(removeChannelButton->sizePolicy().hasHeightForWidth());
        removeChannelButton->setSizePolicy(sizePolicy1);
        removeChannelButton->setMinimumSize(QSize(80, 25));

        horizontalLayout->addWidget(removeChannelButton);


        retranslateUi(ChannelListItemClass);

        QMetaObject::connectSlotsByName(ChannelListItemClass);
    } // setupUi

    void retranslateUi(QFrame *ChannelListItemClass)
    {
        titleLabel->setText(QApplication::translate("ChannelListItemClass", "TextLabel_DT", 0, QApplication::UnicodeUTF8));
        descriptionLabel->setText(QApplication::translate("ChannelListItemClass", "TextLabel_DT", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        addChannelButton->setAccessibleName(QApplication::translate("ChannelListItemClass", "bigRectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
#ifndef QT_NO_ACCESSIBILITY
        removeChannelButton->setAccessibleName(QApplication::translate("ChannelListItemClass", "bigRectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        Q_UNUSED(ChannelListItemClass);
    } // retranslateUi

};

namespace Ui {
    class ChannelListItemClass: public Ui_ChannelListItemClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CHANNELLISTITEM_H
