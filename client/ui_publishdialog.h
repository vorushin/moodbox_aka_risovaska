/********************************************************************************
** Form generated from reading ui file 'publishdialog.ui'
**
** Created: Tue May 5 10:34:50 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_PUBLISHDIALOG_H
#define UI_PUBLISHDIALOG_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QComboBox>
#include <QtGui/QDialog>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPlainTextEdit>
#include <QtGui/QProgressBar>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QStackedWidget>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include "clickedlineedit.h"

QT_BEGIN_NAMESPACE

class Ui_PublishDialogClass
{
public:
    QVBoxLayout *verticalLayout_8;
    QFrame *styledFrame;
    QVBoxLayout *verticalLayout_11;
    QHBoxLayout *horizontalLayout_3;
    QSpacerItem *horizontalSpacer_4;
    QToolButton *closeToolButton;
    QLabel *titleLabel;
    QStackedWidget *stackedWidget;
    QWidget *page;
    QVBoxLayout *verticalLayout_7;
    QFrame *mainDialogFrame;
    QVBoxLayout *verticalLayout_5;
    QLabel *moodstripNameLabel;
    QLineEdit *moodstripNameEdit;
    QCheckBox *hiddenCheckBox;
    QCheckBox *blogCheckBox;
    QWidget *ljControlsWidget;
    QVBoxLayout *verticalLayout;
    QFrame *line;
    QGridLayout *gridLayout_2;
    QLabel *accountLabel;
    QComboBox *loginCombo;
    QLabel *passwordLabel;
    QLineEdit *passwordEdit;
    QLabel *methodLabel;
    QComboBox *methodCombo;
    QLabel *label;
    QPlainTextEdit *postText;
    QLabel *typeLabel;
    QComboBox *typeCombo;
    QCheckBox *savePasswordCheck;
    QSpacerItem *verticalSpacer_3;
    QHBoxLayout *horizontalLayout;
    QSpacerItem *horizontalSpacer;
    QPushButton *cancelButton;
    QPushButton *publishingButton;
    QSpacerItem *horizontalSpacer_2;
    QWidget *page_2;
    QVBoxLayout *verticalLayout_12;
    QFrame *publishingFrame;
    QVBoxLayout *verticalLayout_4;
    QSpacerItem *verticalSpacer;
    QLabel *publishingLabel;
    QProgressBar *publishingProgressBar;
    QSpacerItem *verticalSpacer_2;
    QHBoxLayout *horizontalLayout_2;
    QPushButton *cancelPublishingButton;
    QWidget *page_3;
    QVBoxLayout *verticalLayout_9;
    QFrame *publishedFrame;
    QVBoxLayout *verticalLayout_6;
    QLabel *publishedLabel;
    QSpacerItem *verticalSpacer_6;
    QGridLayout *gridLayout;
    QLabel *ljUrlLabel;
    QVBoxLayout *verticalLayout_2;
    MoodBox::ClickedLineEdit *ljUrlEdit;
    QHBoxLayout *horizontalLayout_6;
    QSpacerItem *horizontalSpacer_5;
    QPushButton *blogUrlButton;
    QLabel *urlLabel;
    QVBoxLayout *verticalLayout_3;
    MoodBox::ClickedLineEdit *urlEdit;
    QHBoxLayout *horizontalLayout_4;
    QSpacerItem *horizontalSpacer_3;
    QPushButton *urlButton;
    MoodBox::ClickedLineEdit *embedEdit;
    QLabel *embedLabel;
    MoodBox::ClickedLineEdit *embedMoodstripEdit;
    QLabel *moodstripLabel;
    QLabel *ljMoodstripCutLabel;
    MoodBox::ClickedLineEdit *embedMoodstripCutEdit;
    QSpacerItem *verticalSpacer_7;
    QHBoxLayout *horizontalLayout_5;
    QPushButton *closeButton;

    void setupUi(QDialog *PublishDialogClass)
    {
        if (PublishDialogClass->objectName().isEmpty())
            PublishDialogClass->setObjectName(QString::fromUtf8("PublishDialogClass"));
        PublishDialogClass->setWindowModality(Qt::NonModal);
        PublishDialogClass->resize(322, 523);
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(PublishDialogClass->sizePolicy().hasHeightForWidth());
        PublishDialogClass->setSizePolicy(sizePolicy);
        PublishDialogClass->setMinimumSize(QSize(296, 0));
        PublishDialogClass->setModal(true);
        verticalLayout_8 = new QVBoxLayout(PublishDialogClass);
        verticalLayout_8->setSpacing(0);
        verticalLayout_8->setMargin(0);
        verticalLayout_8->setObjectName(QString::fromUtf8("verticalLayout_8"));
        verticalLayout_8->setSizeConstraint(QLayout::SetFixedSize);
        styledFrame = new QFrame(PublishDialogClass);
        styledFrame->setObjectName(QString::fromUtf8("styledFrame"));
        styledFrame->setMinimumSize(QSize(296, 0));
        styledFrame->setFrameShape(QFrame::NoFrame);
        styledFrame->setFrameShadow(QFrame::Plain);
        styledFrame->setLineWidth(0);
        verticalLayout_11 = new QVBoxLayout(styledFrame);
        verticalLayout_11->setSpacing(5);
        verticalLayout_11->setMargin(0);
        verticalLayout_11->setObjectName(QString::fromUtf8("verticalLayout_11"));
        verticalLayout_11->setSizeConstraint(QLayout::SetFixedSize);
        verticalLayout_11->setContentsMargins(1, 0, 1, 1);
        horizontalLayout_3 = new QHBoxLayout();
        horizontalLayout_3->setSpacing(0);
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        horizontalLayout_3->setContentsMargins(-1, 5, 5, -1);
        horizontalSpacer_4 = new QSpacerItem(10, 5, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_4);

        closeToolButton = new QToolButton(styledFrame);
        closeToolButton->setObjectName(QString::fromUtf8("closeToolButton"));
        closeToolButton->setFocusPolicy(Qt::NoFocus);
        closeToolButton->setLayoutDirection(Qt::LeftToRight);
        closeToolButton->setPopupMode(QToolButton::DelayedPopup);

        horizontalLayout_3->addWidget(closeToolButton);


        verticalLayout_11->addLayout(horizontalLayout_3);

        titleLabel = new QLabel(styledFrame);
        titleLabel->setObjectName(QString::fromUtf8("titleLabel"));
        titleLabel->setIndent(11);

        verticalLayout_11->addWidget(titleLabel);

        stackedWidget = new QStackedWidget(styledFrame);
        stackedWidget->setObjectName(QString::fromUtf8("stackedWidget"));
        page = new QWidget();
        page->setObjectName(QString::fromUtf8("page"));
        verticalLayout_7 = new QVBoxLayout(page);
        verticalLayout_7->setSpacing(0);
        verticalLayout_7->setMargin(0);
        verticalLayout_7->setObjectName(QString::fromUtf8("verticalLayout_7"));
        mainDialogFrame = new QFrame(page);
        mainDialogFrame->setObjectName(QString::fromUtf8("mainDialogFrame"));
        mainDialogFrame->setFrameShape(QFrame::NoFrame);
        mainDialogFrame->setFrameShadow(QFrame::Plain);
        mainDialogFrame->setLineWidth(0);
        verticalLayout_5 = new QVBoxLayout(mainDialogFrame);
        verticalLayout_5->setSpacing(8);
        verticalLayout_5->setMargin(0);
        verticalLayout_5->setObjectName(QString::fromUtf8("verticalLayout_5"));
        verticalLayout_5->setSizeConstraint(QLayout::SetDefaultConstraint);
        verticalLayout_5->setContentsMargins(20, 45, 20, 10);
        moodstripNameLabel = new QLabel(mainDialogFrame);
        moodstripNameLabel->setObjectName(QString::fromUtf8("moodstripNameLabel"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(moodstripNameLabel->sizePolicy().hasHeightForWidth());
        moodstripNameLabel->setSizePolicy(sizePolicy1);

        verticalLayout_5->addWidget(moodstripNameLabel);

        moodstripNameEdit = new QLineEdit(mainDialogFrame);
        moodstripNameEdit->setObjectName(QString::fromUtf8("moodstripNameEdit"));
        QSizePolicy sizePolicy2(QSizePolicy::Expanding, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(moodstripNameEdit->sizePolicy().hasHeightForWidth());
        moodstripNameEdit->setSizePolicy(sizePolicy2);
        moodstripNameEdit->setMinimumSize(QSize(285, 0));

        verticalLayout_5->addWidget(moodstripNameEdit);

        hiddenCheckBox = new QCheckBox(mainDialogFrame);
        hiddenCheckBox->setObjectName(QString::fromUtf8("hiddenCheckBox"));
        hiddenCheckBox->setChecked(false);

        verticalLayout_5->addWidget(hiddenCheckBox);

        blogCheckBox = new QCheckBox(mainDialogFrame);
        blogCheckBox->setObjectName(QString::fromUtf8("blogCheckBox"));
        blogCheckBox->setChecked(false);

        verticalLayout_5->addWidget(blogCheckBox);

        ljControlsWidget = new QWidget(mainDialogFrame);
        ljControlsWidget->setObjectName(QString::fromUtf8("ljControlsWidget"));
        verticalLayout = new QVBoxLayout(ljControlsWidget);
        verticalLayout->setSpacing(10);
        verticalLayout->setMargin(0);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        verticalLayout->setContentsMargins(0, 6, 0, 0);
        line = new QFrame(ljControlsWidget);
        line->setObjectName(QString::fromUtf8("line"));
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(line);

        gridLayout_2 = new QGridLayout();
        gridLayout_2->setSpacing(0);
        gridLayout_2->setObjectName(QString::fromUtf8("gridLayout_2"));
        gridLayout_2->setHorizontalSpacing(5);
        gridLayout_2->setVerticalSpacing(10);
        accountLabel = new QLabel(ljControlsWidget);
        accountLabel->setObjectName(QString::fromUtf8("accountLabel"));

        gridLayout_2->addWidget(accountLabel, 1, 0, 1, 1);

        loginCombo = new QComboBox(ljControlsWidget);
        loginCombo->setObjectName(QString::fromUtf8("loginCombo"));
        loginCombo->setMaximumSize(QSize(150, 16777215));
        loginCombo->setEditable(true);

        gridLayout_2->addWidget(loginCombo, 1, 1, 1, 1);

        passwordLabel = new QLabel(ljControlsWidget);
        passwordLabel->setObjectName(QString::fromUtf8("passwordLabel"));

        gridLayout_2->addWidget(passwordLabel, 2, 0, 1, 1);

        passwordEdit = new QLineEdit(ljControlsWidget);
        passwordEdit->setObjectName(QString::fromUtf8("passwordEdit"));
        sizePolicy.setHeightForWidth(passwordEdit->sizePolicy().hasHeightForWidth());
        passwordEdit->setSizePolicy(sizePolicy);
        passwordEdit->setMaximumSize(QSize(150, 16777215));
        passwordEdit->setEchoMode(QLineEdit::Password);

        gridLayout_2->addWidget(passwordEdit, 2, 1, 1, 1);

        methodLabel = new QLabel(ljControlsWidget);
        methodLabel->setObjectName(QString::fromUtf8("methodLabel"));

        gridLayout_2->addWidget(methodLabel, 4, 0, 1, 1);

        methodCombo = new QComboBox(ljControlsWidget);
        methodCombo->setObjectName(QString::fromUtf8("methodCombo"));
        methodCombo->setMaximumSize(QSize(170, 16777215));

        gridLayout_2->addWidget(methodCombo, 4, 1, 1, 2);

        label = new QLabel(ljControlsWidget);
        label->setObjectName(QString::fromUtf8("label"));

        gridLayout_2->addWidget(label, 5, 0, 1, 3);

        postText = new QPlainTextEdit(ljControlsWidget);
        postText->setObjectName(QString::fromUtf8("postText"));
        postText->setMaximumSize(QSize(16777215, 65));

        gridLayout_2->addWidget(postText, 6, 0, 1, 3);

        typeLabel = new QLabel(ljControlsWidget);
        typeLabel->setObjectName(QString::fromUtf8("typeLabel"));
        typeLabel->setMinimumSize(QSize(90, 0));

        gridLayout_2->addWidget(typeLabel, 0, 0, 1, 1);

        typeCombo = new QComboBox(ljControlsWidget);
        typeCombo->setObjectName(QString::fromUtf8("typeCombo"));

        gridLayout_2->addWidget(typeCombo, 0, 1, 1, 1);

        savePasswordCheck = new QCheckBox(ljControlsWidget);
        savePasswordCheck->setObjectName(QString::fromUtf8("savePasswordCheck"));

        gridLayout_2->addWidget(savePasswordCheck, 3, 1, 1, 1);


        verticalLayout->addLayout(gridLayout_2);


        verticalLayout_5->addWidget(ljControlsWidget);

        verticalSpacer_3 = new QSpacerItem(1, 1, QSizePolicy::Minimum, QSizePolicy::MinimumExpanding);

        verticalLayout_5->addItem(verticalSpacer_3);


        verticalLayout_7->addWidget(mainDialogFrame);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(8);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalLayout->setContentsMargins(15, 9, -1, 12);
        horizontalSpacer = new QSpacerItem(20, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);

        cancelButton = new QPushButton(page);
        cancelButton->setObjectName(QString::fromUtf8("cancelButton"));
        sizePolicy.setHeightForWidth(cancelButton->sizePolicy().hasHeightForWidth());
        cancelButton->setSizePolicy(sizePolicy);
        cancelButton->setMinimumSize(QSize(90, 25));
        cancelButton->setMaximumSize(QSize(90, 25));
        cancelButton->setAutoDefault(false);

        horizontalLayout->addWidget(cancelButton);

        publishingButton = new QPushButton(page);
        publishingButton->setObjectName(QString::fromUtf8("publishingButton"));
        sizePolicy.setHeightForWidth(publishingButton->sizePolicy().hasHeightForWidth());
        publishingButton->setSizePolicy(sizePolicy);
        publishingButton->setMinimumSize(QSize(90, 25));
        publishingButton->setMaximumSize(QSize(90, 25));

        horizontalLayout->addWidget(publishingButton);

        horizontalSpacer_2 = new QSpacerItem(20, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer_2);


        verticalLayout_7->addLayout(horizontalLayout);

        stackedWidget->addWidget(page);
        page_2 = new QWidget();
        page_2->setObjectName(QString::fromUtf8("page_2"));
        verticalLayout_12 = new QVBoxLayout(page_2);
        verticalLayout_12->setSpacing(0);
        verticalLayout_12->setMargin(0);
        verticalLayout_12->setObjectName(QString::fromUtf8("verticalLayout_12"));
        publishingFrame = new QFrame(page_2);
        publishingFrame->setObjectName(QString::fromUtf8("publishingFrame"));
        publishingFrame->setMinimumSize(QSize(0, 0));
        publishingFrame->setFrameShape(QFrame::NoFrame);
        publishingFrame->setFrameShadow(QFrame::Plain);
        publishingFrame->setLineWidth(0);
        verticalLayout_4 = new QVBoxLayout(publishingFrame);
        verticalLayout_4->setSpacing(11);
        verticalLayout_4->setMargin(0);
        verticalLayout_4->setObjectName(QString::fromUtf8("verticalLayout_4"));
        verticalLayout_4->setContentsMargins(20, -1, 20, -1);
        verticalSpacer = new QSpacerItem(1, 1, QSizePolicy::Minimum, QSizePolicy::MinimumExpanding);

        verticalLayout_4->addItem(verticalSpacer);

        publishingLabel = new QLabel(publishingFrame);
        publishingLabel->setObjectName(QString::fromUtf8("publishingLabel"));
        sizePolicy1.setHeightForWidth(publishingLabel->sizePolicy().hasHeightForWidth());
        publishingLabel->setSizePolicy(sizePolicy1);
        publishingLabel->setTextFormat(Qt::PlainText);
        publishingLabel->setAlignment(Qt::AlignCenter|Qt::AlignHCenter|Qt::AlignLeading|Qt::AlignLeft|Qt::AlignVCenter);

        verticalLayout_4->addWidget(publishingLabel);

        publishingProgressBar = new QProgressBar(publishingFrame);
        publishingProgressBar->setObjectName(QString::fromUtf8("publishingProgressBar"));
        publishingProgressBar->setValue(24);
        publishingProgressBar->setTextVisible(false);

        verticalLayout_4->addWidget(publishingProgressBar);

        verticalSpacer_2 = new QSpacerItem(1, 1, QSizePolicy::Minimum, QSizePolicy::MinimumExpanding);

        verticalLayout_4->addItem(verticalSpacer_2);


        verticalLayout_12->addWidget(publishingFrame);

        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setSpacing(0);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        horizontalLayout_2->setContentsMargins(15, 9, -1, 12);
        cancelPublishingButton = new QPushButton(page_2);
        cancelPublishingButton->setObjectName(QString::fromUtf8("cancelPublishingButton"));
        cancelPublishingButton->setMinimumSize(QSize(77, 25));
        cancelPublishingButton->setMaximumSize(QSize(77, 25));

        horizontalLayout_2->addWidget(cancelPublishingButton);


        verticalLayout_12->addLayout(horizontalLayout_2);

        stackedWidget->addWidget(page_2);
        page_3 = new QWidget();
        page_3->setObjectName(QString::fromUtf8("page_3"));
        verticalLayout_9 = new QVBoxLayout(page_3);
        verticalLayout_9->setSpacing(0);
        verticalLayout_9->setMargin(0);
        verticalLayout_9->setObjectName(QString::fromUtf8("verticalLayout_9"));
        verticalLayout_9->setSizeConstraint(QLayout::SetDefaultConstraint);
        publishedFrame = new QFrame(page_3);
        publishedFrame->setObjectName(QString::fromUtf8("publishedFrame"));
        publishedFrame->setMinimumSize(QSize(0, 0));
        publishedFrame->setFrameShape(QFrame::NoFrame);
        publishedFrame->setFrameShadow(QFrame::Plain);
        publishedFrame->setLineWidth(0);
        verticalLayout_6 = new QVBoxLayout(publishedFrame);
        verticalLayout_6->setSpacing(0);
        verticalLayout_6->setMargin(0);
        verticalLayout_6->setObjectName(QString::fromUtf8("verticalLayout_6"));
        verticalLayout_6->setSizeConstraint(QLayout::SetDefaultConstraint);
        verticalLayout_6->setContentsMargins(20, 30, 20, -1);
        publishedLabel = new QLabel(publishedFrame);
        publishedLabel->setObjectName(QString::fromUtf8("publishedLabel"));
        publishedLabel->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignVCenter);

        verticalLayout_6->addWidget(publishedLabel);

        verticalSpacer_6 = new QSpacerItem(1, 1, QSizePolicy::Minimum, QSizePolicy::MinimumExpanding);

        verticalLayout_6->addItem(verticalSpacer_6);

        gridLayout = new QGridLayout();
        gridLayout->setSpacing(10);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        gridLayout->setSizeConstraint(QLayout::SetMaximumSize);
        ljUrlLabel = new QLabel(publishedFrame);
        ljUrlLabel->setObjectName(QString::fromUtf8("ljUrlLabel"));
        ljUrlLabel->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignTop);

        gridLayout->addWidget(ljUrlLabel, 0, 0, 1, 1);

        verticalLayout_2 = new QVBoxLayout();
        verticalLayout_2->setSpacing(4);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        ljUrlEdit = new MoodBox::ClickedLineEdit(publishedFrame);
        ljUrlEdit->setObjectName(QString::fromUtf8("ljUrlEdit"));
        ljUrlEdit->setAlignment(Qt::AlignLeading);
        ljUrlEdit->setReadOnly(true);

        verticalLayout_2->addWidget(ljUrlEdit);

        horizontalLayout_6 = new QHBoxLayout();
        horizontalLayout_6->setSpacing(0);
        horizontalLayout_6->setObjectName(QString::fromUtf8("horizontalLayout_6"));
        horizontalLayout_6->setContentsMargins(-1, -1, -1, 0);
        horizontalSpacer_5 = new QSpacerItem(40, 10, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_6->addItem(horizontalSpacer_5);

        blogUrlButton = new QPushButton(publishedFrame);
        blogUrlButton->setObjectName(QString::fromUtf8("blogUrlButton"));
        blogUrlButton->setMaximumSize(QSize(16777215, 10));
        QFont font;
        font.setFamily(QString::fromUtf8("MS Shell Dlg 2"));
        font.setPointSize(8);
        font.setBold(false);
        font.setItalic(false);
        font.setUnderline(true);
        font.setWeight(50);
        font.setStrikeOut(false);
        blogUrlButton->setFont(font);
        blogUrlButton->setCursor(QCursor(Qt::PointingHandCursor));
        blogUrlButton->setAutoDefault(false);
        blogUrlButton->setFlat(true);

        horizontalLayout_6->addWidget(blogUrlButton);


        verticalLayout_2->addLayout(horizontalLayout_6);


        gridLayout->addLayout(verticalLayout_2, 0, 1, 1, 1);

        urlLabel = new QLabel(publishedFrame);
        urlLabel->setObjectName(QString::fromUtf8("urlLabel"));
        urlLabel->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignTop);

        gridLayout->addWidget(urlLabel, 1, 0, 1, 1);

        verticalLayout_3 = new QVBoxLayout();
        verticalLayout_3->setSpacing(3);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        urlEdit = new MoodBox::ClickedLineEdit(publishedFrame);
        urlEdit->setObjectName(QString::fromUtf8("urlEdit"));
        urlEdit->setAlignment(Qt::AlignLeading);
        urlEdit->setReadOnly(true);

        verticalLayout_3->addWidget(urlEdit);

        horizontalLayout_4 = new QHBoxLayout();
        horizontalLayout_4->setSpacing(0);
        horizontalLayout_4->setObjectName(QString::fromUtf8("horizontalLayout_4"));
        horizontalLayout_4->setContentsMargins(-1, -1, -1, 0);
        horizontalSpacer_3 = new QSpacerItem(40, 10, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_4->addItem(horizontalSpacer_3);

        urlButton = new QPushButton(publishedFrame);
        urlButton->setObjectName(QString::fromUtf8("urlButton"));
        urlButton->setMaximumSize(QSize(16777215, 10));
        urlButton->setFont(font);
        urlButton->setCursor(QCursor(Qt::PointingHandCursor));
        urlButton->setAutoDefault(false);
        urlButton->setFlat(true);

        horizontalLayout_4->addWidget(urlButton);


        verticalLayout_3->addLayout(horizontalLayout_4);


        gridLayout->addLayout(verticalLayout_3, 1, 1, 1, 1);

        embedEdit = new MoodBox::ClickedLineEdit(publishedFrame);
        embedEdit->setObjectName(QString::fromUtf8("embedEdit"));
        embedEdit->setAlignment(Qt::AlignLeading);
        embedEdit->setReadOnly(true);

        gridLayout->addWidget(embedEdit, 2, 1, 1, 1);

        embedLabel = new QLabel(publishedFrame);
        embedLabel->setObjectName(QString::fromUtf8("embedLabel"));

        gridLayout->addWidget(embedLabel, 2, 0, 1, 1);

        embedMoodstripEdit = new MoodBox::ClickedLineEdit(publishedFrame);
        embedMoodstripEdit->setObjectName(QString::fromUtf8("embedMoodstripEdit"));
        embedMoodstripEdit->setAlignment(Qt::AlignLeading);
        embedMoodstripEdit->setReadOnly(true);

        gridLayout->addWidget(embedMoodstripEdit, 3, 1, 1, 1);

        moodstripLabel = new QLabel(publishedFrame);
        moodstripLabel->setObjectName(QString::fromUtf8("moodstripLabel"));

        gridLayout->addWidget(moodstripLabel, 3, 0, 1, 1);

        ljMoodstripCutLabel = new QLabel(publishedFrame);
        ljMoodstripCutLabel->setObjectName(QString::fromUtf8("ljMoodstripCutLabel"));

        gridLayout->addWidget(ljMoodstripCutLabel, 4, 0, 1, 1);

        embedMoodstripCutEdit = new MoodBox::ClickedLineEdit(publishedFrame);
        embedMoodstripCutEdit->setObjectName(QString::fromUtf8("embedMoodstripCutEdit"));
        embedMoodstripCutEdit->setAlignment(Qt::AlignLeading);
        embedMoodstripCutEdit->setReadOnly(true);

        gridLayout->addWidget(embedMoodstripCutEdit, 4, 1, 1, 1);


        verticalLayout_6->addLayout(gridLayout);

        verticalSpacer_7 = new QSpacerItem(1, 1, QSizePolicy::Minimum, QSizePolicy::MinimumExpanding);

        verticalLayout_6->addItem(verticalSpacer_7);


        verticalLayout_9->addWidget(publishedFrame);

        horizontalLayout_5 = new QHBoxLayout();
        horizontalLayout_5->setSpacing(0);
        horizontalLayout_5->setObjectName(QString::fromUtf8("horizontalLayout_5"));
        horizontalLayout_5->setContentsMargins(15, 9, -1, 12);
        closeButton = new QPushButton(page_3);
        closeButton->setObjectName(QString::fromUtf8("closeButton"));
        sizePolicy.setHeightForWidth(closeButton->sizePolicy().hasHeightForWidth());
        closeButton->setSizePolicy(sizePolicy);
        closeButton->setMinimumSize(QSize(77, 25));
        closeButton->setMaximumSize(QSize(77, 25));

        horizontalLayout_5->addWidget(closeButton);


        verticalLayout_9->addLayout(horizontalLayout_5);

        stackedWidget->addWidget(page_3);

        verticalLayout_11->addWidget(stackedWidget);


        verticalLayout_8->addWidget(styledFrame);

#ifndef QT_NO_SHORTCUT
        moodstripNameLabel->setBuddy(moodstripNameEdit);
        accountLabel->setBuddy(loginCombo);
        passwordLabel->setBuddy(passwordEdit);
        methodLabel->setBuddy(methodCombo);
        label->setBuddy(postText);
        urlLabel->setBuddy(urlEdit);
        embedLabel->setBuddy(embedEdit);
#endif // QT_NO_SHORTCUT
        QWidget::setTabOrder(moodstripNameEdit, hiddenCheckBox);
        QWidget::setTabOrder(hiddenCheckBox, blogCheckBox);
        QWidget::setTabOrder(blogCheckBox, loginCombo);
        QWidget::setTabOrder(loginCombo, passwordEdit);
        QWidget::setTabOrder(passwordEdit, methodCombo);
        QWidget::setTabOrder(methodCombo, postText);
        QWidget::setTabOrder(postText, cancelButton);
        QWidget::setTabOrder(cancelButton, publishingButton);
        QWidget::setTabOrder(publishingButton, cancelPublishingButton);
        QWidget::setTabOrder(cancelPublishingButton, blogUrlButton);
        QWidget::setTabOrder(blogUrlButton, urlButton);
        QWidget::setTabOrder(urlButton, closeButton);

        retranslateUi(PublishDialogClass);

        stackedWidget->setCurrentIndex(0);


        QMetaObject::connectSlotsByName(PublishDialogClass);
    } // setupUi

    void retranslateUi(QDialog *PublishDialogClass)
    {
#ifndef QT_NO_ACCESSIBILITY
        closeToolButton->setAccessibleName(QApplication::translate("PublishDialogClass", "transparentButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        titleLabel->setText(QApplication::translate("PublishDialogClass", "PublishingMoodstripTitle", 0, QApplication::UnicodeUTF8));
        moodstripNameLabel->setText(QApplication::translate("PublishDialogClass", "NameYourMoodstrip", 0, QApplication::UnicodeUTF8));
        hiddenCheckBox->setText(QApplication::translate("PublishDialogClass", "IsHiddenMoodstrip", 0, QApplication::UnicodeUTF8));
        blogCheckBox->setText(QApplication::translate("PublishDialogClass", "PostToBlog", 0, QApplication::UnicodeUTF8));
        accountLabel->setText(QApplication::translate("PublishDialogClass", "LJAccount", 0, QApplication::UnicodeUTF8));
        passwordLabel->setText(QApplication::translate("PublishDialogClass", "Password", 0, QApplication::UnicodeUTF8));
        methodLabel->setText(QApplication::translate("PublishDialogClass", "PostMethod", 0, QApplication::UnicodeUTF8));
        label->setText(QApplication::translate("PublishDialogClass", "PostTextBeforeMoodstrip", 0, QApplication::UnicodeUTF8));
        typeLabel->setText(QApplication::translate("PublishDialogClass", "BlogType", 0, QApplication::UnicodeUTF8));
        savePasswordCheck->setText(QApplication::translate("PublishDialogClass", "SavePassword", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        cancelButton->setAccessibleName(QApplication::translate("PublishDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        cancelButton->setText(QApplication::translate("PublishDialogClass", "CancelButton", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        publishingButton->setAccessibleName(QApplication::translate("PublishDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        publishingButton->setText(QApplication::translate("PublishDialogClass", "NextButton", 0, QApplication::UnicodeUTF8));
        publishingLabel->setText(QApplication::translate("PublishDialogClass", "Publishing...", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        cancelPublishingButton->setAccessibleName(QApplication::translate("PublishDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        cancelPublishingButton->setText(QApplication::translate("PublishDialogClass", "CancelButton", 0, QApplication::UnicodeUTF8));
        publishedLabel->setText(QApplication::translate("PublishDialogClass", "Published", 0, QApplication::UnicodeUTF8));
        ljUrlLabel->setText(QApplication::translate("PublishDialogClass", "UrlLJ", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        blogUrlButton->setAccessibleName(QApplication::translate("PublishDialogClass", "blueButton8", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        blogUrlButton->setText(QApplication::translate("PublishDialogClass", "OpenBlogUrlButton", 0, QApplication::UnicodeUTF8));
        urlLabel->setText(QApplication::translate("PublishDialogClass", "URL", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        urlButton->setAccessibleName(QApplication::translate("PublishDialogClass", "blueButton8", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        urlButton->setText(QApplication::translate("PublishDialogClass", "OpenUrlButton", 0, QApplication::UnicodeUTF8));
        embedLabel->setText(QApplication::translate("PublishDialogClass", "Embed", 0, QApplication::UnicodeUTF8));
        moodstripLabel->setText(QApplication::translate("PublishDialogClass", "EmbedMoodstrip", 0, QApplication::UnicodeUTF8));
        ljMoodstripCutLabel->setText(QApplication::translate("PublishDialogClass", "EmbedMoodstripCut", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        closeButton->setAccessibleName(QApplication::translate("PublishDialogClass", "rectButton", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_ACCESSIBILITY
        closeButton->setText(QApplication::translate("PublishDialogClass", "FinishButton", 0, QApplication::UnicodeUTF8));
        Q_UNUSED(PublishDialogClass);
    } // retranslateUi

};

namespace Ui {
    class PublishDialogClass: public Ui_PublishDialogClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_PUBLISHDIALOG_H
