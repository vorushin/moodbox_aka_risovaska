/********************************************************************************
** Form generated from reading ui file 'brushstylewidget.ui'
**
** Created: Tue May 5 10:34:35 2009
**      by: Qt User Interface Compiler version 4.5.1
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_BRUSHSTYLEWIDGET_H
#define UI_BRUSHSTYLEWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QHeaderView>
#include <QtGui/QListView>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_BrushStyleWidgetClass
{
public:
    QListView *styleList;

    void setupUi(QWidget *BrushStyleWidgetClass)
    {
        if (BrushStyleWidgetClass->objectName().isEmpty())
            BrushStyleWidgetClass->setObjectName(QString::fromUtf8("BrushStyleWidgetClass"));
        BrushStyleWidgetClass->resize(43, 206);
        styleList = new QListView(BrushStyleWidgetClass);
        styleList->setObjectName(QString::fromUtf8("styleList"));
        styleList->setGeometry(QRect(2, -1, 44, 207));
        styleList->setFrameShape(QFrame::NoFrame);
        styleList->setLineWidth(0);
        styleList->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        styleList->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        styleList->setEditTriggers(QAbstractItemView::NoEditTriggers);
        styleList->setProperty("showDropIndicator", QVariant(false));
        styleList->setDragDropMode(QAbstractItemView::NoDragDrop);
        styleList->setIconSize(QSize(41, 29));
        styleList->setMovement(QListView::Static);
        styleList->setFlow(QListView::LeftToRight);
        styleList->setViewMode(QListView::IconMode);
        styleList->setUniformItemSizes(true);

        retranslateUi(BrushStyleWidgetClass);

        QMetaObject::connectSlotsByName(BrushStyleWidgetClass);
    } // setupUi

    void retranslateUi(QWidget *BrushStyleWidgetClass)
    {
        Q_UNUSED(BrushStyleWidgetClass);
    } // retranslateUi

};

namespace Ui {
    class BrushStyleWidgetClass: public Ui_BrushStyleWidgetClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_BRUSHSTYLEWIDGET_H
