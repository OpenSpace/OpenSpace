/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#ifndef __OPENSPACE_UI_LAUNCHER___UI_MARKNODES___H__
#define __OPENSPACE_UI_LAUNCHER___UI_MARKNODES___H__

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QFrame>
#include <QtWidgets/QGridLayout>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QListWidget>
#include <QtWidgets/QPushButton>

QT_BEGIN_NAMESPACE

class Ui_markNodes
{
public:
    QGridLayout *gridLayout;
    QListWidget *list;
    QLabel *label_node;
    QLineEdit *line_node;
    QPushButton *button_remove;
    QPushButton *button_add;
    QHBoxLayout *hLay_bottom_buttonBox;
    QLabel *label_error;
    QDialogButtonBox *buttonBox;
    QFrame *line;

    void setupUi(QDialog *markNodes)
    {
        if (markNodes->objectName().isEmpty())
            markNodes->setObjectName(QString::fromUtf8("markNodes"));
        markNodes->resize(295, 500);
        markNodes->setMinimumSize(QSize(360, 500));
        markNodes->setMaximumSize(QSize(1000, 800));
        gridLayout = new QGridLayout(markNodes);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        list = new QListWidget(markNodes);
        list->setObjectName(QString::fromUtf8("list"));
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(list->sizePolicy().hasHeightForWidth());
        list->setSizePolicy(sizePolicy);
        list->setMinimumSize(QSize(0, 50));
        list->setMaximumSize(QSize(16777215, 1000));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        list->setFont(font);
        list->setAlternatingRowColors(true);
        list->setMovement(QListView::Free);
        list->setResizeMode(QListView::Adjust);

        gridLayout->addWidget(list, 0, 0, 1, 1);

        label_node = new QLabel(markNodes);
        label_node->setObjectName(QString::fromUtf8("label_node"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(label_node->sizePolicy().hasHeightForWidth());
        label_node->setSizePolicy(sizePolicy1);
        QFont font1;
        font1.setFamily(QString::fromUtf8("Arial"));
        font1.setPointSize(12);
        label_node->setFont(font1);

        gridLayout->addWidget(label_node, 2, 0, 1, 1);

        line_node = new QLineEdit(markNodes);
        line_node->setObjectName(QString::fromUtf8("line_node"));
        line_node->setMinimumSize(QSize(200, 0));
        line_node->setMaximumSize(QSize(800, 16777215));

        gridLayout->addWidget(line_node, 3, 0, 1, 1);

        button_remove = new QPushButton(markNodes);
        button_remove->setObjectName(QString::fromUtf8("button_remove"));
        button_remove->setMinimumSize(QSize(50, 0));
        button_remove->setMaximumSize(QSize(100, 16777215));
        button_remove->setFont(font);

        gridLayout->addWidget(button_remove, 1, 0, 1, 1);

        button_add = new QPushButton(markNodes);
        button_add->setObjectName(QString::fromUtf8("button_add"));
        button_add->setMaximumSize(QSize(100, 16777215));
        button_add->setFont(font);

        gridLayout->addWidget(button_add, 4, 0, 1, 1);

        buttonBox = new QDialogButtonBox(markNodes);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        //gridLayout->addWidget(buttonBox, 6, 0, 1, 1);

        line = new QFrame(markNodes);
        line->setObjectName(QString::fromUtf8("line"));
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);

        gridLayout->addWidget(line, 5, 0, 1, 1);

        hLay_bottom_buttonBox = new QHBoxLayout();
        hLay_bottom_buttonBox->setObjectName(QString::fromUtf8("hLay_bottom_buttonBox"));

        QFont fontE;
        fontE.setFamily(QString::fromUtf8("Arial"));
        label_error = new QLabel(markNodes);
        label_error->setObjectName(QString::fromUtf8("label_error"));
        QSizePolicy sizePolicy3(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy3.setHorizontalStretch(0);
        sizePolicy3.setVerticalStretch(0);
        sizePolicy3.setHeightForWidth(label_error->sizePolicy().hasHeightForWidth());
        label_error->setSizePolicy(sizePolicy3);
        label_error->setMinimumSize(QSize(200, 40));
        label_error->setMaximumSize(QSize(800, 40));
        label_error->setFont(fontE);
        label_error->setWordWrap(true);
        hLay_bottom_buttonBox->addWidget(label_error);

        hLay_bottom_buttonBox->addWidget(buttonBox);
        gridLayout->addLayout(hLay_bottom_buttonBox, 6, 0, 1, 1);

        retranslateUi(markNodes);
        //QObject::connect(buttonBox, SIGNAL(accepted()), markNodes, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), markNodes, SLOT(reject()));

        QMetaObject::connectSlotsByName(markNodes);
    } // setupUi

    void retranslateUi(QDialog *markNodes)
    {
        markNodes->setWindowTitle(QCoreApplication::translate("markNodes", "Mark Interesting Nodes", nullptr));
        label_node->setText(QCoreApplication::translate("markNodes", "Node to add:", nullptr));
#if QT_CONFIG(tooltip)
        line_node->setToolTip(QCoreApplication::translate("markNodes", "<html><head/><body><p>Name of scenegraph node to add to list of &quot;interesting&quot; nodes</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        button_remove->setText(QCoreApplication::translate("markNodes", "Remove", nullptr));
        button_add->setText(QCoreApplication::translate("markNodes", "Add New", nullptr));
        label_error->setText(QCoreApplication::translate("markNodes", "", nullptr));
    } // retranslateUi

};

namespace Ui {
    class markNodes: public Ui_markNodes {};
} // namespace Ui

QT_END_NAMESPACE

#endif // __OPENSPACE_UI_LAUNCHER___UI_MARKNODES___H__
