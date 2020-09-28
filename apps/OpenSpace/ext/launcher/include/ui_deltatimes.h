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

#ifndef UI_DELTATIMES_H
#define UI_DELTATIMES_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QListWidget>
#include <QtWidgets/QPushButton>

QT_BEGIN_NAMESPACE

class Ui_deltaTimes
{
public:
    QDialogButtonBox *buttonBox;
    QLabel *label_module;
    QListWidget *listWidget;
    QLabel *label_adjust;
    QPushButton *button_save;
    QPushButton *button_cancel;
    QPushButton *button_add;
    QPushButton *button_remove;
    QLineEdit *line_seconds;

    void setupUi(QDialog *deltaTimes)
    {
        if (deltaTimes->objectName().isEmpty())
            deltaTimes->setObjectName(QString::fromUtf8("deltaTimes"));
        deltaTimes->resize(531, 439);
        buttonBox = new QDialogButtonBox(deltaTimes);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setGeometry(QRect(170, 390, 341, 32));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);
        label_module = new QLabel(deltaTimes);
        label_module->setObjectName(QString::fromUtf8("label_module"));
        label_module->setGeometry(QRect(20, 20, 141, 20));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        font.setPointSize(12);
        label_module->setFont(font);
        listWidget = new QListWidget(deltaTimes);
        listWidget->setObjectName(QString::fromUtf8("listWidget"));
        listWidget->setGeometry(QRect(20, 50, 381, 171));
        listWidget->setAutoScroll(true);
        listWidget->setLayoutMode(QListView::SinglePass);
        label_adjust = new QLabel(deltaTimes);
        label_adjust->setObjectName(QString::fromUtf8("label_adjust"));
        label_adjust->setGeometry(QRect(20, 284, 421, 20));
        label_adjust->setFont(font);
        button_save = new QPushButton(deltaTimes);
        button_save->setObjectName(QString::fromUtf8("button_save"));
        button_save->setGeometry(QRect(20, 354, 71, 25));
        button_cancel = new QPushButton(deltaTimes);
        button_cancel->setObjectName(QString::fromUtf8("button_cancel"));
        button_cancel->setGeometry(QRect(120, 354, 71, 25));
        button_add = new QPushButton(deltaTimes);
        button_add->setObjectName(QString::fromUtf8("button_add"));
        button_add->setGeometry(QRect(20, 230, 151, 25));
        button_remove = new QPushButton(deltaTimes);
        button_remove->setObjectName(QString::fromUtf8("button_remove"));
        button_remove->setGeometry(QRect(190, 230, 151, 25));
        line_seconds = new QLineEdit(deltaTimes);
        line_seconds->setObjectName(QString::fromUtf8("line_seconds"));
        line_seconds->setGeometry(QRect(20, 310, 191, 31));
        line_seconds->setFont(font);

        retranslateUi(deltaTimes);
        //QObject::connect(buttonBox, SIGNAL(accepted()), deltaTimes, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), deltaTimes, SLOT(reject()));

        QMetaObject::connectSlotsByName(deltaTimes);
    } // setupUi

    void retranslateUi(QDialog *deltaTimes)
    {
        deltaTimes->setWindowTitle(QCoreApplication::translate("deltaTimes",
            "Delta Time Steps", nullptr));
        label_module->setText(QCoreApplication::translate("deltaTimes",
            "Delta Times", nullptr));
        label_adjust->setText(QCoreApplication::translate("deltaTimes",
            "Set Delta Time", nullptr));
        button_save->setText(QCoreApplication::translate("deltaTimes",
            "Save", nullptr));
        button_cancel->setText(QCoreApplication::translate("deltaTimes",
            "Cancel", nullptr));
        button_add->setText(QCoreApplication::translate("deltaTimes",
            "Add New Entry", nullptr));
        button_remove->setText(QCoreApplication::translate("deltaTimes",
            "Remove Last Entry", nullptr));
    } // retranslateUi

};

namespace Ui {
    class deltaTimes: public Ui_deltaTimes {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_DELTATIMES_H
