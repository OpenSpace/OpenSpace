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

#ifndef UI_ADDEDSCRIPTS_H
#define UI_ADDEDSCRIPTS_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QLabel>
#include <QtWidgets/QTextEdit>

QT_BEGIN_NAMESPACE

class Ui_addedScripts
{
public:
    QDialogButtonBox *buttonBox;
    QLabel *label_module;
    QTextEdit *text_scripts;

    void setupUi(QDialog *addedScripts)
    {
        if (addedScripts->objectName().isEmpty())
            addedScripts->setObjectName(QString::fromUtf8("addedScripts"));
        addedScripts->resize(591, 469);
        buttonBox = new QDialogButtonBox(addedScripts);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setGeometry(QRect(240, 430, 341, 32));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);
        label_module = new QLabel(addedScripts);
        label_module->setObjectName(QString::fromUtf8("label_module"));
        label_module->setGeometry(QRect(20, 20, 357, 20));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        font.setPointSize(12);
        label_module->setFont(font);
        text_scripts = new QTextEdit(addedScripts);
        text_scripts->setObjectName(QString::fromUtf8("text_scripts"));
        text_scripts->setGeometry(QRect(20, 50, 551, 371));

        retranslateUi(addedScripts);
        QObject::connect(buttonBox, SIGNAL(accepted()), addedScripts, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), addedScripts, SLOT(reject()));

        QMetaObject::connectSlotsByName(addedScripts);
    } // setupUi

    void retranslateUi(QDialog *addedScripts)
    {
        addedScripts->setWindowTitle(QCoreApplication::translate("addedScripts",
            "Additional Scripts", nullptr));
        label_module->setText(QCoreApplication::translate("addedScripts",
            "Additional Lua Scripts for Configuration", nullptr));
    } // retranslateUi

};

namespace Ui {
    class addedScripts: public Ui_addedScripts {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_ADDEDSCRIPTS_H
