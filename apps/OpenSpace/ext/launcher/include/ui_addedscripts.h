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
#include <QtWidgets/QFrame>
#include <QtWidgets/QLabel>
#include <QtWidgets/QTextEdit>
#include <QtWidgets/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_addedScripts
{
public:
    QVBoxLayout *verticalLayout;
    QLabel *label_module;
    QTextEdit *text_scripts;
    QFrame *line;
    QHBoxLayout *hLay_bottom_buttonBox;
    QLabel *label_error;
    QDialogButtonBox *buttonBox;

    void setupUi(QDialog *addedScripts)
    {
        if (addedScripts->objectName().isEmpty())
            addedScripts->setObjectName(QString::fromUtf8("addedScripts"));
        addedScripts->resize(591, 469);
        addedScripts->setMinimumSize(QSize(400, 400));
        addedScripts->setMaximumSize(QSize(900, 900));
        verticalLayout = new QVBoxLayout(addedScripts);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        label_module = new QLabel(addedScripts);
        label_module->setObjectName(QString::fromUtf8("label_module"));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        font.setPointSize(12);
        label_module->setFont(font);

        verticalLayout->addWidget(label_module);

        text_scripts = new QTextEdit(addedScripts);
        text_scripts->setObjectName(QString::fromUtf8("text_scripts"));

        verticalLayout->addWidget(text_scripts);

        line = new QFrame(addedScripts);
        line->setObjectName(QString::fromUtf8("line"));
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(line);

        hLay_bottom_buttonBox = new QHBoxLayout();
        hLay_bottom_buttonBox->setObjectName(QString::fromUtf8("hLay_bottom_buttonBox"));

        QFont fontE;
        fontE.setFamily(QString::fromUtf8("Arial"));
        label_error = new QLabel(addedScripts);
        label_error->setObjectName(QString::fromUtf8("label_error"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(label_error->sizePolicy().hasHeightForWidth());
        label_error->setSizePolicy(sizePolicy1);
        label_error->setMinimumSize(QSize(200, 40));
        label_error->setMaximumSize(QSize(800, 40));
        label_error->setFont(fontE);
        label_error->setWordWrap(true);
        hLay_bottom_buttonBox->addWidget(label_error);

        buttonBox = new QDialogButtonBox(addedScripts);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        hLay_bottom_buttonBox->addWidget(buttonBox);
        verticalLayout->addLayout(hLay_bottom_buttonBox);


        retranslateUi(addedScripts);
        //QObject::connect(buttonBox, SIGNAL(accepted()), addedScripts, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), addedScripts, SLOT(reject()));

        QMetaObject::connectSlotsByName(addedScripts);
    } // setupUi

    void retranslateUi(QDialog *addedScripts)
    {
        addedScripts->setWindowTitle(QCoreApplication::translate("addedScripts", "Additional Scripts", nullptr));
        label_module->setText(QCoreApplication::translate("addedScripts", "Additional Lua Scripts for Configuration", nullptr));
        label_error->setText(QCoreApplication::translate("addedScripts", "", nullptr));
    } // retranslateUi

};

namespace Ui {
    class addedScripts: public Ui_addedScripts {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_ADDEDSCRIPTS_H
