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

#ifndef UI_ERRORDIALOG_H
#define UI_ERRORDIALOG_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QLabel>

QT_BEGIN_NAMESPACE

class Ui_errordialog
{
public:
    QDialogButtonBox *buttonBox;
    QLabel *label;

    void setupUi(QDialog *errordialog)
    {
        if (errordialog->objectName().isEmpty())
            errordialog->setObjectName(QString::fromUtf8("errordialog"));
        errordialog->resize(400, 181);
        buttonBox = new QDialogButtonBox(errordialog);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setGeometry(QRect(140, 120, 91, 32));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Ok);
        label = new QLabel(errordialog);
        label->setObjectName(QString::fromUtf8("label"));
        label->setGeometry(QRect(20, 10, 360, 100));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        font.setPointSize(12);
        label->setFont(font);
        label->setWordWrap(true);

        retranslateUi(errordialog);
        QObject::connect(buttonBox, SIGNAL(accepted()), errordialog, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), errordialog, SLOT(reject()));

        QMetaObject::connectSlotsByName(errordialog);
    } // setupUi

    void retranslateUi(QDialog *errordialog)
    {
        errordialog->setWindowTitle(QCoreApplication::translate("errordialog",
            "Profile Format Error", nullptr));
        label->setText(QCoreApplication::translate("errordialog", "TextLabel",
            nullptr));
    } // retranslateUi

};

namespace Ui {
    class errordialog: public Ui_errordialog {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_ERRORDIALOG_H
