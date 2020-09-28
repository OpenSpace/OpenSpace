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

#ifndef UI_META_H
#define UI_META_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QTextEdit>

QT_BEGIN_NAMESPACE

class Ui_meta
{
public:
    QDialogButtonBox *buttonBox;
    QLabel *label_name;
    QLineEdit *line_name;
    QLabel *label_version;
    QLineEdit *line_version;
    QLabel *label_description;
    QTextEdit *text_description;
    QLabel *label_author;
    QLineEdit *line_author;
    QLineEdit *line_url;
    QLabel *label_url;
    QLineEdit *line_license;
    QLabel *label_license;

    void setupUi(QDialog *meta)
    {
        if (meta->objectName().isEmpty())
            meta->setObjectName(QString::fromUtf8("meta"));
        meta->resize(400, 443);
        buttonBox = new QDialogButtonBox(meta);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setGeometry(QRect(40, 390, 341, 32));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);
        label_name = new QLabel(meta);
        label_name->setObjectName(QString::fromUtf8("label_name"));
        label_name->setGeometry(QRect(20, 20, 67, 17));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        font.setPointSize(12);
        label_name->setFont(font);
        line_name = new QLineEdit(meta);
        line_name->setObjectName(QString::fromUtf8("line_name"));
        line_name->setGeometry(QRect(20, 40, 331, 25));
        label_version = new QLabel(meta);
        label_version->setObjectName(QString::fromUtf8("label_version"));
        label_version->setGeometry(QRect(20, 70, 67, 17));
        label_version->setFont(font);
        line_version = new QLineEdit(meta);
        line_version->setObjectName(QString::fromUtf8("line_version"));
        line_version->setGeometry(QRect(20, 90, 331, 25));
        label_description = new QLabel(meta);
        label_description->setObjectName(QString::fromUtf8("label_description"));
        label_description->setGeometry(QRect(20, 120, 91, 17));
        label_description->setFont(font);
        text_description = new QTextEdit(meta);
        text_description->setObjectName(QString::fromUtf8("text_description"));
        text_description->setGeometry(QRect(20, 140, 331, 75));
        label_author = new QLabel(meta);
        label_author->setObjectName(QString::fromUtf8("label_author"));
        label_author->setGeometry(QRect(20, 220, 67, 17));
        label_author->setFont(font);
        line_author = new QLineEdit(meta);
        line_author->setObjectName(QString::fromUtf8("line_author"));
        line_author->setGeometry(QRect(20, 240, 331, 25));
        line_url = new QLineEdit(meta);
        line_url->setObjectName(QString::fromUtf8("line_url"));
        line_url->setGeometry(QRect(20, 290, 331, 25));
        label_url = new QLabel(meta);
        label_url->setObjectName(QString::fromUtf8("label_url"));
        label_url->setGeometry(QRect(20, 270, 67, 17));
        label_url->setFont(font);
        line_license = new QLineEdit(meta);
        line_license->setObjectName(QString::fromUtf8("line_license"));
        line_license->setGeometry(QRect(20, 340, 331, 25));
        label_license = new QLabel(meta);
        label_license->setObjectName(QString::fromUtf8("label_license"));
        label_license->setGeometry(QRect(20, 320, 67, 17));
        label_license->setFont(font);

        retranslateUi(meta);
        //QObject::connect(buttonBox, SIGNAL(accepted()), meta, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), meta, SLOT(reject()));

        QMetaObject::connectSlotsByName(meta);
    } // setupUi

    void retranslateUi(QDialog *meta)
    {
        meta->setWindowTitle(QCoreApplication::translate("meta", "Meta", nullptr));
        label_name->setText(QCoreApplication::translate("meta", "Name", nullptr));
        label_version->setText(QCoreApplication::translate("meta", "Version", nullptr));
        label_description->setText(QCoreApplication::translate("meta", "Description",
            nullptr));
        label_author->setText(QCoreApplication::translate("meta", "Author", nullptr));
        label_url->setText(QCoreApplication::translate("meta", "URL", nullptr));
        line_license->setText(QString());
        label_license->setText(QCoreApplication::translate("meta", "License", nullptr));
    } // retranslateUi

};

namespace Ui {
    class meta: public Ui_meta {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_META_H
