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
#include <QtWidgets/QFrame>
#include <QtWidgets/QGridLayout>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QTextEdit>
#include <QtWidgets/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_meta
{
public:
    QGridLayout *gridLayout;
    QVBoxLayout *verticalLayout;
    QLabel *label_name;
    QLineEdit *line_name;
    QLabel *label_version;
    QLineEdit *line_version;
    QLabel *label_description;
    QTextEdit *text_description;
    QLabel *label_author;
    QLineEdit *line_author;
    QLabel *label_url;
    QLineEdit *line_url;
    QLabel *label_license;
    QLineEdit *line_license;
    QFrame *line;
    QHBoxLayout *hLay_bottom_buttonBox;
    QLabel *label_error;
    QDialogButtonBox *buttonBox;

    void setupUi(QDialog *meta)
    {
        if (meta->objectName().isEmpty())
            meta->setObjectName(QString::fromUtf8("meta"));
        meta->resize(400, 443);
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(meta->sizePolicy().hasHeightForWidth());
        meta->setSizePolicy(sizePolicy);
        meta->setMinimumSize(QSize(320, 400));
        meta->setMaximumSize(QSize(1000, 900));
        gridLayout = new QGridLayout(meta);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        verticalLayout = new QVBoxLayout();
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        verticalLayout->setSizeConstraint(QLayout::SetMaximumSize);
        label_name = new QLabel(meta);
        label_name->setObjectName(QString::fromUtf8("label_name"));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        font.setPointSize(12);
        label_name->setFont(font);

        verticalLayout->addWidget(label_name);

        line_name = new QLineEdit(meta);
        line_name->setObjectName(QString::fromUtf8("line_name"));
        line_name->setMinimumSize(QSize(300, 0));

        verticalLayout->addWidget(line_name);

        label_version = new QLabel(meta);
        label_version->setObjectName(QString::fromUtf8("label_version"));
        label_version->setFont(font);

        verticalLayout->addWidget(label_version);

        line_version = new QLineEdit(meta);
        line_version->setObjectName(QString::fromUtf8("line_version"));
        line_version->setMinimumSize(QSize(300, 0));

        verticalLayout->addWidget(line_version);

        label_description = new QLabel(meta);
        label_description->setObjectName(QString::fromUtf8("label_description"));
        label_description->setFont(font);

        verticalLayout->addWidget(label_description);

        text_description = new QTextEdit(meta);
        text_description->setObjectName(QString::fromUtf8("text_description"));
        text_description->setMinimumSize(QSize(300, 0));

        verticalLayout->addWidget(text_description);

        label_author = new QLabel(meta);
        label_author->setObjectName(QString::fromUtf8("label_author"));
        label_author->setFont(font);

        verticalLayout->addWidget(label_author);

        line_author = new QLineEdit(meta);
        line_author->setObjectName(QString::fromUtf8("line_author"));
        line_author->setMinimumSize(QSize(300, 0));

        verticalLayout->addWidget(line_author);

        label_url = new QLabel(meta);
        label_url->setObjectName(QString::fromUtf8("label_url"));
        label_url->setFont(font);

        verticalLayout->addWidget(label_url);

        line_url = new QLineEdit(meta);
        line_url->setObjectName(QString::fromUtf8("line_url"));
        line_url->setMinimumSize(QSize(300, 0));

        verticalLayout->addWidget(line_url);

        label_license = new QLabel(meta);
        label_license->setObjectName(QString::fromUtf8("label_license"));
        label_license->setFont(font);

        verticalLayout->addWidget(label_license);

        line_license = new QLineEdit(meta);
        line_license->setObjectName(QString::fromUtf8("line_license"));
        line_license->setMinimumSize(QSize(300, 0));

        verticalLayout->addWidget(line_license);

        line = new QFrame(meta);
        line->setObjectName(QString::fromUtf8("line"));
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(line);

        hLay_bottom_buttonBox = new QHBoxLayout();
        hLay_bottom_buttonBox->setObjectName(QString::fromUtf8("hLay_bottom_buttonBox"));

        QFont fontE;
        fontE.setFamily(QString::fromUtf8("Arial"));
        label_error = new QLabel(meta);
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

        buttonBox = new QDialogButtonBox(meta);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        hLay_bottom_buttonBox->addWidget(buttonBox);
        verticalLayout->addLayout(hLay_bottom_buttonBox);

        gridLayout->addLayout(verticalLayout, 0, 0, 1, 1);


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
        label_description->setText(QCoreApplication::translate("meta", "Description", nullptr));
        label_author->setText(QCoreApplication::translate("meta", "Author", nullptr));
        label_url->setText(QCoreApplication::translate("meta", "URL", nullptr));
        label_license->setText(QCoreApplication::translate("meta", "License", nullptr));
        line_license->setText(QString());
        label_error->setText(QCoreApplication::translate("meta", "", nullptr));
    } // retranslateUi

};

namespace Ui {
    class meta: public Ui_meta {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_META_H
