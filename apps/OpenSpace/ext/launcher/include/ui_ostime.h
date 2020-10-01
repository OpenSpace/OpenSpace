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

#ifndef UI_OSTIME_H
#define UI_OSTIME_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QDateTimeEdit>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QFrame>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_time
{
public:
    QVBoxLayout *verticalLayout;
    QLabel *label_type;
    QComboBox *combo_type;
    QLabel *label_absolete;
    QDateTimeEdit *dateTimeEdit;
    QLabel *label_relative;
    QLineEdit *line_relative;
    QFrame *line;
    QHBoxLayout *hLay_bottom_buttonBox;
    QLabel *label_error;
    QDialogButtonBox *buttonBox;

    void setupUi(QDialog *time)
    {
        if (time->objectName().isEmpty())
            time->setObjectName(QString::fromUtf8("time"));
        time->resize(340, 260);
        time->setMinimumSize(QSize(340, 260));
        time->setMaximumSize(QSize(520, 260));
        verticalLayout = new QVBoxLayout(time);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        label_type = new QLabel(time);
        label_type->setObjectName(QString::fromUtf8("label_type"));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(label_type->sizePolicy().hasHeightForWidth());
        label_type->setSizePolicy(sizePolicy);
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        font.setPointSize(12);
        label_type->setFont(font);

        verticalLayout->addWidget(label_type);

        combo_type = new QComboBox(time);
        combo_type->setObjectName(QString::fromUtf8("combo_type"));
        combo_type->setMinimumSize(QSize(180, 0));
        combo_type->setMaximumSize(QSize(220, 16777215));

        verticalLayout->addWidget(combo_type);

        label_absolete = new QLabel(time);
        label_absolete->setObjectName(QString::fromUtf8("label_absolete"));
        sizePolicy.setHeightForWidth(label_absolete->sizePolicy().hasHeightForWidth());
        label_absolete->setSizePolicy(sizePolicy);
        label_absolete->setFont(font);

        verticalLayout->addWidget(label_absolete);

        dateTimeEdit = new QDateTimeEdit(time);
        dateTimeEdit->setObjectName(QString::fromUtf8("dateTimeEdit"));
        dateTimeEdit->setMinimumSize(QSize(180, 0));
        dateTimeEdit->setMaximumSize(QSize(220, 16777215));
        dateTimeEdit->setFont(font);

        verticalLayout->addWidget(dateTimeEdit);

        label_relative = new QLabel(time);
        label_relative->setObjectName(QString::fromUtf8("label_relative"));
        sizePolicy.setHeightForWidth(label_relative->sizePolicy().hasHeightForWidth());
        label_relative->setSizePolicy(sizePolicy);
        label_relative->setFont(font);

        verticalLayout->addWidget(label_relative);

        line_relative = new QLineEdit(time);
        line_relative->setObjectName(QString::fromUtf8("line_relative"));
        line_relative->setMaximumSize(QSize(440, 16777215));
        line_relative->setFont(font);

        verticalLayout->addWidget(line_relative);

        line = new QFrame(time);
        line->setObjectName(QString::fromUtf8("line"));
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(line);

        hLay_bottom_buttonBox = new QHBoxLayout();
        hLay_bottom_buttonBox->setObjectName(QString::fromUtf8("hLay_bottom_buttonBox"));

        QFont fontE;
        fontE.setFamily(QString::fromUtf8("Arial"));
        label_error = new QLabel(time);
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

        buttonBox = new QDialogButtonBox(time);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        verticalLayout->addWidget(buttonBox);

        hLay_bottom_buttonBox->addWidget(buttonBox);
        verticalLayout->addLayout(hLay_bottom_buttonBox);

        retranslateUi(time);
        //QObject::connect(buttonBox, SIGNAL(accepted()), time, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), time, SLOT(reject()));

        QMetaObject::connectSlotsByName(time);
    } // setupUi

    void retranslateUi(QDialog *time)
    {
        time->setWindowTitle(QCoreApplication::translate("time", "Time", nullptr));
        label_type->setText(QCoreApplication::translate("time", "Time Type", nullptr));
#if QT_CONFIG(tooltip)
        combo_type->setToolTip(QCoreApplication::translate("time", "<html><head/><body><p>Types: Absolute defined time or Relative to actual time</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_absolete->setText(QCoreApplication::translate("time", "Absolute UTC:", nullptr));
        dateTimeEdit->setDisplayFormat(QCoreApplication::translate("time", "yyyy-MM-dd  T  hh:mm:ss", nullptr));
        label_relative->setText(QCoreApplication::translate("time", "Relative Time:", nullptr));
#if QT_CONFIG(tooltip)
        line_relative->setToolTip(QCoreApplication::translate("time", "<html><head/><body><p>String for relative time to actual (e.g. &quot;-1d&quot; for back 1 day)</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_error->setText(QCoreApplication::translate("time", "", nullptr));
    } // retranslateUi

};

namespace Ui {
    class time: public Ui_time {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_OSTIME_H
