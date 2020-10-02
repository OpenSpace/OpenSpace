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
#include <QtWidgets/QFrame>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QListWidget>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_deltaTimes
{
public:
    QVBoxLayout *verticalLayout;
    QListWidget *listWidget;
    QHBoxLayout *horizontalLayout;
    QPushButton *button_add;
    QPushButton *button_remove;
    QSpacerItem *horizontalSpacer_2;
    QLabel *label_adjust;
    QLineEdit *line_seconds;
    QHBoxLayout *horizontalLayout_2;
    QPushButton *button_save;
    QPushButton *button_cancel;
    QSpacerItem *horizontalSpacer;
    QFrame *line;
    QHBoxLayout *hLay_bottom_buttonBox;
    QLabel *label_error;
    QLabel *label_value;
    QHBoxLayout *hLay_value;
    QDialogButtonBox *buttonBox;

    void setupUi(QDialog *deltaTimes)
    {
        if (deltaTimes->objectName().isEmpty())
            deltaTimes->setObjectName(QString::fromUtf8("deltaTimes"));
        deltaTimes->resize(531, 439);
        deltaTimes->setMinimumSize(QSize(350, 300));
        deltaTimes->setMaximumSize(QSize(550, 800));
        verticalLayout = new QVBoxLayout(deltaTimes);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        listWidget = new QListWidget(deltaTimes);
        listWidget->setObjectName(QString::fromUtf8("listWidget"));
        listWidget->setAutoScroll(true);
        listWidget->setLayoutMode(QListView::SinglePass);

        verticalLayout->addWidget(listWidget);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        button_add = new QPushButton(deltaTimes);
        button_add->setObjectName(QString::fromUtf8("button_add"));
        button_add->setMinimumSize(QSize(100, 0));
        button_add->setMaximumSize(QSize(120, 16777215));

        horizontalLayout->addWidget(button_add);

        button_remove = new QPushButton(deltaTimes);
        button_remove->setObjectName(QString::fromUtf8("button_remove"));
        button_remove->setMinimumSize(QSize(160, 0));
        button_remove->setMaximumSize(QSize(180, 16777215));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        button_remove->setFont(font);

        horizontalLayout->addWidget(button_remove);

        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding,
            QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer_2);


        verticalLayout->addLayout(horizontalLayout);

        label_adjust = new QLabel(deltaTimes);
        label_adjust->setObjectName(QString::fromUtf8("label_adjust"));
        label_adjust->setMinimumSize(QSize(200, 16777215));
        label_adjust->setMaximumSize(QSize(400, 16777215));
        QFont font1;
        font1.setFamily(QString::fromUtf8("Arial"));
        font1.setPointSize(12);
        label_adjust->setFont(font1);

        verticalLayout->addWidget(label_adjust);

        line_seconds = new QLineEdit(deltaTimes);
        line_seconds->setObjectName(QString::fromUtf8("line_seconds"));
        line_seconds->setMaximumSize(QSize(200, 16777215));
        line_seconds->setFont(font1);

        label_value = new QLabel(deltaTimes);
        label_value->setObjectName(QString::fromUtf8("label_value"));
        label_value->setMinimumSize(QSize(120, 20));
        label_value->setMaximumSize(QSize(300, 16777215));
        label_value->setFont(font1);

        hLay_value = new QHBoxLayout();
        hLay_value->setObjectName(QString::fromUtf8("hLay_value"));

        hLay_value->addWidget(line_seconds);
        hLay_value->addWidget(label_value);

        verticalLayout->addLayout(hLay_value);

        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        button_save = new QPushButton(deltaTimes);
        button_save->setObjectName(QString::fromUtf8("button_save"));
        button_save->setMinimumSize(QSize(80, 0));
        button_save->setMaximumSize(QSize(120, 16777215));
        button_save->setFont(font);

        horizontalLayout_2->addWidget(button_save);

        button_cancel = new QPushButton(deltaTimes);
        button_cancel->setObjectName(QString::fromUtf8("button_cancel"));
        button_cancel->setMinimumSize(QSize(80, 0));
        button_cancel->setMaximumSize(QSize(120, 16777215));

        horizontalLayout_2->addWidget(button_cancel);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding,
            QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer);


        verticalLayout->addLayout(horizontalLayout_2);

        line = new QFrame(deltaTimes);
        line->setObjectName(QString::fromUtf8("line"));
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(line);

        hLay_bottom_buttonBox = new QHBoxLayout();
        hLay_bottom_buttonBox->setObjectName(QString::fromUtf8("hLay_bottom_buttonBox"));

        QFont fontE;
        fontE.setFamily(QString::fromUtf8("Arial"));
        label_error = new QLabel(deltaTimes);
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

        buttonBox = new QDialogButtonBox(deltaTimes);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        hLay_bottom_buttonBox->addWidget(buttonBox);
        verticalLayout->addLayout(hLay_bottom_buttonBox);

        retranslateUi(deltaTimes);
        //QObject::connect(buttonBox, SIGNAL(accepted()), deltaTimes, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), deltaTimes, SLOT(reject()));

        QMetaObject::connectSlotsByName(deltaTimes);
    } // setupUi

    void retranslateUi(QDialog *deltaTimes)
    {
        deltaTimes->setWindowTitle(QCoreApplication::translate("deltaTimes",
            "Simulation Time Increments", nullptr));
        button_add->setText(QCoreApplication::translate("deltaTimes", "Add Entry",
            nullptr));
        button_remove->setText(QCoreApplication::translate("deltaTimes",
            "Remove Last Entry", nullptr));
        label_adjust->setText(QCoreApplication::translate("deltaTimes",
            "Set Simulation Time Increment for key", nullptr));
        label_value->setText(QCoreApplication::translate("deltaTimes", "", nullptr));
#if QT_CONFIG(tooltip)
        line_seconds->setToolTip(QCoreApplication::translate("deltaTimes",
            "<html><head/><body><p>Enter number of simulated seconds to elapse per "
            "actual second for the particular delta time</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        button_save->setText(QCoreApplication::translate("deltaTimes", "Save", nullptr));
        button_cancel->setText(QCoreApplication::translate("deltaTimes", "Cancel",
            nullptr));
        label_error->setText(QCoreApplication::translate("deltaTimes", "", nullptr));
    } // retranslateUi

};

namespace Ui {
    class deltaTimes: public Ui_deltaTimes {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_DELTATIMES_H
