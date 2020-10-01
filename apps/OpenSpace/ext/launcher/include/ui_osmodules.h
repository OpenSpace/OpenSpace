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

#ifndef UI_OSMODULES_H
#define UI_OSMODULES_H

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
#include <QtWidgets/QTextEdit>
#include <QtWidgets/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_osmodules
{
public:
    QVBoxLayout *verticalLayout;
    QListWidget *list;
    QHBoxLayout *horizontalLayout;
    QPushButton *button_add;
    QPushButton *button_remove;
    QSpacerItem *horizontalSpacer;
    QLabel *label_module;
    QLineEdit *line_module;
    QLabel *label_loaded;
    QTextEdit *line_loaded;
    QLabel *label_notLoaded;
    QTextEdit *line_notLoaded;
    QHBoxLayout *horizontalLayout_2;
    QPushButton *button_save;
    QPushButton *button_cancel;
    QSpacerItem *horizontalSpacer_2;
    QFrame *line;
    QHBoxLayout *hLay_bottom_buttonBox;
    QLabel *label_error;
    QDialogButtonBox *buttonBox;

    void setupUi(QDialog *osmodules)
    {
        if (osmodules->objectName().isEmpty())
            osmodules->setObjectName(QString::fromUtf8("osmodules"));
        osmodules->resize(689, 559);
        osmodules->setMinimumSize(QSize(400, 500));
        osmodules->setMaximumSize(QSize(1020, 900));
        verticalLayout = new QVBoxLayout(osmodules);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        list = new QListWidget(osmodules);
        list->setObjectName(QString::fromUtf8("list"));
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(list->sizePolicy().hasHeightForWidth());
        list->setSizePolicy(sizePolicy);
        list->setMinimumSize(QSize(0, 80));
        list->setMaximumSize(QSize(1000, 800));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        list->setFont(font);
        list->setAlternatingRowColors(true);
        list->setMovement(QListView::Free);
        list->setResizeMode(QListView::Adjust);

        verticalLayout->addWidget(list);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        button_add = new QPushButton(osmodules);
        button_add->setObjectName(QString::fromUtf8("button_add"));
        button_add->setMinimumSize(QSize(60, 0));
        button_add->setMaximumSize(QSize(100, 16777215));

        horizontalLayout->addWidget(button_add);

        button_remove = new QPushButton(osmodules);
        button_remove->setObjectName(QString::fromUtf8("button_remove"));
        button_remove->setMinimumSize(QSize(60, 0));
        button_remove->setMaximumSize(QSize(100, 16777215));

        horizontalLayout->addWidget(button_remove);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);


        verticalLayout->addLayout(horizontalLayout);

        label_module = new QLabel(osmodules);
        label_module->setObjectName(QString::fromUtf8("label_module"));
        QFont font1;
        font1.setFamily(QString::fromUtf8("Arial"));
        font1.setPointSize(12);
        label_module->setFont(font1);

        verticalLayout->addWidget(label_module);

        line_module = new QLineEdit(osmodules);
        line_module->setObjectName(QString::fromUtf8("line_module"));
        line_module->setMinimumSize(QSize(160, 0));
        line_module->setMaximumSize(QSize(400, 16777215));

        verticalLayout->addWidget(line_module);

        label_loaded = new QLabel(osmodules);
        label_loaded->setObjectName(QString::fromUtf8("label_loaded"));
        label_loaded->setFont(font1);

        verticalLayout->addWidget(label_loaded);

        line_loaded = new QTextEdit(osmodules);
        line_loaded->setObjectName(QString::fromUtf8("line_loaded"));
        line_loaded->setMinimumSize(QSize(160, 80));
        line_loaded->setMaximumSize(QSize(650, 450));

        verticalLayout->addWidget(line_loaded);

        label_notLoaded = new QLabel(osmodules);
        label_notLoaded->setObjectName(QString::fromUtf8("label_notLoaded"));
        label_notLoaded->setFont(font1);

        verticalLayout->addWidget(label_notLoaded);

        line_notLoaded = new QTextEdit(osmodules);
        line_notLoaded->setObjectName(QString::fromUtf8("line_notLoaded"));
        line_notLoaded->setMinimumSize(QSize(160, 80));
        line_notLoaded->setMaximumSize(QSize(650, 450));

        verticalLayout->addWidget(line_notLoaded);

        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        button_save = new QPushButton(osmodules);
        button_save->setObjectName(QString::fromUtf8("button_save"));
        button_save->setMinimumSize(QSize(60, 0));
        button_save->setMaximumSize(QSize(100, 16777215));

        horizontalLayout_2->addWidget(button_save);

        button_cancel = new QPushButton(osmodules);
        button_cancel->setObjectName(QString::fromUtf8("button_cancel"));
        button_cancel->setMinimumSize(QSize(60, 0));
        button_cancel->setMaximumSize(QSize(100, 16777215));

        horizontalLayout_2->addWidget(button_cancel);

        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_2);


        verticalLayout->addLayout(horizontalLayout_2);

        line = new QFrame(osmodules);
        line->setObjectName(QString::fromUtf8("line"));
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(line);

        hLay_bottom_buttonBox = new QHBoxLayout();
        hLay_bottom_buttonBox->setObjectName(QString::fromUtf8("hLay_bottom_buttonBox"));

        QFont fontE;
        fontE.setFamily(QString::fromUtf8("Arial"));
        label_error = new QLabel(osmodules);
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

        buttonBox = new QDialogButtonBox(osmodules);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        hLay_bottom_buttonBox->addWidget(buttonBox);
        verticalLayout->addLayout(hLay_bottom_buttonBox);

        retranslateUi(osmodules);
        //QObject::connect(buttonBox, SIGNAL(accepted()), osmodules, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), osmodules, SLOT(reject()));

        QMetaObject::connectSlotsByName(osmodules);
    } // setupUi

    void retranslateUi(QDialog *osmodules)
    {
        osmodules->setWindowTitle(QCoreApplication::translate("osmodules", "Modules", nullptr));
        button_add->setText(QCoreApplication::translate("osmodules", "Add New", nullptr));
        button_remove->setText(QCoreApplication::translate("osmodules", "Remove", nullptr));
        label_module->setText(QCoreApplication::translate("osmodules", "Module", nullptr));
#if QT_CONFIG(tooltip)
        line_module->setToolTip(QCoreApplication::translate("osmodules", "<html><head/><body><p>Name of OpenSpace module related to this profile</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_loaded->setText(QCoreApplication::translate("osmodules", "Command if Module is Loaded", nullptr));
#if QT_CONFIG(tooltip)
        line_loaded->setToolTip(QCoreApplication::translate("osmodules", "<html><head/><body><p>[OPTIONAL] Lua command(s) to execute if the module is present in the OpenSpace application</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_notLoaded->setText(QCoreApplication::translate("osmodules", "Command if Module is NOT Loaded", nullptr));
#if QT_CONFIG(tooltip)
        line_notLoaded->setToolTip(QCoreApplication::translate("osmodules", "<html><head/><body><p>[OPTIONAL] Lua command(s) to execute if the module is <span style=\" font-style:italic;\">not </span>present in the OpenSpace application (for example steps to account for a missing module)</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
#if QT_CONFIG(tooltip)
        button_save->setToolTip(QCoreApplication::translate("osmodules", "<html><head/><body><p>Save module changes to the above list</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        button_save->setText(QCoreApplication::translate("osmodules", "Save", nullptr));
#if QT_CONFIG(tooltip)
        button_cancel->setToolTip(QCoreApplication::translate("osmodules", "<html><head/><body><p>Cancel adding this module to the above list</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        button_cancel->setText(QCoreApplication::translate("osmodules", "Cancel", nullptr));
        label_error->setText(QCoreApplication::translate("osmodules", "", nullptr));
    } // retranslateUi

};

namespace Ui {
    class osmodules: public Ui_osmodules {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_OSMODULES_H
