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
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QListWidget>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QTextEdit>

QT_BEGIN_NAMESPACE

class Ui_osmodules
{
public:
    QDialogButtonBox *buttonBox;
    QListWidget *list;
    QPushButton *button_add;
    QPushButton *button_remove;
    QFrame *frame;
    QLabel *label_module;
    QLabel *label_loaded;
    QLabel *label_notLoaded;
    QLineEdit *line_module;
    QPushButton *button_cancel;
    QPushButton *button_save;
    QTextEdit *line_loaded;
    QTextEdit *line_notLoaded;

    void setupUi(QDialog *osmodules)
    {
        if (osmodules->objectName().isEmpty())
            osmodules->setObjectName(QString::fromUtf8("osmodules"));
        osmodules->resize(591, 559);
        buttonBox = new QDialogButtonBox(osmodules);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setGeometry(QRect(240, 520, 341, 32));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);
        list = new QListWidget(osmodules);
        list->setObjectName(QString::fromUtf8("list"));
        list->setGeometry(QRect(10, 10, 571, 120));
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(list->sizePolicy().hasHeightForWidth());
        list->setSizePolicy(sizePolicy);
        list->setMinimumSize(QSize(0, 50));
        list->setMaximumSize(QSize(16777215, 120));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        list->setFont(font);
        list->setAlternatingRowColors(true);
        list->setMovement(QListView::Free);
        list->setResizeMode(QListView::Adjust);
        button_add = new QPushButton(osmodules);
        button_add->setObjectName(QString::fromUtf8("button_add"));
        button_add->setGeometry(QRect(10, 140, 111, 25));
        button_remove = new QPushButton(osmodules);
        button_remove->setObjectName(QString::fromUtf8("button_remove"));
        button_remove->setGeometry(QRect(160, 140, 111, 25));
        frame = new QFrame(osmodules);
        frame->setObjectName(QString::fromUtf8("frame"));
        frame->setGeometry(QRect(10, 190, 571, 311));
        frame->setFrameShape(QFrame::StyledPanel);
        frame->setFrameShadow(QFrame::Raised);
        label_module = new QLabel(frame);
        label_module->setObjectName(QString::fromUtf8("label_module"));
        label_module->setGeometry(QRect(10, 10, 357, 20));
        QFont font1;
        font1.setFamily(QString::fromUtf8("Arial"));
        font1.setPointSize(12);
        label_module->setFont(font1);
        label_loaded = new QLabel(frame);
        label_loaded->setObjectName(QString::fromUtf8("label_loaded"));
        label_loaded->setGeometry(QRect(10, 70, 357, 20));
        label_loaded->setFont(font1);
        label_notLoaded = new QLabel(frame);
        label_notLoaded->setObjectName(QString::fromUtf8("label_notLoaded"));
        label_notLoaded->setGeometry(QRect(10, 170, 357, 20));
        label_notLoaded->setFont(font1);
        line_module = new QLineEdit(frame);
        line_module->setObjectName(QString::fromUtf8("line_module"));
        line_module->setGeometry(QRect(10, 30, 357, 25));
        line_loaded = new QTextEdit(frame);
        line_loaded->setObjectName(QString::fromUtf8("line_loaded"));
        line_loaded->setGeometry(QRect(10, 90, 511, 71));
        line_notLoaded = new QTextEdit(frame);
        line_notLoaded->setObjectName(QString::fromUtf8("line_notLoaded"));
        line_notLoaded->setGeometry(QRect(10, 190, 511, 71));
        button_cancel = new QPushButton(frame);
        button_cancel->setObjectName(QString::fromUtf8("button_cancel"));
        button_cancel->setGeometry(QRect(410, 270, 71, 25));
        button_save = new QPushButton(frame);
        button_save->setObjectName(QString::fromUtf8("button_save"));
        button_save->setGeometry(QRect(490, 270, 71, 25));
        label_module->raise();
        label_loaded->raise();
        label_notLoaded->raise();
        button_save->raise();
        button_cancel->raise();
        line_module->raise();
        line_loaded->raise();
        line_notLoaded->raise();

        retranslateUi(osmodules);
        //QObject::connect(buttonBox, SIGNAL(accepted()), osmodules, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), osmodules, SLOT(reject()));

        QMetaObject::connectSlotsByName(osmodules);
    } // setupUi

    void retranslateUi(QDialog *osmodules)
    {
        osmodules->setWindowTitle(QCoreApplication::translate("osmodules", "Modules",
            nullptr));
        button_add->setText(QCoreApplication::translate("osmodules", "Add New",
            nullptr));
        button_remove->setText(QCoreApplication::translate("osmodules", "Remove",
            nullptr));
        label_module->setText(QCoreApplication::translate("osmodules", "Module",
            nullptr));
        label_loaded->setText(QCoreApplication::translate("osmodules",
            "Command if Module is Loaded", nullptr));
        label_notLoaded->setText(QCoreApplication::translate("osmodules",
            "Command if Module is NOT Loaded", nullptr));
#if QT_CONFIG(tooltip)
        line_module->setToolTip(QCoreApplication::translate("osmodules", "<html><head/>"
            "<body><p>Name of OpenSpace module related to this profile</p></body></html>",
            nullptr));
#endif // QT_CONFIG(tooltip)
#if QT_CONFIG(tooltip)
        button_cancel->setToolTip(QCoreApplication::translate("osmodules", "<html><head/>"
            "<body><p>Cancel adding this module to the above list</p></body></html>",
            nullptr));
#endif // QT_CONFIG(tooltip)
        button_cancel->setText(QCoreApplication::translate("osmodules", "Cancel",
            nullptr));
#if QT_CONFIG(tooltip)
        button_save->setToolTip(QCoreApplication::translate("osmodules", "<html><head/>"
            "<body><p>Save module changes to the above list</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        button_save->setText(QCoreApplication::translate("osmodules", "Save", nullptr));
#if QT_CONFIG(tooltip)
        line_loaded->setToolTip(QCoreApplication::translate("osmodules", "<html><head/>"
            "<body><p>[OPTIONAL] Lua command(s) to execute if the module is present in "
            "the OpenSpace application</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
#if QT_CONFIG(tooltip)
        line_notLoaded->setToolTip(QCoreApplication::translate("osmodules", "<html>"
            "<head/><body><p>[OPTIONAL] Lua command(s) to execute if the module is "
            "<span style=\" font-style:italic;\">not </span>present in the OpenSpace "
            "application (for example steps to account for a missing module)</p></body>"
            "</html>", nullptr));
#endif // QT_CONFIG(tooltip)
    } // retranslateUi

};

namespace Ui {
    class osmodules: public Ui_osmodules {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_OSMODULES_H
