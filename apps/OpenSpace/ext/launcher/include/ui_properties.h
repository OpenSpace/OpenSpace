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

#ifndef UI_PROPERTIES_H
#define UI_PROPERTIES_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QComboBox>
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

class Ui_properties
{
public:
    QVBoxLayout *verticalLayout;
    QListWidget *list;
    QHBoxLayout *horizontalLayout;
    QPushButton *button_add;
    QPushButton *button_remove;
    QSpacerItem *horizontalSpacer;
    QFrame *line;
    QLabel *label_command;
    QComboBox *combo_command;
    QLabel *label_property;
    QLineEdit *line_property;
    QLabel *label_value;
    QLineEdit *line_value;
    QHBoxLayout *horizontalLayout_2;
    QPushButton *button_save;
    QPushButton *button_cancel;
    QSpacerItem *horizontalSpacer_2;
    QFrame *line_2;
    QHBoxLayout *hLay_bottom_buttonBox;
    QLabel *label_error;
    QDialogButtonBox *buttonBox;

    void setupUi(QDialog *properties)
    {
        if (properties->objectName().isEmpty())
            properties->setObjectName(QString::fromUtf8("properties"));
        properties->resize(591, 448);
        properties->setMaximumSize(QSize(1600, 1000));
        verticalLayout = new QVBoxLayout(properties);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        list = new QListWidget(properties);
        list->setObjectName(QString::fromUtf8("list"));
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(list->sizePolicy().hasHeightForWidth());
        list->setSizePolicy(sizePolicy);
        list->setMinimumSize(QSize(0, 150));
        list->setMaximumSize(QSize(1600, 800));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        list->setFont(font);
        list->setAlternatingRowColors(true);
        list->setMovement(QListView::Free);
        list->setResizeMode(QListView::Adjust);

        verticalLayout->addWidget(list);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        button_add = new QPushButton(properties);
        button_add->setObjectName(QString::fromUtf8("button_add"));
        button_add->setMinimumSize(QSize(90, 0));
        button_add->setMaximumSize(QSize(110, 16777215));

        horizontalLayout->addWidget(button_add);

        button_remove = new QPushButton(properties);
        button_remove->setObjectName(QString::fromUtf8("button_remove"));
        button_remove->setMinimumSize(QSize(90, 0));
        button_remove->setMaximumSize(QSize(110, 16777215));

        horizontalLayout->addWidget(button_remove);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);


        verticalLayout->addLayout(horizontalLayout);

        line = new QFrame(properties);
        line->setObjectName(QString::fromUtf8("line"));
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(line);

        label_command = new QLabel(properties);
        label_command->setObjectName(QString::fromUtf8("label_command"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(label_command->sizePolicy().hasHeightForWidth());
        label_command->setSizePolicy(sizePolicy1);
        QFont font1;
        font1.setFamily(QString::fromUtf8("Arial"));
        font1.setPointSize(12);
        label_command->setFont(font1);

        verticalLayout->addWidget(label_command);

        combo_command = new QComboBox(properties);
        combo_command->setObjectName(QString::fromUtf8("combo_command"));
        QSizePolicy sizePolicy2(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(combo_command->sizePolicy().hasHeightForWidth());
        combo_command->setSizePolicy(sizePolicy2);
        combo_command->setMinimumSize(QSize(200, 0));
        combo_command->setMaximumSize(QSize(300, 16777215));
        combo_command->setFont(font);

        verticalLayout->addWidget(combo_command);

        label_property = new QLabel(properties);
        label_property->setObjectName(QString::fromUtf8("label_property"));
        sizePolicy1.setHeightForWidth(label_property->sizePolicy().hasHeightForWidth());
        label_property->setSizePolicy(sizePolicy1);
        label_property->setMinimumSize(QSize(100, 0));
        label_property->setMaximumSize(QSize(140, 16777215));
        label_property->setFont(font1);

        verticalLayout->addWidget(label_property);

        line_property = new QLineEdit(properties);
        line_property->setObjectName(QString::fromUtf8("line_property"));
        line_property->setMinimumSize(QSize(350, 0));
        line_property->setMaximumSize(QSize(1600, 16777215));

        verticalLayout->addWidget(line_property);

        label_value = new QLabel(properties);
        label_value->setObjectName(QString::fromUtf8("label_value"));
        sizePolicy1.setHeightForWidth(label_value->sizePolicy().hasHeightForWidth());
        label_value->setSizePolicy(sizePolicy1);
        label_value->setMinimumSize(QSize(100, 0));
        label_value->setMaximumSize(QSize(140, 16777215));
        label_value->setFont(font1);

        verticalLayout->addWidget(label_value);

        line_value = new QLineEdit(properties);
        line_value->setObjectName(QString::fromUtf8("line_value"));
        line_value->setMinimumSize(QSize(350, 0));
        line_value->setMaximumSize(QSize(600, 16777215));

        verticalLayout->addWidget(line_value);

        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        button_save = new QPushButton(properties);
        button_save->setObjectName(QString::fromUtf8("button_save"));
        button_save->setMinimumSize(QSize(90, 0));
        button_save->setMaximumSize(QSize(110, 16777215));

        horizontalLayout_2->addWidget(button_save);

        button_cancel = new QPushButton(properties);
        button_cancel->setObjectName(QString::fromUtf8("button_cancel"));
        button_cancel->setMinimumSize(QSize(90, 0));
        button_cancel->setMaximumSize(QSize(110, 16777215));

        horizontalLayout_2->addWidget(button_cancel);

        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_2);


        verticalLayout->addLayout(horizontalLayout_2);

        line_2 = new QFrame(properties);
        line_2->setObjectName(QString::fromUtf8("line_2"));
        line_2->setFrameShape(QFrame::HLine);
        line_2->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(line_2);

        hLay_bottom_buttonBox = new QHBoxLayout();
        hLay_bottom_buttonBox->setObjectName(QString::fromUtf8("hLay_bottom_buttonBox"));

        QFont fontE;
        fontE.setFamily(QString::fromUtf8("Arial"));
        label_error = new QLabel(properties);
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

        buttonBox = new QDialogButtonBox(properties);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        hLay_bottom_buttonBox->addWidget(buttonBox);
        verticalLayout->addLayout(hLay_bottom_buttonBox);


        retranslateUi(properties);
        //QObject::connect(buttonBox, SIGNAL(accepted()), properties, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), properties, SLOT(reject()));

        QMetaObject::connectSlotsByName(properties);
    } // setupUi

    void retranslateUi(QDialog *properties)
    {
        properties->setWindowTitle(QCoreApplication::translate("properties", "Set Property Values", nullptr));
        button_add->setText(QCoreApplication::translate("properties", "Add New", nullptr));
        button_remove->setText(QCoreApplication::translate("properties", "Remove", nullptr));
        label_command->setText(QCoreApplication::translate("properties", "Property Set Command", nullptr));
        label_property->setText(QCoreApplication::translate("properties", "Property", nullptr));
#if QT_CONFIG(tooltip)
        line_property->setToolTip(QCoreApplication::translate("properties", "<html><head/><body><p>Exact string is required for the property to be set</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_value->setText(QCoreApplication::translate("properties", "Value to Set", nullptr));
        button_save->setText(QCoreApplication::translate("properties", "Save", nullptr));
        button_cancel->setText(QCoreApplication::translate("properties", "Cancel", nullptr));
        label_error->setText(QCoreApplication::translate("properties", "", nullptr));
    } // retranslateUi

};

namespace Ui {
    class properties: public Ui_properties {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_PROPERTIES_H
