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

#ifndef __OPENSPACE_UI_LAUNCHER___UI_KEYBINDINGS___H__
#define __OPENSPACE_UI_LAUNCHER___UI_KEYBINDINGS___H__

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QCheckBox>
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
#include <QtWidgets/QTextEdit>
#include <QtWidgets/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_keybindings
{
public:
    QVBoxLayout *verticalLayout;
    QListWidget *list;
    QHBoxLayout *horizontalLayout;
    QPushButton *button_add;
    QPushButton *button_remove;
    QSpacerItem *horizontalSpacer_3;
    QFrame *line;
    QHBoxLayout *horizontalLayout_2;
    QVBoxLayout *verticalLayout_2;
    QLabel *label_keyMod;
    QComboBox *combo_keyMod;
    QVBoxLayout *verticalLayout_3;
    QLabel *label_key;
    QComboBox *combo_key;
    QSpacerItem *horizontalSpacer;
    QLabel *label_name;
    QLineEdit *line_name;
    QLabel *label_guiPath;
    QLineEdit *line_guiPath;
    QLabel *label_documentation;
    QLineEdit *line_documentation;
    QCheckBox *checkBox_local;
    QLabel *label_script;
    QTextEdit *text_script;
    QHBoxLayout *horizontalLayout_3;
    QPushButton *button_save;
    QPushButton *button_cancel;
    QSpacerItem *horizontalSpacer_2;
    QFrame *line_2;
    QHBoxLayout *hLay_bottom_buttonBox;
    QLabel *label_error;
    QDialogButtonBox *buttonBox;

    void setupUi(QDialog *keybindings)
    {
        if (keybindings->objectName().isEmpty())
            keybindings->setObjectName(QString::fromUtf8("keybindings"));
        keybindings->resize(660, 700);
        keybindings->setMinimumSize(QSize(400, 600));
        keybindings->setMaximumSize(QSize(1600, 950));
        verticalLayout = new QVBoxLayout(keybindings);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        list = new QListWidget(keybindings);
        list->setObjectName(QString::fromUtf8("list"));
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(list->sizePolicy().hasHeightForWidth());
        list->setSizePolicy(sizePolicy);
        list->setMinimumSize(QSize(0, 80));
        list->setMaximumSize(QSize(16777215, 260));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        list->setFont(font);
        list->setAlternatingRowColors(true);
        list->setMovement(QListView::Free);
        list->setResizeMode(QListView::Adjust);

        verticalLayout->addWidget(list);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        button_add = new QPushButton(keybindings);
        button_add->setObjectName(QString::fromUtf8("button_add"));
        button_add->setMinimumSize(QSize(80, 0));
        button_add->setMaximumSize(QSize(100, 16777215));

        horizontalLayout->addWidget(button_add);

        button_remove = new QPushButton(keybindings);
        button_remove->setObjectName(QString::fromUtf8("button_remove"));
        button_remove->setMinimumSize(QSize(80, 0));
        button_remove->setMaximumSize(QSize(100, 16777215));

        horizontalLayout->addWidget(button_remove);

        horizontalSpacer_3 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer_3);


        verticalLayout->addLayout(horizontalLayout);

        line = new QFrame(keybindings);
        line->setObjectName(QString::fromUtf8("line"));
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(line);

        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        verticalLayout_2 = new QVBoxLayout();
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        label_keyMod = new QLabel(keybindings);
        label_keyMod->setObjectName(QString::fromUtf8("label_keyMod"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(label_keyMod->sizePolicy().hasHeightForWidth());
        label_keyMod->setSizePolicy(sizePolicy1);
        label_keyMod->setMinimumSize(QSize(100, 0));
        label_keyMod->setMaximumSize(QSize(140, 16777215));
        QFont font1;
        font1.setFamily(QString::fromUtf8("Arial"));
        font1.setPointSize(12);
        label_keyMod->setFont(font1);

        verticalLayout_2->addWidget(label_keyMod);

        combo_keyMod = new QComboBox(keybindings);
        combo_keyMod->setObjectName(QString::fromUtf8("combo_keyMod"));
        combo_keyMod->setMinimumSize(QSize(140, 0));
        combo_keyMod->setMaximumSize(QSize(300, 16777215));

        verticalLayout_2->addWidget(combo_keyMod);


        horizontalLayout_2->addLayout(verticalLayout_2);

        verticalLayout_3 = new QVBoxLayout();
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        label_key = new QLabel(keybindings);
        label_key->setObjectName(QString::fromUtf8("label_key"));
        sizePolicy1.setHeightForWidth(label_key->sizePolicy().hasHeightForWidth());
        label_key->setSizePolicy(sizePolicy1);
        label_key->setMinimumSize(QSize(80, 0));
        label_key->setMaximumSize(QSize(100, 16777215));
        label_key->setFont(font1);

        verticalLayout_3->addWidget(label_key);

        combo_key = new QComboBox(keybindings);
        combo_key->setObjectName(QString::fromUtf8("combo_key"));
        combo_key->setMinimumSize(QSize(140, 0));
        combo_key->setMaximumSize(QSize(240, 16777215));

        verticalLayout_3->addWidget(combo_key);


        horizontalLayout_2->addLayout(verticalLayout_3);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer);


        verticalLayout->addLayout(horizontalLayout_2);

        label_name = new QLabel(keybindings);
        label_name->setObjectName(QString::fromUtf8("label_name"));
        sizePolicy1.setHeightForWidth(label_name->sizePolicy().hasHeightForWidth());
        label_name->setSizePolicy(sizePolicy1);
        label_name->setMaximumSize(QSize(160, 20));
        label_name->setFont(font1);

        verticalLayout->addWidget(label_name);

        line_name = new QLineEdit(keybindings);
        line_name->setObjectName(QString::fromUtf8("line_name"));
        line_name->setMinimumSize(QSize(100, 0));
        line_name->setMaximumSize(QSize(500, 28));

        verticalLayout->addWidget(line_name);

        label_guiPath = new QLabel(keybindings);
        label_guiPath->setObjectName(QString::fromUtf8("label_guiPath"));
        sizePolicy1.setHeightForWidth(label_guiPath->sizePolicy().hasHeightForWidth());
        label_guiPath->setSizePolicy(sizePolicy1);
        label_guiPath->setMaximumSize(QSize(160, 20));
        label_guiPath->setFont(font1);

        verticalLayout->addWidget(label_guiPath);

        line_guiPath = new QLineEdit(keybindings);
        line_guiPath->setObjectName(QString::fromUtf8("line_guiPath"));
        line_guiPath->setMinimumSize(QSize(100, 0));
        line_guiPath->setMaximumSize(QSize(500, 28));

        verticalLayout->addWidget(line_guiPath);

        label_documentation = new QLabel(keybindings);
        label_documentation->setObjectName(QString::fromUtf8("label_documentation"));
        sizePolicy1.setHeightForWidth(label_documentation->sizePolicy().hasHeightForWidth());
        label_documentation->setSizePolicy(sizePolicy1);
        label_documentation->setMaximumSize(QSize(160, 20));
        label_documentation->setFont(font1);

        verticalLayout->addWidget(label_documentation);

        line_documentation = new QLineEdit(keybindings);
        line_documentation->setObjectName(QString::fromUtf8("line_documentation"));
        line_documentation->setMinimumSize(QSize(100, 0));
        line_documentation->setMaximumSize(QSize(16777215, 28));

        verticalLayout->addWidget(line_documentation);

        checkBox_local = new QCheckBox(keybindings);
        checkBox_local->setObjectName(QString::fromUtf8("checkBox_local"));
        checkBox_local->setMaximumSize(QSize(160, 20));
        checkBox_local->setFont(font1);

        verticalLayout->addWidget(checkBox_local);

        label_script = new QLabel(keybindings);
        label_script->setObjectName(QString::fromUtf8("label_script"));
        sizePolicy1.setHeightForWidth(label_script->sizePolicy().hasHeightForWidth());
        label_script->setSizePolicy(sizePolicy1);
        label_script->setMaximumSize(QSize(160, 20));
        label_script->setFont(font1);

        verticalLayout->addWidget(label_script);

        text_script = new QTextEdit(keybindings);
        text_script->setObjectName(QString::fromUtf8("text_script"));
        QSizePolicy sizePolicy2(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(3);
        sizePolicy2.setHeightForWidth(text_script->sizePolicy().hasHeightForWidth());
        text_script->setSizePolicy(sizePolicy2);
        text_script->setMinimumSize(QSize(100, 80));
        text_script->setMaximumSize(QSize(16777215, 260));
        text_script->setFont(font);

        verticalLayout->addWidget(text_script);

        horizontalLayout_3 = new QHBoxLayout();
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        button_save = new QPushButton(keybindings);
        button_save->setObjectName(QString::fromUtf8("button_save"));
        button_save->setMinimumSize(QSize(80, 0));
        button_save->setMaximumSize(QSize(100, 16777215));

        horizontalLayout_3->addWidget(button_save);

        button_cancel = new QPushButton(keybindings);
        button_cancel->setObjectName(QString::fromUtf8("button_cancel"));
        button_cancel->setMinimumSize(QSize(80, 0));
        button_cancel->setMaximumSize(QSize(100, 16777215));

        horizontalLayout_3->addWidget(button_cancel);

        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_2);


        verticalLayout->addLayout(horizontalLayout_3);

        line_2 = new QFrame(keybindings);
        line_2->setObjectName(QString::fromUtf8("line_2"));
        line_2->setFrameShape(QFrame::HLine);
        line_2->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(line_2);

        hLay_bottom_buttonBox = new QHBoxLayout();
        hLay_bottom_buttonBox->setObjectName(QString::fromUtf8("hLay_bottom_buttonBox"));

        QFont fontE;
        fontE.setFamily(QString::fromUtf8("Arial"));
        label_error = new QLabel(keybindings);
        label_error->setObjectName(QString::fromUtf8("label_error"));
        QSizePolicy sizePolicy3(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy3.setHorizontalStretch(0);
        sizePolicy3.setVerticalStretch(0);
        sizePolicy3.setHeightForWidth(label_error->sizePolicy().hasHeightForWidth());
        label_error->setSizePolicy(sizePolicy3);
        label_error->setMinimumSize(QSize(320, 40));
        label_error->setMaximumSize(QSize(800, 40));
        label_error->setFont(fontE);
        label_error->setWordWrap(true);
        hLay_bottom_buttonBox->addWidget(label_error);

        buttonBox = new QDialogButtonBox(keybindings);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        hLay_bottom_buttonBox->addWidget(buttonBox);
        verticalLayout->addLayout(hLay_bottom_buttonBox);

        retranslateUi(keybindings);
        //QObject::connect(buttonBox, SIGNAL(accepted()), keybindings, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), keybindings, SLOT(reject()));

        QMetaObject::connectSlotsByName(keybindings);
    } // setupUi

    void retranslateUi(QDialog *keybindings)
    {
        keybindings->setWindowTitle(QCoreApplication::translate("keybindings", "Assign Keybindings", nullptr));
        button_add->setText(QCoreApplication::translate("keybindings", "Add New", nullptr));
        button_remove->setText(QCoreApplication::translate("keybindings", "Remove", nullptr));
        label_keyMod->setText(QCoreApplication::translate("keybindings", "Key Modifier", nullptr));
#if QT_CONFIG(tooltip)
        combo_keyMod->setToolTip(QCoreApplication::translate("keybindings", "<html><head/><body><p>Modifier keys to hold while key is pressed (blank means none)</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_key->setText(QCoreApplication::translate("keybindings", "Key", nullptr));
#if QT_CONFIG(tooltip)
        combo_key->setToolTip(QCoreApplication::translate("keybindings", "<html><head/><body><p>Key to press for this keybinding</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_name->setText(QCoreApplication::translate("keybindings", "Name", nullptr));
#if QT_CONFIG(tooltip)
        line_name->setToolTip(QCoreApplication::translate("keybindings", "<html><head/><body><p>Name assigned to this keybinding</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_guiPath->setText(QCoreApplication::translate("keybindings", "GUI Path", nullptr));
#if QT_CONFIG(tooltip)
        line_guiPath->setToolTip(QCoreApplication::translate("keybindings", "<html><head/><body><p>[OPTIONAL] Path for where this keybinding appears in GUI menu</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_documentation->setText(QCoreApplication::translate("keybindings", "Documentation", nullptr));
#if QT_CONFIG(tooltip)
        line_documentation->setToolTip(QCoreApplication::translate("keybindings", "<html><head/><body><p>[OPTIONAL] Documentation entry for keybinding</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
#if QT_CONFIG(tooltip)
        checkBox_local->setToolTip(QCoreApplication::translate("keybindings", "<html><head/><body><p>[OPTIONAL] Bool for if the lua script command executes locally</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        checkBox_local->setText(QCoreApplication::translate("keybindings", "Local", nullptr));
        label_script->setText(QCoreApplication::translate("keybindings", "Script", nullptr));
#if QT_CONFIG(tooltip)
        text_script->setToolTip(QCoreApplication::translate("keybindings", "<html><head/><body><p>Command(s) to execute at keypress event</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        button_save->setText(QCoreApplication::translate("keybindings", "Save", nullptr));
        button_cancel->setText(QCoreApplication::translate("keybindings", "Cancel", nullptr));
        label_error->setText(QCoreApplication::translate("keybindings", "", nullptr));
    } // retranslateUi

};

namespace Ui {
    class keybindings: public Ui_keybindings {};
} // namespace Ui

QT_END_NAMESPACE

#endif // __OPENSPACE_UI_LAUNCHER___UI_KEYBINDINGS___H__
