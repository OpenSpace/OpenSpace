/********************************************************************************
** Form generated from reading UI file 'keybindings.ui'
**
** Created by: Qt User Interface Compiler version 5.15.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_KEYBINDINGS_H
#define UI_KEYBINDINGS_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QFrame>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QListWidget>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QTextEdit>

QT_BEGIN_NAMESPACE

class Ui_keybindings
{
public:
    QDialogButtonBox *buttonBox;
    QListWidget *list;
    QPushButton *button_add;
    QPushButton *button_remove;
    QFrame *frame;
    QLabel *label_keyMod;
    QComboBox *combo_keyMod;
    QComboBox *combo_key;
    QLabel *label_name;
    QLineEdit *line_name;
    QLabel *label_documentation;
    QLineEdit *line_documentation;
    QLabel *label_guiPath;
    QLineEdit *line_guiPath;
    QCheckBox *checkBox_local;
    QLabel *label_script;
    QTextEdit *text_script;
    QPushButton *button_cancel;
    QPushButton *button_save;
    QLabel *label_key;

    void setupUi(QDialog *keybindings)
    {
        if (keybindings->objectName().isEmpty())
            keybindings->setObjectName(QString::fromUtf8("keybindings"));
        keybindings->resize(681, 694);
        buttonBox = new QDialogButtonBox(keybindings);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setGeometry(QRect(330, 650, 341, 32));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);
        list = new QListWidget(keybindings);
        list->setObjectName(QString::fromUtf8("list"));
        list->setGeometry(QRect(10, 10, 661, 120));
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
        button_add = new QPushButton(keybindings);
        button_add->setObjectName(QString::fromUtf8("button_add"));
        button_add->setGeometry(QRect(10, 140, 111, 25));
        button_remove = new QPushButton(keybindings);
        button_remove->setObjectName(QString::fromUtf8("button_remove"));
        button_remove->setGeometry(QRect(160, 140, 111, 25));
        frame = new QFrame(keybindings);
        frame->setObjectName(QString::fromUtf8("frame"));
        frame->setGeometry(QRect(10, 190, 661, 441));
        frame->setFrameShape(QFrame::StyledPanel);
        frame->setFrameShadow(QFrame::Raised);
        label_keyMod = new QLabel(frame);
        label_keyMod->setObjectName(QString::fromUtf8("label_keyMod"));
        label_keyMod->setGeometry(QRect(10, 10, 121, 20));
        QFont font1;
        font1.setFamily(QString::fromUtf8("Arial"));
        font1.setPointSize(12);
        label_keyMod->setFont(font1);
        combo_keyMod = new QComboBox(frame);
        combo_keyMod->setObjectName(QString::fromUtf8("combo_keyMod"));
        combo_keyMod->setGeometry(QRect(10, 30, 211, 25));
        combo_key = new QComboBox(frame);
        combo_key->setObjectName(QString::fromUtf8("combo_key"));
        combo_key->setGeometry(QRect(240, 30, 181, 25));
        label_name = new QLabel(frame);
        label_name->setObjectName(QString::fromUtf8("label_name"));
        label_name->setGeometry(QRect(10, 70, 357, 20));
        label_name->setFont(font1);
        line_name = new QLineEdit(frame);
        line_name->setObjectName(QString::fromUtf8("line_name"));
        line_name->setGeometry(QRect(10, 90, 641, 25));
        label_documentation = new QLabel(frame);
        label_documentation->setObjectName(QString::fromUtf8("label_documentation"));
        label_documentation->setGeometry(QRect(10, 130, 357, 20));
        label_documentation->setFont(font1);
        line_documentation = new QLineEdit(frame);
        line_documentation->setObjectName(QString::fromUtf8("line_documentation"));
        line_documentation->setGeometry(QRect(10, 150, 641, 25));
        label_guiPath = new QLabel(frame);
        label_guiPath->setObjectName(QString::fromUtf8("label_guiPath"));
        label_guiPath->setGeometry(QRect(10, 190, 357, 20));
        label_guiPath->setFont(font1);
        line_guiPath = new QLineEdit(frame);
        line_guiPath->setObjectName(QString::fromUtf8("line_guiPath"));
        line_guiPath->setGeometry(QRect(10, 210, 641, 25));
        checkBox_local = new QCheckBox(frame);
        checkBox_local->setObjectName(QString::fromUtf8("checkBox_local"));
        checkBox_local->setGeometry(QRect(10, 250, 92, 23));
        checkBox_local->setFont(font1);
        label_script = new QLabel(frame);
        label_script->setObjectName(QString::fromUtf8("label_script"));
        label_script->setGeometry(QRect(10, 280, 357, 20));
        label_script->setFont(font1);
        text_script = new QTextEdit(frame);
        text_script->setObjectName(QString::fromUtf8("text_script"));
        text_script->setGeometry(QRect(10, 300, 641, 70));
        button_cancel = new QPushButton(frame);
        button_cancel->setObjectName(QString::fromUtf8("button_cancel"));
        button_cancel->setGeometry(QRect(500, 400, 71, 25));
        button_save = new QPushButton(frame);
        button_save->setObjectName(QString::fromUtf8("button_save"));
        button_save->setGeometry(QRect(580, 400, 71, 25));
        label_key = new QLabel(frame);
        label_key->setObjectName(QString::fromUtf8("label_key"));
        label_key->setGeometry(QRect(240, 10, 61, 20));
        label_key->setFont(font1);
        label_keyMod->raise();
        label_documentation->raise();
        line_documentation->raise();
        label_name->raise();
        button_save->raise();
        button_cancel->raise();
        line_name->raise();
        line_guiPath->raise();
        label_guiPath->raise();
        text_script->raise();
        label_script->raise();
        combo_keyMod->raise();
        combo_key->raise();
        checkBox_local->raise();
        label_key->raise();

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
        label_name->setText(QCoreApplication::translate("keybindings", "Name", nullptr));
        label_documentation->setText(QCoreApplication::translate("keybindings", "Documentation", nullptr));
        label_guiPath->setText(QCoreApplication::translate("keybindings", "GUI Path", nullptr));
        checkBox_local->setText(QCoreApplication::translate("keybindings", "Local", nullptr));
        label_script->setText(QCoreApplication::translate("keybindings", "Script", nullptr));
        button_cancel->setText(QCoreApplication::translate("keybindings", "Cancel", nullptr));
        button_save->setText(QCoreApplication::translate("keybindings", "Save", nullptr));
        label_key->setText(QCoreApplication::translate("keybindings", "Key", nullptr));
    } // retranslateUi

};

namespace Ui {
    class keybindings: public Ui_keybindings {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_KEYBINDINGS_H
