/********************************************************************************
** Form generated from reading UI file 'properties.ui'
**
** Created by: Qt User Interface Compiler version 5.15.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_PROPERTIES_H
#define UI_PROPERTIES_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QFrame>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QListWidget>
#include <QtWidgets/QPushButton>

QT_BEGIN_NAMESPACE

class Ui_properties
{
public:
    QDialogButtonBox *buttonBox;
    QListWidget *list;
    QPushButton *button_add;
    QPushButton *button_remove;
    QFrame *frame;
    QLabel *label_command;
    QComboBox *combo_command;
    QLabel *label_property;
    QLineEdit *line_property;
    QLabel *label_value;
    QLineEdit *line_value;
    QPushButton *button_cancel;
    QPushButton *button_save;

    void setupUi(QDialog *properties)
    {
        if (properties->objectName().isEmpty())
            properties->setObjectName(QString::fromUtf8("properties"));
        properties->resize(591, 448);
        buttonBox = new QDialogButtonBox(properties);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setGeometry(QRect(230, 400, 341, 32));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);
        list = new QListWidget(properties);
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
        button_add = new QPushButton(properties);
        button_add->setObjectName(QString::fromUtf8("button_add"));
        button_add->setGeometry(QRect(10, 140, 111, 25));
        button_remove = new QPushButton(properties);
        button_remove->setObjectName(QString::fromUtf8("button_remove"));
        button_remove->setGeometry(QRect(160, 140, 111, 25));
        frame = new QFrame(properties);
        frame->setObjectName(QString::fromUtf8("frame"));
        frame->setGeometry(QRect(10, 190, 571, 191));
        frame->setFrameShape(QFrame::StyledPanel);
        frame->setFrameShadow(QFrame::Raised);
        label_command = new QLabel(frame);
        label_command->setObjectName(QString::fromUtf8("label_command"));
        label_command->setGeometry(QRect(10, 10, 357, 20));
        QFont font1;
        font1.setFamily(QString::fromUtf8("Arial"));
        font1.setPointSize(12);
        label_command->setFont(font1);
        combo_command = new QComboBox(frame);
        combo_command->setObjectName(QString::fromUtf8("combo_command"));
        combo_command->setGeometry(QRect(10, 30, 220, 25));
        QSizePolicy sizePolicy1(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(combo_command->sizePolicy().hasHeightForWidth());
        combo_command->setSizePolicy(sizePolicy1);
        combo_command->setMinimumSize(QSize(220, 0));
        combo_command->setMaximumSize(QSize(220, 16777215));
        label_property = new QLabel(frame);
        label_property->setObjectName(QString::fromUtf8("label_property"));
        label_property->setGeometry(QRect(10, 70, 357, 20));
        label_property->setFont(font1);
        line_property = new QLineEdit(frame);
        line_property->setObjectName(QString::fromUtf8("line_property"));
        line_property->setGeometry(QRect(10, 90, 541, 25));
        label_value = new QLabel(frame);
        label_value->setObjectName(QString::fromUtf8("label_value"));
        label_value->setGeometry(QRect(10, 130, 357, 20));
        label_value->setFont(font1);
        line_value = new QLineEdit(frame);
        line_value->setObjectName(QString::fromUtf8("line_value"));
        line_value->setGeometry(QRect(10, 150, 357, 25));
        button_cancel = new QPushButton(frame);
        button_cancel->setObjectName(QString::fromUtf8("button_cancel"));
        button_cancel->setGeometry(QRect(410, 150, 71, 25));
        button_save = new QPushButton(frame);
        button_save->setObjectName(QString::fromUtf8("button_save"));
        button_save->setGeometry(QRect(490, 150, 71, 25));

        retranslateUi(properties);
        QObject::connect(buttonBox, SIGNAL(accepted()), properties, SLOT(accept()));
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
        button_cancel->setText(QCoreApplication::translate("properties", "Cancel", nullptr));
        button_save->setText(QCoreApplication::translate("properties", "Save", nullptr));
    } // retranslateUi

};

namespace Ui {
    class properties: public Ui_properties {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_PROPERTIES_H
