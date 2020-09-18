/********************************************************************************
** Form generated from reading UI file 'deltaTimes.ui'
**
** Created by: Qt User Interface Compiler version 5.15.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_DELTATIMES_H
#define UI_DELTATIMES_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QListWidget>
#include <QtWidgets/QPushButton>

QT_BEGIN_NAMESPACE

class Ui_deltaTimes
{
public:
    QDialogButtonBox *buttonBox;
    QLabel *label_module;
    QListWidget *listWidget;
    QLabel *label_adjust;
    QPushButton *button_save;
    QPushButton *button_add;
    QPushButton *button_remove;
    QLineEdit *line_seconds;

    void setupUi(QDialog *deltaTimes)
    {
        if (deltaTimes->objectName().isEmpty())
            deltaTimes->setObjectName(QString::fromUtf8("deltaTimes"));
        deltaTimes->resize(531, 439);
        buttonBox = new QDialogButtonBox(deltaTimes);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setGeometry(QRect(170, 390, 341, 32));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);
        label_module = new QLabel(deltaTimes);
        label_module->setObjectName(QString::fromUtf8("label_module"));
        label_module->setGeometry(QRect(20, 20, 141, 20));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        font.setPointSize(12);
        label_module->setFont(font);
        listWidget = new QListWidget(deltaTimes);
        listWidget->setObjectName(QString::fromUtf8("listWidget"));
        listWidget->setGeometry(QRect(20, 50, 381, 171));
        listWidget->setAutoScroll(true);
        listWidget->setLayoutMode(QListView::SinglePass);
        label_adjust = new QLabel(deltaTimes);
        label_adjust->setObjectName(QString::fromUtf8("label_adjust"));
        label_adjust->setGeometry(QRect(20, 284, 421, 20));
        label_adjust->setFont(font);
        button_save = new QPushButton(deltaTimes);
        button_save->setObjectName(QString::fromUtf8("button_save"));
        button_save->setGeometry(QRect(20, 354, 71, 25));
        button_add = new QPushButton(deltaTimes);
        button_add->setObjectName(QString::fromUtf8("button_add"));
        button_add->setGeometry(QRect(20, 230, 151, 25));
        button_remove = new QPushButton(deltaTimes);
        button_remove->setObjectName(QString::fromUtf8("button_remove"));
        button_remove->setGeometry(QRect(190, 230, 151, 25));
        line_seconds = new QLineEdit(deltaTimes);
        line_seconds->setObjectName(QString::fromUtf8("line_seconds"));
        line_seconds->setGeometry(QRect(20, 310, 191, 31));
        line_seconds->setFont(font);

        retranslateUi(deltaTimes);
        //QObject::connect(buttonBox, SIGNAL(accepted()), deltaTimes, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), deltaTimes, SLOT(reject()));

        QMetaObject::connectSlotsByName(deltaTimes);
    } // setupUi

    void retranslateUi(QDialog *deltaTimes)
    {
        deltaTimes->setWindowTitle(QCoreApplication::translate("deltaTimes", "Delta Time Steps", nullptr));
        label_module->setText(QCoreApplication::translate("deltaTimes", "Delta Times", nullptr));
        label_adjust->setText(QCoreApplication::translate("deltaTimes", "Set Delta Time for key", nullptr));
        button_save->setText(QCoreApplication::translate("deltaTimes", "Save", nullptr));
        button_add->setText(QCoreApplication::translate("deltaTimes", "Add New Entry", nullptr));
        button_remove->setText(QCoreApplication::translate("deltaTimes", "Remove Last Entry", nullptr));
    } // retranslateUi

};

namespace Ui {
    class deltaTimes: public Ui_deltaTimes {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_DELTATIMES_H
