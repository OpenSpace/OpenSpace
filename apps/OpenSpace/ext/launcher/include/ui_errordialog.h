/********************************************************************************
** Form generated from reading UI file 'errordialog.ui'
**
** Created by: Qt User Interface Compiler version 5.15.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_ERRORDIALOG_H
#define UI_ERRORDIALOG_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QLabel>

QT_BEGIN_NAMESPACE

class Ui_errordialog
{
public:
    QDialogButtonBox *buttonBox;
    QLabel *label;

    void setupUi(QDialog *errordialog)
    {
        if (errordialog->objectName().isEmpty())
            errordialog->setObjectName(QString::fromUtf8("errordialog"));
        errordialog->resize(400, 181);
        buttonBox = new QDialogButtonBox(errordialog);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setGeometry(QRect(140, 120, 91, 32));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Ok);
        label = new QLabel(errordialog);
        label->setObjectName(QString::fromUtf8("label"));
        label->setGeometry(QRect(20, 10, 360, 100));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        font.setPointSize(12);
        label->setFont(font);
        label->setWordWrap(true);

        retranslateUi(errordialog);
        QObject::connect(buttonBox, SIGNAL(accepted()), errordialog, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), errordialog, SLOT(reject()));

        QMetaObject::connectSlotsByName(errordialog);
    } // setupUi

    void retranslateUi(QDialog *errordialog)
    {
        errordialog->setWindowTitle(QCoreApplication::translate("errordialog", "Profile Format Error", nullptr));
        label->setText(QCoreApplication::translate("errordialog", "TextLabel", nullptr));
    } // retranslateUi

};

namespace Ui {
    class errordialog: public Ui_errordialog {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_ERRORDIALOG_H
