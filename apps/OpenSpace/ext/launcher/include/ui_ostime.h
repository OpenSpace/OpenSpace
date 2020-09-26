/********************************************************************************
** Form generated from reading UI file 'ostime.ui'
**
** Created by: Qt User Interface Compiler version 5.15.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_OSTIME_H
#define UI_OSTIME_H

#include <QtCore/QDate>
#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QDateEdit>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QTimeEdit>

QT_BEGIN_NAMESPACE

class Ui_time
{
public:
    QDialogButtonBox *buttonBox;
    QLabel *label_relative;
    QLineEdit *line_relative;
    QComboBox *combo_type;
    QLabel *label_type;
    QLabel *label_absolete;
    QDateTimeEdit *dateTimeEdit;

    void setupUi(QDialog *time)
    {
        if (time->objectName().isEmpty())
            time->setObjectName(QString::fromUtf8("time"));
        time->resize(488, 230);
        buttonBox = new QDialogButtonBox(time);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setGeometry(QRect(130, 180, 341, 32));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);
        label_relative = new QLabel(time);
        label_relative->setObjectName(QString::fromUtf8("label_relative"));
        label_relative->setGeometry(QRect(20, 132, 111, 17));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        font.setPointSize(12);
        label_relative->setFont(font);
        line_relative = new QLineEdit(time);
        line_relative->setObjectName(QString::fromUtf8("line_relative"));
        line_relative->setGeometry(QRect(140, 130, 331, 25));
        line_relative->setFont(font);
        combo_type = new QComboBox(time);
        combo_type->setObjectName(QString::fromUtf8("combo_type"));
        combo_type->setGeometry(QRect(20, 40, 151, 25));
        label_type = new QLabel(time);
        label_type->setObjectName(QString::fromUtf8("label_type"));
        label_type->setGeometry(QRect(20, 20, 91, 17));
        label_type->setFont(font);
        label_absolete = new QLabel(time);
        label_absolete->setObjectName(QString::fromUtf8("label_absolete"));
        label_absolete->setGeometry(QRect(20, 88, 111, 17));
        label_absolete->setFont(font);
        dateTimeEdit = new QDateTimeEdit(time);
        dateTimeEdit->setObjectName(QString::fromUtf8("dateTimeEdit"));
        dateTimeEdit->setGeometry(QRect(140, 80, 201, 31));
        dateTimeEdit->setFont(font);
        dateTimeEdit->setMinimumDate(QDate(100,1,1));

        retranslateUi(time);
        //QObject::connect(buttonBox, SIGNAL(accepted()), time, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), time, SLOT(reject()));

        QMetaObject::connectSlotsByName(time);
    } // setupUi

    void retranslateUi(QDialog *time)
    {
        time->setWindowTitle(QCoreApplication::translate("time", "Time", nullptr));
        label_relative->setText(QCoreApplication::translate("time", "Relative Time:", nullptr));
#if QT_CONFIG(tooltip)
        line_relative->setToolTip(QCoreApplication::translate("time", "<html><head/><body><p>String for relative time to actual (e.g. &quot;-1d&quot; for back 1 day)</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
#if QT_CONFIG(tooltip)
        combo_type->setToolTip(QCoreApplication::translate("time", "<html><head/><body><p>Types: Absolute defined time or Relative to actual time</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_type->setText(QCoreApplication::translate("time", "Time Type", nullptr));
        label_absolete->setText(QCoreApplication::translate("time", "Absolute UTC:", nullptr));
        dateTimeEdit->setDisplayFormat(QCoreApplication::translate("time", "yyyy-MM-dd  T  hh:mm:ss", nullptr));
    } // retranslateUi

};

namespace Ui {
    class time: public Ui_time {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_OSTIME_H
