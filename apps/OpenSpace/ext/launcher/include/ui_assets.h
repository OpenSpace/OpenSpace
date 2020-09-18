/********************************************************************************
** Form generated from reading UI file 'assets.ui'
**
** Created by: Qt User Interface Compiler version 5.15.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_ASSETS_H
#define UI_ASSETS_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QTreeView>

QT_BEGIN_NAMESPACE

class Ui_assets
{
public:
    QDialogButtonBox *buttonBox;
    QTreeView *treeView;
    QPushButton *varName;
    QLineEdit *lineEdit;
    QLabel *label;

    void setupUi(QDialog *assets)
    {
        if (assets->objectName().isEmpty())
            assets->setObjectName(QString::fromUtf8("assets"));
        assets->resize(610, 479);
        buttonBox = new QDialogButtonBox(assets);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setGeometry(QRect(240, 430, 341, 32));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);
        treeView = new QTreeView(assets);
        treeView->setObjectName(QString::fromUtf8("treeView"));
        treeView->setGeometry(QRect(20, 20, 571, 351));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        treeView->setFont(font);
        //treeView->setStyleSheet(QString::fromUtf8("background-color: rgb(85, 87, 83);"));
        treeView->setAlternatingRowColors(false);
        treeView->setAnimated(false);
        varName = new QPushButton(assets);
        varName->setObjectName(QString::fromUtf8("varName"));
        varName->setGeometry(QRect(130, 430, 121, 25));
        lineEdit = new QLineEdit(assets);
        lineEdit->setObjectName(QString::fromUtf8("lineEdit"));
        lineEdit->setGeometry(QRect(130, 390, 261, 25));
        label = new QLabel(assets);
        label->setObjectName(QString::fromUtf8("label"));
        label->setGeometry(QRect(30, 390, 71, 61));
        QFont font1;
        font1.setFamily(QString::fromUtf8("Arial"));
        font1.setPointSize(12);
        label->setFont(font1);
        label->setWordWrap(true);

        retranslateUi(assets);
        //QObject::connect(buttonBox, SIGNAL(accepted()), assets, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), assets, SLOT(reject()));

        QMetaObject::connectSlotsByName(assets);
    } // setupUi

    void retranslateUi(QDialog *assets)
    {
        assets->setWindowTitle(QCoreApplication::translate("assets", "Dialog", nullptr));
#if QT_CONFIG(tooltip)
        treeView->setToolTip(QCoreApplication::translate("assets", "<html><head/><body><p>Expand arrow entries to browse assets in this OpenSpace installation. Enable checkbox to include an asset. Those assets highlighted in red are present in the profile but do not exist in this OpenSpace installation.</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        varName->setText(QCoreApplication::translate("assets", "Modify Name", nullptr));
        label->setText(QCoreApplication::translate("assets", "Optional variable name", nullptr));
    } // retranslateUi

};

namespace Ui {
    class assets: public Ui_assets {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_ASSETS_H
