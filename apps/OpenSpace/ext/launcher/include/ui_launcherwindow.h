/********************************************************************************
** Form generated from reading UI file 'launcherwindow.ui'
**
** Created by: Qt User Interface Compiler version 5.15.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_LAUNCHERWINDOW_H
#define UI_LAUNCHERWINDOW_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QLabel>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QMenu>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_LauncherWindow
{
public:
    QWidget *centralwidget;
    QPushButton *newButton;
    QPushButton *qBtn_start;
    QPushButton *editButton;
    QLabel *logolabel;
    QComboBox *comboBoxProfiles;
    QComboBox *comboBoxWindowConfigs;
    QLabel *labelChoose;
    QLabel *labelOptions;
    QPushButton *buttonSim;
    QMenuBar *menubar;
    QMenu *menuOpenSpace_Launcher;
    QStatusBar *statusbar;

    void setupUi(QMainWindow *LauncherWindow)
    {
        if (LauncherWindow->objectName().isEmpty())
            LauncherWindow->setObjectName(QString::fromUtf8("LauncherWindow"));
        LauncherWindow->resize(473, 576);
        LauncherWindow->setAutoFillBackground(false);
        LauncherWindow->setStyleSheet(QString::fromUtf8("background: rgb(46, 52, 54);\n"
"\n"
"QPushButton {\n"
"  background: rgb(186, 189, 182);\n"
"}"));
        centralwidget = new QWidget(LauncherWindow);
        centralwidget->setObjectName(QString::fromUtf8("centralwidget"));
        newButton = new QPushButton(centralwidget);
        newButton->setObjectName(QString::fromUtf8("newButton"));
        newButton->setGeometry(QRect(270, 440, 100, 40));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        newButton->setFont(font);
        newButton->setStyleSheet(QString::fromUtf8("background: rgb(186, 189, 182)"));
        qBtn_start = new QPushButton(centralwidget);
        qBtn_start->setObjectName(QString::fromUtf8("qBtn_start"));
        qBtn_start->setGeometry(QRect(130, 350, 240, 60));
        qBtn_start->setFont(font);
        qBtn_start->setStyleSheet(QString::fromUtf8("background: rgb(186, 189, 182)"));
        editButton = new QPushButton(centralwidget);
        editButton->setObjectName(QString::fromUtf8("editButton"));
        editButton->setGeometry(QRect(130, 440, 100, 40));
        editButton->setFont(font);
        editButton->setStyleSheet(QString::fromUtf8("background: rgb(186, 189, 182)"));
        logolabel = new QLabel(centralwidget);
        logolabel->setObjectName(QString::fromUtf8("logolabel"));
        logolabel->setGeometry(QRect(40, 10, 400, 120));
        comboBoxProfiles = new QComboBox(centralwidget);
        comboBoxProfiles->setObjectName(QString::fromUtf8("comboBoxProfiles"));
        comboBoxProfiles->setGeometry(QRect(130, 180, 240, 50));
        comboBoxWindowConfigs = new QComboBox(centralwidget);
        comboBoxWindowConfigs->setObjectName(QString::fromUtf8("comboBoxWindowConfigs"));
        comboBoxWindowConfigs->setGeometry(QRect(130, 270, 240, 50));
        labelChoose = new QLabel(centralwidget);
        labelChoose->setObjectName(QString::fromUtf8("labelChoose"));
        labelChoose->setGeometry(QRect(130, 160, 151, 17));
        labelChoose->setFont(font);
        labelChoose->setTextFormat(Qt::RichText);
        labelOptions = new QLabel(centralwidget);
        labelOptions->setObjectName(QString::fromUtf8("labelOptions"));
        labelOptions->setGeometry(QRect(130, 250, 151, 17));
        labelOptions->setFont(font);
        labelOptions->setTextFormat(Qt::RichText);
        buttonSim = new QPushButton(centralwidget);
        buttonSim->setObjectName(QString::fromUtf8("buttonSim"));
        buttonSim->setGeometry(QRect(210, 500, 89, 25));
        LauncherWindow->setCentralWidget(centralwidget);
        menubar = new QMenuBar(LauncherWindow);
        menubar->setObjectName(QString::fromUtf8("menubar"));
        menubar->setEnabled(false);
        menubar->setGeometry(QRect(0, 0, 473, 22));
        menuOpenSpace_Launcher = new QMenu(menubar);
        menuOpenSpace_Launcher->setObjectName(QString::fromUtf8("menuOpenSpace_Launcher"));
        LauncherWindow->setMenuBar(menubar);
        statusbar = new QStatusBar(LauncherWindow);
        statusbar->setObjectName(QString::fromUtf8("statusbar"));
        statusbar->setEnabled(false);
        LauncherWindow->setStatusBar(statusbar);

        menubar->addAction(menuOpenSpace_Launcher->menuAction());

        retranslateUi(LauncherWindow);

        QMetaObject::connectSlotsByName(LauncherWindow);
    } // setupUi

    void retranslateUi(QMainWindow *LauncherWindow)
    {
        LauncherWindow->setWindowTitle(QCoreApplication::translate("LauncherWindow", "LauncherWindow", nullptr));
        newButton->setText(QCoreApplication::translate("LauncherWindow", "New Profile", nullptr));
        qBtn_start->setText(QCoreApplication::translate("LauncherWindow", "Start OpenSpace", nullptr));
        editButton->setText(QCoreApplication::translate("LauncherWindow", "Edit Profile", nullptr));
        logolabel->setText(QString());
        labelChoose->setText(QCoreApplication::translate("LauncherWindow", "<html><head/><body><p><span style=\" font-size:12pt; font-weight:600; color:#ffffff;\">Choose Profile</span></p></body></html>", nullptr));
        labelOptions->setText(QCoreApplication::translate("LauncherWindow", "<html><head/><body><p><span style=\" font-size:12pt; font-weight:600; color:#ffffff;\">Window Options</span></p></body></html>", nullptr));
        buttonSim->setText(QCoreApplication::translate("LauncherWindow", "simData", nullptr));
        menuOpenSpace_Launcher->setTitle(QCoreApplication::translate("LauncherWindow", "OpenSpace Launcher", nullptr));
    } // retranslateUi

};

namespace Ui {
    class LauncherWindow: public Ui_LauncherWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_LAUNCHERWINDOW_H
