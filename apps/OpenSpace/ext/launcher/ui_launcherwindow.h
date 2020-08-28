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

/********************************************************************************
** Form generated from reading UI file 'launcherwindow.ui'
**
** Created by: Qt User Interface Compiler version 5.12.2
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef __OPENSPACE_LAUNCHER_UI__LAUNCHERWINDOW___H__
#define __OPENSPACE_LAUNCHER_UI__LAUNCHERWINDOW___H__

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
#include <QtWidgets/QTextEdit>
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
    QMenuBar *menubar;
    QMenu *menuOpenSpace_Launcher;
    QStatusBar *statusbar;

    void setupUi(QMainWindow *LauncherWindow)
    {
        if (LauncherWindow->objectName().isEmpty())
            LauncherWindow->setObjectName(QString::fromUtf8("LauncherWindow"));
        LauncherWindow->resize(586, 600);
        LauncherWindow->setAutoFillBackground(false);
        LauncherWindow->setStyleSheet(QString::fromUtf8(
            "background: rgb(46, 52, 54);\n"
            "\n"
            "QPushButton {\n"
            "  background: rgb(186, 189, 182);\n"
            "}")
        );
        centralwidget = new QWidget(LauncherWindow);
        centralwidget->setObjectName(QString::fromUtf8("centralwidget"));
        newButton = new QPushButton(centralwidget);
        newButton->setObjectName(QString::fromUtf8("newButton"));
        newButton->setGeometry(QRect(300, 420, 100, 40));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        newButton->setFont(font);
        newButton->setStyleSheet(QString::fromUtf8("background: rgb(186, 189, 182)"));
        qBtn_start = new QPushButton(centralwidget);
        qBtn_start->setObjectName(QString::fromUtf8("qBtn_start"));
        qBtn_start->setGeometry(QRect(160, 330, 240, 60));
        qBtn_start->setFont(font);
        qBtn_start->setStyleSheet(QString::fromUtf8("background: rgb(186, 189, 182)"));
        editButton = new QPushButton(centralwidget);
        editButton->setObjectName(QString::fromUtf8("editButton"));
        editButton->setGeometry(QRect(160, 420, 100, 40));
        editButton->setFont(font);
        editButton->setStyleSheet(QString::fromUtf8("background: rgb(186, 189, 182)"));
        logolabel = new QLabel(centralwidget);
        logolabel->setObjectName(QString::fromUtf8("logolabel"));
        logolabel->setGeometry(QRect(80, 0, 400, 120));
        comboBoxProfiles = new QComboBox(centralwidget);
        comboBoxProfiles->setObjectName(QString::fromUtf8("comboBoxProfiles"));
        comboBoxProfiles->setGeometry(QRect(160, 160, 240, 50));
        comboBoxWindowConfigs = new QComboBox(centralwidget);
        comboBoxWindowConfigs->setObjectName(QString::fromUtf8("comboBoxWindowConfigs"));
        comboBoxWindowConfigs->setGeometry(QRect(160, 250, 240, 50));
        labelChoose = new QLabel(centralwidget);
        labelChoose->setObjectName(QString::fromUtf8("labelChoose"));
        labelChoose->setGeometry(QRect(160, 140, 151, 17));
        labelChoose->setFont(font);
        labelChoose->setTextFormat(Qt::RichText);
        labelOptions = new QLabel(centralwidget);
        labelOptions->setObjectName(QString::fromUtf8("labelOptions"));
        labelOptions->setGeometry(QRect(160, 230, 151, 17));
        labelOptions->setFont(font);
        labelOptions->setTextFormat(Qt::RichText);
        LauncherWindow->setCentralWidget(centralwidget);
        menubar = new QMenuBar(LauncherWindow);
        menubar->setObjectName(QString::fromUtf8("menubar"));
        menubar->setEnabled(false);
        menubar->setGeometry(QRect(0, 0, 586, 22));
        menuOpenSpace_Launcher = new QMenu(menubar);
        menuOpenSpace_Launcher->setObjectName(
            QString::fromUtf8("menuOpenSpace_Launcher"));
        LauncherWindow->setMenuBar(menubar);
        statusbar = new QStatusBar(LauncherWindow);
        statusbar->setObjectName(QString::fromUtf8("statusbar"));
        statusbar->setEnabled(false);
        LauncherWindow->setStatusBar(statusbar);

        menubar->addAction(menuOpenSpace_Launcher->menuAction());

        retranslateUi(LauncherWindow);

        QMetaObject::connectSlotsByName(LauncherWindow);
    }

    void retranslateUi(QMainWindow *LauncherWindow)
    {
        LauncherWindow->setWindowTitle(QApplication::translate(
            "LauncherWindow",
            "LauncherWindow",
            nullptr));
        newButton->setText(QApplication::translate(
            "LauncherWindow",
            "New Profile",
            nullptr)
        );
        qBtn_start->setText(QApplication::translate(
            "LauncherWindow",
            "Start OpenSpace",
            nullptr)
        );
        editButton->setText(QApplication::translate(
            "LauncherWindow",
            "Edit Profile",
            nullptr)
        );
        logolabel->setText(QString());
        labelChoose->setText(QApplication::translate(
            "LauncherWindow",
            "<html><head/><body><p><span style=\" font-size:12pt; font-weight:600; "
            "color:#ffffff;\">Choose Profile</span></p></body></html>",
            nullptr)
        );
        labelOptions->setText(QApplication::translate(
            "LauncherWindow",
            "<html><head/><body><p><span style=\" font-size:12pt; font-weight:600; "
            "color:#ffffff;\">Window Options</span></p></body></html>",
            nullptr)
        );
        menuOpenSpace_Launcher->setTitle(QApplication::translate(
            "LauncherWindow",
            "OpenSpace Launcher",
            nullptr)
        );
    }

};

namespace Ui {
    class LauncherWindow: public Ui_LauncherWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // __OPENSPACE_LAUNCHER_UI__LAUNCHERWINDOW___H__
