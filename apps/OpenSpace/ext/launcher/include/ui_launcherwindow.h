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

#ifndef __OPENSPACE_UI_LAUNCHER___UI_LAUNCHERWINDOW___H__
#define __OPENSPACE_UI_LAUNCHER___UI_LAUNCHERWINDOW___H__

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
    QLabel* backgroundImage;
    QLabel* logolabel;
    QComboBox *comboBoxProfiles;
    QComboBox *comboBoxWindowConfigs;
    QLabel *labelChoose;
    QLabel *labelOptions;
    QMenuBar *menubar;
    QMenu *menuOpenSpace_Launcher;
    QStatusBar *statusbar;

    void setupUi(QMainWindow *LauncherWindow)
    {
        int left_ruler = 40;
        int top_ruler = 80;
        int item_width = 240;
        int small_item_width = 100;
        int screen_width = 480;
        int screen_height = 640;

        QString clearStyle = QString::fromStdString("background-color: rgba(0,0,0,0%);");

        if (LauncherWindow->objectName().isEmpty())
            LauncherWindow->setObjectName(QString::fromUtf8("LauncherWindow"));
        LauncherWindow->resize(screen_width, screen_height);
        LauncherWindow->setAutoFillBackground(false);

        QString buttonStyle = QString::fromUtf8(
            "background: rgb(128, 128, 128);\n"
            "border-radius: 2px;\n"
            "border-style: outset;\n"
            "border-width: 2px;\n"
            "border-color: #333;\n"
            "font-size: 24px;\n"
            "font-family: Segoe UI;\n"
            "font-weight: bold;\n"
            "color: #fff;"
        );

        QString buttonStyleSmall = QString::fromUtf8(
            "background: rgb(96, 96, 96);\n"
            "border-radius: 2px;\n"
            "border-style: outset;\n"
            "border-width: 1px;\n"
            "border-color: #111;\n"
            "font-size: 14px;\n"
            "font-family: Segoe UI;\n"
            "font-weight: bold;\n"
            "color: #fff;"
        );

        QString comboStyle = QString::fromUtf8(
            "QComboBox{"
            "background: rgb(96, 96, 96);\n"
            "border: 1px solid gray;"
            "border-radius: 3px;"
            "padding: 1px 18px 1px 3px;"
            "min-width: 6em;"
            "font-size: 14px;"
            "font-family: Segoe UI;"
            "font-weight: bold;"
            "color: #fff;"
            "}"

            "QComboBox QListView"
            "{"
            "background: qlineargradient(x1 : 0, y1 : 0, x2 : 0, y2 : 1,stop : 0 #666, stop: 1 #888);"
            "color:#fff;"
            "}"
        );

        QFont font;
        font.setFamily(QString::fromUtf8("Segoe UI"));

        centralwidget = new QWidget(LauncherWindow);
        centralwidget->setObjectName(QString::fromUtf8("centralwidget"));
        backgroundImage = new QLabel(centralwidget);
        backgroundImage->setObjectName(QString::fromUtf8("logolabel"));
        backgroundImage->setGeometry(QRect(0, 0, screen_width, screen_height));

        logolabel = new QLabel(centralwidget);
        logolabel->setObjectName(QString::fromUtf8("logolabel"));
        logolabel->setGeometry(QRect(left_ruler, top_ruler, item_width, item_width / 4));
        logolabel->setStyleSheet(clearStyle);

        labelChoose = new QLabel(centralwidget);
        labelChoose->setObjectName(QString::fromUtf8("labelChoose"));
        labelChoose->setGeometry(QRect(left_ruler, top_ruler + 80, 151, 24));
        labelChoose->setFont(font);
        labelChoose->setStyleSheet(clearStyle);

        comboBoxProfiles = new QComboBox(centralwidget);
        comboBoxProfiles->setObjectName(QString::fromUtf8("comboBoxProfiles"));
        comboBoxProfiles->setGeometry(QRect(left_ruler, top_ruler + 110, item_width, item_width / 4));
        comboBoxProfiles->setStyleSheet(comboStyle);

        labelOptions = new QLabel(centralwidget);
        labelOptions->setObjectName(QString::fromUtf8("labelOptions"));
        labelOptions->setGeometry(QRect(left_ruler, top_ruler + 180, 151, 24));
        labelOptions->setFont(font);
        labelOptions->setStyleSheet(clearStyle);

        comboBoxWindowConfigs = new QComboBox(centralwidget);
        comboBoxWindowConfigs->setObjectName(QString::fromUtf8("comboBoxWindowConfigs"));
        comboBoxWindowConfigs->setGeometry(QRect(left_ruler, top_ruler + 210, item_width, item_width / 4));
        comboBoxWindowConfigs->setStyleSheet(comboStyle);

        qBtn_start = new QPushButton(centralwidget);
        qBtn_start->setObjectName(QString::fromUtf8("qBtn_start"));
        qBtn_start->setGeometry(QRect(left_ruler, top_ruler + 290, item_width, item_width / 4));
        qBtn_start->setStyleSheet(buttonStyle);
        qBtn_start->setCursor(Qt::PointingHandCursor);

        newButton = new QPushButton(centralwidget);
        newButton->setObjectName(QString::fromUtf8("newButton"));
        newButton->setGeometry(QRect(left_ruler + 140, top_ruler + 380, small_item_width, small_item_width/4));
        newButton->setStyleSheet(buttonStyleSmall);
        newButton->setCursor(Qt::PointingHandCursor);

        editButton = new QPushButton(centralwidget);
        editButton->setObjectName(QString::fromUtf8("editButton"));
        editButton->setGeometry(QRect(left_ruler, top_ruler + 380, small_item_width, small_item_width/4));
        editButton->setStyleSheet(buttonStyleSmall);
        editButton->setCursor(Qt::PointingHandCursor);

        LauncherWindow->setCentralWidget(centralwidget);
        retranslateUi(LauncherWindow);
        QMetaObject::connectSlotsByName(LauncherWindow);
    } // setupUi

    void retranslateUi(QMainWindow *LauncherWindow)
    {
        LauncherWindow->setWindowTitle(QCoreApplication::translate("LauncherWindow",
            "OpenSpace Launcher", nullptr));
        newButton->setText(QCoreApplication::translate("LauncherWindow",
            "New", nullptr));
        qBtn_start->setText(QCoreApplication::translate("LauncherWindow",
            "START", nullptr));
        editButton->setText(QCoreApplication::translate("LauncherWindow",
            "Edit", nullptr));
        backgroundImage->setText(QString());
        logolabel->setText(QString());
        labelChoose->setText(QCoreApplication::translate("LauncherWindow",
            "<html><head/><body><p><span style=\" font-size:12pt; font-weight:600; "
            "color:#ffffff;\">Choose Profile</span></p></body></html>", nullptr));
        labelOptions->setText(QCoreApplication::translate("LauncherWindow",
            "<html><head/><body><p><span style=\" font-size:12pt; font-weight:600; "
            "color:#ffffff;\">Window Options</span></p></body></html>", nullptr));
    } // retranslateUi

};

namespace Ui {
    class LauncherWindow: public Ui_LauncherWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // __OPENSPACE_UI_LAUNCHER___UI_LAUNCHERWINDOW___H__
