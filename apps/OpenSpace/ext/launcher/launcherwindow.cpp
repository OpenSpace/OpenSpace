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

#include "launcherwindow.h"
#include "editorwindow.h"
#include "./ui_launcherwindow.h"
#include <QPixmap>
#include "filesystemaccess.h"
#include <sstream>


LauncherWindow::LauncherWindow(std::string basePath, QWidget* parent)
    : QMainWindow(parent)
    , ui(new Ui::LauncherWindow)
    , _fileAccess_profiles(".profile", {"./"}, true, false)
    , _fileAccess_winConfigs(".xml", {"./"}, true, false)
    , _basePath(QString::fromUtf8(basePath.c_str()))
{
    ui->setupUi(this);
    QString logoPath = _basePath + "/data/openspace-horiz-logo.png";
    QPixmap pix(logoPath);
    ui->logolabel->setPixmap(pix.scaled(400, 120, Qt::KeepAspectRatio));
    connect(ui->newButton, SIGNAL(released()), this, SLOT(openWindow_new()));
    connect(ui->editButton, SIGNAL(released()), this, SLOT(openWindow_edit()));
    populateProfilesList();
    populateWindowConfigsList();
}

void LauncherWindow::populateProfilesList() {
    std::string reportProfiles = _fileAccess_profiles.useQtFileSystemModelToTraverseDir(
        _basePath + "/data/profiles");
    std::stringstream instream(reportProfiles);
    std::string iline;
    QStringList profilesListLine;
    while (std::getline(instream, iline)) {
        profilesListLine << iline.c_str();
    }
    ui->comboBoxProfiles->addItems(profilesListLine);
}

void LauncherWindow::populateWindowConfigsList() {
    std::string reportConfigs = _fileAccess_winConfigs.useQtFileSystemModelToTraverseDir(
        _basePath + "/config");
    std::stringstream instream(reportConfigs);
    std::string iline;
    QStringList windowConfigsListLine;
    while (std::getline(instream, iline)) {
        windowConfigsListLine << iline.c_str();
    }
    ui->comboBoxWindowConfigs->addItems(windowConfigsListLine);
}

void LauncherWindow::openWindow_new() {
    myEditorWindow = new editorwindow(_basePath + "/data/assets");
    myEditorWindow->exec();
    std::vector<std::string> results = myEditorWindow->parseSelections();
    receiveAssets(results);
}

void LauncherWindow::openWindow_edit() {
    myEditorWindow = new editorwindow(_basePath + "/data/assets");

    int selectedProfileIdx = ui->comboBoxProfiles->currentIndex();
    QString profileToSet = ui->comboBoxProfiles->itemText(selectedProfileIdx);
    myEditorWindow->setProfileName(profileToSet);

    myEditorWindow->exec();
    std::vector<std::string> results = myEditorWindow->parseSelections();
    receiveAssets(results);
}

void LauncherWindow::receiveAssets(std::vector<std::string> results) {
    //std::string windowText;
    //for (std::string line : results) {
    //    windowText += line + "\n";
    //}
    //ui->textEdit->setText(QString::fromUtf8(windowText.c_str()));
}

LauncherWindow::~LauncherWindow() {
    delete ui;
    delete myEditorWindow;
}
