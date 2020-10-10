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

#include <openspace/scene/profile.h>
#include "launcherwindow.h"
#include "profileedit.h"
#include "./ui_launcherwindow.h"
#include <QPixmap>
#include <QKeyEvent>
#include "filesystemaccess.h"
#include <openspace/engine/configuration.h>
#include <sstream>
#include <iostream>
#include <random>

LauncherWindow::LauncherWindow(std::string basePath, bool profileEnabled,
                               openspace::configuration::Configuration& globalConfig,
                               bool sgctConfigEnabled, std::string sgctConfigName,
                               QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::LauncherWindow)
    , _fileAccess_profiles(".profile", {"./"}, true, false)
    , _fileAccess_winConfigs(".xml", {"./"}, true, false)
    , _filesystemAccess(".asset",
                        {"scene", "global", "customization", "examples", "util"},
                        true, true)
    , _basePath(QString::fromUtf8(basePath.c_str()))
    , _profileChangeAllowed(profileEnabled)
    , _sgctConfigChangeAllowed(sgctConfigEnabled)
    , _globalConfig(globalConfig)
{
    ui->setupUi(this);
    QString logoPath = _basePath + "/data/images/openspace-horiz-logo-small.png";
     QPixmap pix(logoPath);
    ui->logolabel->setPixmap(pix);
    connect(ui->qBtn_start, SIGNAL(released()), this, SLOT(startOpenSpace()));
    connect(ui->newButton, SIGNAL(released()), this, SLOT(openWindow_new()));
    connect(ui->editButton, SIGNAL(released()), this, SLOT(openWindow_edit()));
    _reportAssetsInFilesystem = _filesystemAccess.useQtFileSystemModelToTraverseDir(
        QString(basePath.c_str()) + "/data/assets");
    populateProfilesList(QString(globalConfig.profile.c_str()));
    ui->comboBoxProfiles->setDisabled(!_profileChangeAllowed);
    populateWindowConfigsList(QString(sgctConfigName.c_str()));
    ui->comboBoxWindowConfigs->setDisabled(!_sgctConfigChangeAllowed);
    _fullyConfiguredViaCliArgs = (!profileEnabled && !sgctConfigEnabled);

    std::random_device rd;
    std::mt19937 rng(rd());
    std::uniform_int_distribution<int> uni(0, 4);
    auto random_integer = uni(rng);
    QString filename = QString::fromStdString("/data/images/profile" + std::to_string(random_integer) + ".png");
    QString bgpath = QDir::fromNativeSeparators(_basePath)+ filename;
    QPixmap bgpix(bgpath);
    ui->backgroundImage->setPixmap(bgpix);
}

void LauncherWindow::populateProfilesList(QString preset) {
    for (int i = 0; i < ui->comboBoxProfiles->count(); ++i) {
        ui->comboBoxProfiles->removeItem(i);
    }
    std::string reportProfiles = _fileAccess_profiles.useQtFileSystemModelToTraverseDir(
        _basePath + "/data/profiles");
    std::stringstream instream(reportProfiles);
    std::string iline;
    QStringList profilesListLine;
    while (std::getline(instream, iline)) {
        if (ui->comboBoxProfiles->findText(QString(iline.c_str())) == -1) {
            ui->comboBoxProfiles->addItem(iline.c_str());
        }
    }
    if (preset.length() > 0) {
        int presetMatchIdx = ui->comboBoxProfiles->findText(preset);
        if (presetMatchIdx != -1) {
            ui->comboBoxProfiles->setCurrentIndex(presetMatchIdx);
        }
    }
}

void LauncherWindow::populateWindowConfigsList(QString preset) {
    std::string reportConfigs = _fileAccess_winConfigs.useQtFileSystemModelToTraverseDir(
        _basePath + "/config");
    std::stringstream instream(reportConfigs);
    std::string iline;
    QStringList windowConfigsListLine;
    while (std::getline(instream, iline)) {
        windowConfigsListLine << iline.c_str();
    }
    ui->comboBoxWindowConfigs->addItems(windowConfigsListLine);
    if (preset.length() > 0) {
        int presetMatchIdx = ui->comboBoxWindowConfigs->findText(preset);
        if (presetMatchIdx != -1) {
            ui->comboBoxWindowConfigs->setCurrentIndex(presetMatchIdx);
        }
        else {
            ui->comboBoxWindowConfigs->addItem(preset);
            ui->comboBoxWindowConfigs->setCurrentIndex(
                ui->comboBoxWindowConfigs->count() - 1);
        }
    }
}

void LauncherWindow::openWindow_new() {
    QString initialProfileSelection = ui->comboBoxProfiles->currentText();
    openspace::Profile* pData = new openspace::Profile();
    if (pData != nullptr) {
        myEditorWindow = new ProfileEdit(pData, _reportAssetsInFilesystem,
            _globalConfig.profilesReadOnly);
        myEditorWindow->exec();
        if (myEditorWindow->wasSaved()) {
            std::string saveProfilePath = _basePath.toUtf8().constData();
            saveProfilePath += "/data/profiles/";
            saveProfilePath += myEditorWindow->specifiedFilename() + ".profile";
            saveProfileToFile(saveProfilePath, pData);
            populateProfilesList(QString(myEditorWindow->specifiedFilename().c_str()));
        }
        else {
            populateProfilesList(initialProfileSelection);
        }
        delete myEditorWindow;
        delete pData;
    }
}

void LauncherWindow::openWindow_edit() {
    QString initialProfileSelection = ui->comboBoxProfiles->currentText();
    std::string profilePath = _basePath.toUtf8().constData();
    profilePath += "/data/profiles/";
    int selectedProfileIdx = ui->comboBoxProfiles->currentIndex();
    QString profileToSet = ui->comboBoxProfiles->itemText(selectedProfileIdx);
    std::string editProfilePath = profilePath + profileToSet.toUtf8().constData();
    editProfilePath += ".profile";
    openspace::Profile* pData;
    bool validFile = loadProfileFromFile(pData, editProfilePath);
    if (pData != nullptr && validFile) {
        myEditorWindow = new ProfileEdit(pData, _reportAssetsInFilesystem,
            _globalConfig.profilesReadOnly);
        myEditorWindow->setProfileName(profileToSet);
        myEditorWindow->exec();
        if (myEditorWindow->wasSaved()) {
            profilePath += myEditorWindow->specifiedFilename() + ".profile";
            saveProfileToFile(profilePath, pData);
            populateProfilesList(QString(myEditorWindow->specifiedFilename().c_str()));
        }
        else {
            populateProfilesList(initialProfileSelection);
        }
        delete myEditorWindow;
        delete pData;
    }
}

void LauncherWindow::saveProfileToFile(const std::string& path, openspace::Profile* p) {
    std::ofstream outFile;
    try {
        outFile.open(path, std::ofstream::out);
    }
    catch (const std::ofstream::failure& e) {
        displayErrorDialog(fmt::format(
            "Exception opening profile file {} for write: ({})",
            path,
            e.what()
        ));
    }

    try {
        outFile << p->serialize();
    }
    catch (const std::ofstream::failure& e) {
        displayErrorDialog(fmt::format(
            "Data write error to file: {} ({})",
            path,
            e.what()
        ));
    }

    outFile.close();
}

bool LauncherWindow::loadProfileFromFile(openspace::Profile*& p, std::string filename) {
    bool successfulLoad = true;
    std::string content;
    if (filename.length() > 0) {
        std::ifstream inFile;
        try {
            inFile.open(filename, std::ifstream::in);
        }
        catch (const std::ifstream::failure& e) {
            throw ghoul::RuntimeError(fmt::format(
                "Exception opening {} profile for read: ({})",
                filename,
                e.what()
            ));
        }
        std::string line;
        while (std::getline(inFile, line)) {
            content += line;
        }
    }
    try {
        p = new openspace::Profile(content);
    }
    catch (const ghoul::MissingCaseException& e) {
        displayErrorDialog(fmt::format(
            "Missing case exception in {}: {}",
            filename,
            e.what()
        ));
        successfulLoad = false;
    }
    catch (const openspace::Profile::ParsingError& e) {
        displayErrorDialog(fmt::format(
            "ParsingError exception in {}: {}, {}",
            filename,
            e.component,
            e.message
        ));
        successfulLoad = false;
    }
    catch (const ghoul::RuntimeError& e) {
        displayErrorDialog(fmt::format(
            "RuntimeError exception in {}, component {}: {}",
            filename,
            e.component,
            e.message
        ));
        successfulLoad = false;
    }
    return successfulLoad;
}

void LauncherWindow::displayErrorDialog(std::string msg) {
    //New instance of info dialog window
    _myDialog = new errordialog(QString(msg.c_str()));
    _myDialog->exec();
    delete _myDialog;
}

LauncherWindow::~LauncherWindow() {
    delete myEditorWindow;
    delete ui;
}

bool LauncherWindow::wasLaunchSelected() {
    return _launch;
}

bool LauncherWindow::isFullyConfiguredFromCliArgs() {
    return _fullyConfiguredViaCliArgs;
}

std::string LauncherWindow::selectedProfile() {
    return ui->comboBoxProfiles->currentText().toUtf8().constData();
}

std::string LauncherWindow::selectedWindowConfig() {
    return ui->comboBoxWindowConfigs->currentText().toUtf8().constData();
}

void LauncherWindow::startOpenSpace() {
    _launch = true;
    close();
}
