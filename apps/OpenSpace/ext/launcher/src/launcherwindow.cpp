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
#include <QPixmap>
#include <QKeyEvent>
#include "filesystemaccess.h"
#include <openspace/engine/configuration.h>
#include <sstream>
#include <iostream>
#include <QMessageBox>
#include <random>
#include <QLabel>
#include <QComboBox>
#include <QPushButton>

namespace {
    constexpr const int ScreenWidth = 480;
    constexpr const int ScreenHeight = 640;

    constexpr const int LeftRuler = 40;
    constexpr const int TopRuler = 80;
    constexpr const int ItemWidth = 240;
    constexpr const int SmallItemWidth = 100;
} // namespace

LauncherWindow::LauncherWindow(std::string basePath, bool profileEnabled,
                               openspace::configuration::Configuration& globalConfig,
                               bool sgctConfigEnabled, std::string sgctConfigName,
                               QWidget *parent)
    : QMainWindow(parent)
    , _fileAccessProfiles(".profile", { "./" }, true, false)
    , _fileAccessWinConfigs(".xml", { "./" }, true, false)
    , _filesystemAccess(".asset",
                        {"scene", "global", "customization", "examples", "util"},
                        true, true)
    , _basePath(QString::fromUtf8(basePath.c_str()))
    , _profileChangeAllowed(profileEnabled)
    , _sgctConfigChangeAllowed(sgctConfigEnabled)
    , _globalConfig(globalConfig)
{
    Q_INIT_RESOURCE(resources);

    setWindowTitle("OpenSpace Launcher");
    setFixedSize(ScreenWidth, ScreenHeight);
    setAutoFillBackground(false);

    QFile file(":/qss/launcher.qss");
    file.open(QFile::ReadOnly);
    QString styleSheet = QLatin1String(file.readAll());
    setStyleSheet(styleSheet);


    QWidget* centralWidget = new QWidget(this);

    QLabel* backgroundImage = new QLabel(centralWidget);
    backgroundImage->setGeometry(QRect(0, 0, ScreenWidth, ScreenHeight));

    QLabel* logoImage = new QLabel(centralWidget);
    logoImage->setObjectName("clear");
    logoImage->setGeometry(QRect(LeftRuler, TopRuler, ItemWidth, ItemWidth / 4));
    logoImage->setPixmap(QPixmap(":/images/openspace-horiz-logo-small.png"));

    QLabel* labelChoose = new QLabel("Choose Profile", centralWidget);
    labelChoose->setObjectName("clear");
    labelChoose->setGeometry(QRect(LeftRuler, TopRuler + 80, 151, 24));

    _profileBox = new QComboBox(centralWidget);
    _profileBox->setObjectName("config");
    _profileBox->setGeometry(QRect(LeftRuler, TopRuler + 110, ItemWidth, ItemWidth / 4));

    QLabel* optionsLabel = new QLabel("Window Options", centralWidget);
    optionsLabel->setObjectName("clear");
    optionsLabel->setGeometry(QRect(LeftRuler, TopRuler + 180, 151, 24));

    _windowConfigBox = new QComboBox(centralWidget);
    _windowConfigBox->setObjectName("config");
    _windowConfigBox->setGeometry(QRect(LeftRuler, TopRuler + 210, ItemWidth, ItemWidth / 4));

    QPushButton* startButton = new QPushButton("START", centralWidget);
    connect(startButton, &QPushButton::released, this, &LauncherWindow::startOpenSpace);
    startButton->setObjectName("large");
    startButton->setGeometry(QRect(LeftRuler, TopRuler + 290, ItemWidth, ItemWidth / 4));
    startButton->setCursor(Qt::PointingHandCursor);
    
    QPushButton* newButton = new QPushButton("New", centralWidget);
    connect(newButton, &QPushButton::released, this, &LauncherWindow::openWindow_new);
    newButton->setObjectName("small");
    newButton->setGeometry(QRect(LeftRuler + 140, TopRuler + 380, SmallItemWidth, SmallItemWidth / 4));
    newButton->setCursor(Qt::PointingHandCursor);

    QPushButton* editButton = new QPushButton("Edit", centralWidget);
    connect(editButton, &QPushButton::released, this, &LauncherWindow::openWindow_edit);
    editButton->setObjectName("small");
    editButton->setGeometry(QRect(LeftRuler, TopRuler + 380, SmallItemWidth, SmallItemWidth / 4));
    editButton->setCursor(Qt::PointingHandCursor);

    setCentralWidget(centralWidget);





    _reportAssetsInFilesystem = _filesystemAccess.useQtFileSystemModelToTraverseDir(
        QString(basePath.c_str()) + "/data/assets");
    populateProfilesList(QString(globalConfig.profile.c_str()));

    _profileBox->setDisabled(!_profileChangeAllowed);

    populateWindowConfigsList(QString(sgctConfigName.c_str()));
    _windowConfigBox->setDisabled(!_sgctConfigChangeAllowed);
    _fullyConfiguredViaCliArgs = (!profileEnabled && !sgctConfigEnabled);

    bool hasSyncFiles = false;
    QString syncFilePath = QString(globalConfig.pathTokens["SYNC"].c_str()) + "/http/launcher_images/1/profile1.png";
    QFileInfo check_file(syncFilePath);
    // check if file exists and if yes: Is it really a file and no directory?
    if (check_file.exists() && check_file.isFile()) {
        hasSyncFiles = true;
    }


    QString filename;
    QString bgpath;
    if (hasSyncFiles) {
        std::random_device rd;
        std::mt19937 rng(rd());
        std::uniform_int_distribution<int> uni(0, 4);
        auto random_integer = uni(rng);
        filename = QString::fromStdString("/http/launcher_images/1/profile" + std::to_string(random_integer) + ".png");
        bgpath = QString(globalConfig.pathTokens["SYNC"].c_str()) + filename;
    }
    else {
        bgpath = QString::fromStdString(":/images/launcher-background.png");
    }

    backgroundImage->setPixmap(QPixmap(bgpath));
}

void LauncherWindow::populateProfilesList(QString preset) {
    for (int i = 0; i < _profileBox->count(); ++i) {
        _profileBox->removeItem(i);
    }
    std::string reportProfiles = _fileAccessProfiles.useQtFileSystemModelToTraverseDir(
        _basePath + "/data/profiles");
    std::stringstream instream(reportProfiles);
    std::string iline;
    QStringList profilesListLine;
    while (std::getline(instream, iline)) {
        if (_profileBox->findText(QString(iline.c_str())) == -1) {
            _profileBox->addItem(iline.c_str());
        }
    }
    if (preset.length() > 0) {
        int presetMatchIdx = _profileBox->findText(preset);
        if (presetMatchIdx != -1) {
            _profileBox->setCurrentIndex(presetMatchIdx);
        }
    }
}

void LauncherWindow::populateWindowConfigsList(QString preset) {
    std::string reportConfigs = _fileAccessWinConfigs.useQtFileSystemModelToTraverseDir(
        _basePath + "/config");
    std::stringstream instream(reportConfigs);
    std::string iline;
    QStringList windowConfigsListLine;
    while (std::getline(instream, iline)) {
        windowConfigsListLine << iline.c_str();
    }
    _windowConfigBox->addItems(windowConfigsListLine);
    if (preset.length() > 0) {
        int presetMatchIdx = _windowConfigBox->findText(preset);
        if (presetMatchIdx != -1) {
            _windowConfigBox->setCurrentIndex(presetMatchIdx);
        }
        else {
            _windowConfigBox->addItem(preset);
            _windowConfigBox->setCurrentIndex(_windowConfigBox->count() - 1);
        }
    }
}

void LauncherWindow::openWindow_new() {
    QString initialProfileSelection = _profileBox->currentText();
    openspace::Profile* pData = new openspace::Profile();
    if (pData != nullptr) {
        myEditorWindow = new ProfileEdit(pData, _reportAssetsInFilesystem,
            _globalConfig.profilesReadOnly, this);
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
    QString initialProfileSelection = _profileBox->currentText();
    std::string profilePath = _basePath.toUtf8().constData();
    profilePath += "/data/profiles/";
    int selectedProfileIdx = _profileBox->currentIndex();
    QString profileToSet = _profileBox->itemText(selectedProfileIdx);
    std::string editProfilePath = profilePath + profileToSet.toUtf8().constData();
    editProfilePath += ".profile";
    openspace::Profile* pData;
    bool validFile = loadProfileFromFile(pData, editProfilePath);
    if (pData != nullptr && validFile) {
        myEditorWindow = new ProfileEdit(pData, _reportAssetsInFilesystem,
            _globalConfig.profilesReadOnly, this);
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
        QMessageBox::critical(
            this,
            "Exception",
            QString::fromStdString(fmt::format(
                "Exception opening profile file {} for write: ({})", path, e.what()
            ))
        );
    }

    try {
        outFile << p->serialize();
    }
    catch (const std::ofstream::failure& e) {
        QMessageBox::critical(
            this,
            "Exception",
            QString::fromStdString(fmt::format(
                "Data write error to file: {} ({})", path, e.what()
            ))
        );
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
    catch (const openspace::Profile::ParsingError& e) {
        QMessageBox::critical(
            this,
            "Exception",
            QString::fromStdString(fmt::format(
                "ParsingError exception in {}: {}, {}", filename, e.component, e.message
            ))
        );
        successfulLoad = false;
    }
    catch (const ghoul::RuntimeError& e) {
        QMessageBox::critical(
            this,
            "Exception",
            QString::fromStdString(fmt::format(
                "RuntimeError exception in {}, component {}: {}",
                filename, e.component, e.message
            ))
        );
        successfulLoad = false;
    }
    return successfulLoad;
}

LauncherWindow::~LauncherWindow() {
    delete myEditorWindow;
}

bool LauncherWindow::wasLaunchSelected() {
    return _launch;
}

bool LauncherWindow::isFullyConfiguredFromCliArgs() {
    return _fullyConfiguredViaCliArgs;
}

std::string LauncherWindow::selectedProfile() {
    return _profileBox->currentText().toUtf8().constData();
}

std::string LauncherWindow::selectedWindowConfig() {
    return _windowConfigBox->currentText().toUtf8().constData();
}

void LauncherWindow::startOpenSpace() {
    _launch = true;
    close();
}
