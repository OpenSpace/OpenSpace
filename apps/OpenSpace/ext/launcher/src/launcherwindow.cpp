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

#include "profile/profileedit.h"

#include <openspace/engine/configuration.h>
#include <QComboBox>
#include <QLabel>
#include <QMessageBox>
#include <QPushButton>
#include <filesystem>
#include <iostream>
#include <random>

namespace {
    constexpr const int ScreenWidth = 480;
    constexpr const int ScreenHeight = 640;

    constexpr const int LeftRuler = 40;
    constexpr const int TopRuler = 80;
    constexpr const int ItemWidth = 240;
    constexpr const int ItemHeight = ItemWidth / 4;
    constexpr const int SmallItemWidth = 100;
    constexpr const int SmallItemHeight = SmallItemWidth / 4;

    namespace geometry {
        constexpr const QRect BackgroundImage(0, 0, ScreenWidth, ScreenHeight);
        constexpr const QRect LogoImage(LeftRuler, TopRuler, ItemWidth, ItemHeight);
        constexpr const QRect ChooseLabel(LeftRuler, TopRuler + 80, 151, 24);
        constexpr const QRect ProfileBox(
            LeftRuler, TopRuler + 110, ItemWidth, ItemHeight
        );
        constexpr const QRect OptionsLabel(LeftRuler, TopRuler + 180, 151, 24);
        constexpr const QRect WindowConfigBox(
            LeftRuler, TopRuler + 210, ItemWidth, ItemHeight
        );
        constexpr const QRect StartButton(
            LeftRuler, TopRuler + 290, ItemWidth, ItemHeight
        );
        constexpr const QRect NewButton(
            LeftRuler + 140, TopRuler + 380, SmallItemWidth, SmallItemHeight
        );
        constexpr const QRect EditButton(
            LeftRuler, TopRuler + 380, SmallItemWidth, SmallItemHeight
        );
    } // geometry
} // namespace

using namespace openspace;

LauncherWindow::LauncherWindow(std::string basePath, bool profileEnabled,
                               configuration::Configuration& globalConfig,
                               bool sgctConfigEnabled, std::string sgctConfigName,
                               QWidget* parent)
    : QMainWindow(parent)
    , _basePath(std::move(basePath))
    , _globalConfig(globalConfig)
    , _fullyConfiguredViaCliArgs(!profileEnabled && !sgctConfigEnabled)

{
    Q_INIT_RESOURCE(resources);

    qInstallMessageHandler(
        // Now that the log is enabled and available, we can pipe all Qt messages to that
        [](QtMsgType type, const QMessageLogContext&, const QString& msg) {
            if (type == QtCriticalMsg || type == QtFatalMsg || type == QtSystemMsg) {
                std::cerr << msg.toStdString() << std::endl;
            }
        }
    );

    setWindowTitle("OpenSpace Launcher");
    setFixedSize(ScreenWidth, ScreenHeight);
    setAutoFillBackground(false);

    {
        QFile file(":/qss/launcher.qss");
        file.open(QFile::ReadOnly);
        QString styleSheet = QLatin1String(file.readAll());
        setStyleSheet(styleSheet);
    }

    setCentralWidget(createCentralWidget());




    populateProfilesList(globalConfig.profile);

    _profileBox->setEnabled(profileEnabled);

    populateWindowConfigsList(sgctConfigName);
    _windowConfigBox->setEnabled(sgctConfigEnabled);


    std::string syncPath = globalConfig.pathTokens["SYNC"] + "/http/launcher_images";
    if (std::filesystem::exists(syncPath)) {
        try {
            setBackgroundImage(syncPath);
        }
        catch (const std::exception& e) {
            std::cerr << "Error occurrred while reading background images: " << e.what();
        }
    }
}

QWidget* LauncherWindow::createCentralWidget() {
    QWidget* centralWidget = new QWidget(this);

    _backgroundImage = new QLabel(centralWidget);
    _backgroundImage->setGeometry(geometry::BackgroundImage);

    QLabel* logoImage = new QLabel(centralWidget);
    logoImage->setObjectName("clear");
    logoImage->setGeometry(geometry::LogoImage);
    logoImage->setPixmap(QPixmap(":/images/openspace-horiz-logo-small.png"));

    QLabel* labelChoose = new QLabel("Choose Profile", centralWidget);
    labelChoose->setObjectName("clear");
    labelChoose->setGeometry(geometry::ChooseLabel);
    labelChoose->setObjectName("label_choose");

    _profileBox = new QComboBox(centralWidget);
    _profileBox->setObjectName("config");
    _profileBox->setGeometry(geometry::ProfileBox);

    QLabel* optionsLabel = new QLabel("Window Options", centralWidget);
    optionsLabel->setObjectName("clear");
    optionsLabel->setGeometry(geometry::OptionsLabel);
    optionsLabel->setObjectName("label_options");

    _windowConfigBox = new QComboBox(centralWidget);
    _windowConfigBox->setObjectName("config");
    _windowConfigBox->setGeometry(geometry::WindowConfigBox);

    QPushButton* startButton = new QPushButton("START", centralWidget);
    connect(startButton, &QPushButton::released, this, &LauncherWindow::startOpenSpace);
    startButton->setObjectName("large");
    startButton->setGeometry(geometry::StartButton);
    startButton->setCursor(Qt::PointingHandCursor);

    QPushButton* newButton = new QPushButton("New", centralWidget);
    connect(newButton, &QPushButton::released, this, &LauncherWindow::openWindowNew);
    newButton->setObjectName("small");
    newButton->setGeometry(geometry::NewButton);
    newButton->setCursor(Qt::PointingHandCursor);

    QPushButton* editButton = new QPushButton("Edit", centralWidget);
    connect(editButton, &QPushButton::released, this, &LauncherWindow::openWindowEdit);
    editButton->setObjectName("small");
    editButton->setGeometry(geometry::EditButton);
    editButton->setCursor(Qt::PointingHandCursor);

    return centralWidget;
}

void LauncherWindow::setBackgroundImage(const std::string& syncPath) {
    namespace fs = std::filesystem;

    fs::directory_entry latestVersion;
    for (const fs::directory_entry& p : fs::directory_iterator(syncPath)) {
        if (!p.is_directory()) {
            continue;
        }
        std::string path = p.path().string();
        std::

    }


        bool hasSyncFiles = false;
    QString syncFilePath = QString::fromStdString(
        syncPath + "/http/launcher_images/1/profile1.png"
    );
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
        int random_integer = uni(rng);
        filename = QString::fromStdString(
            "/http/launcher_images/1/profile" + std::to_string(random_integer) + ".png"
        );
        bgpath = QString::fromStdString(syncPath) + filename;
    }
    else {
        bgpath = QString::fromStdString(":/images/launcher-background.png");
    }

    _backgroundImage->setPixmap(QPixmap(bgpath));
}

void LauncherWindow::populateProfilesList(std::string preset) {
    _profileBox->clear();

    FileSystemAccess profiles(".profile", { "./" }, true, false);
    std::string reportProfiles = profiles.useQtFileSystemModelToTraverseDir(
        _basePath + "/data/profiles"
    );
    std::stringstream instream(reportProfiles);
    std::string iline;
    QStringList profilesListLine;
    while (std::getline(instream, iline)) {
        if (_profileBox->findText(QString::fromStdString(iline)) == -1) {
            _profileBox->addItem(QString::fromStdString(iline));
        }
    }
    if (preset.length() > 0) {
        int presetMatchIdx = _profileBox->findText(QString::fromStdString(preset));
        if (presetMatchIdx != -1) {
            _profileBox->setCurrentIndex(presetMatchIdx);
        }
    }
}

void LauncherWindow::populateWindowConfigsList(std::string preset) {
    FileSystemAccess configs(".xml", { "./" }, true, false);
    std::string reportConfigs = configs.useQtFileSystemModelToTraverseDir(
        _basePath + "/config"
    );
    std::stringstream instream(reportConfigs);
    std::string iline;
    QStringList windowConfigsListLine;
    while (std::getline(instream, iline)) {
        windowConfigsListLine << QString::fromStdString(iline);
    }
    _windowConfigBox->addItems(windowConfigsListLine);
    if (preset.length() > 0) {
        int presetMatchIdx = _windowConfigBox->findText(QString::fromStdString(preset));
        if (presetMatchIdx != -1) {
            _windowConfigBox->setCurrentIndex(presetMatchIdx);
        }
        else {
            _windowConfigBox->addItem(QString::fromStdString(preset));
            _windowConfigBox->setCurrentIndex(_windowConfigBox->count() - 1);
        }
    }
}

void LauncherWindow::openWindowNew() {
    FileSystemAccess assets(
        ".asset", { "scene", "global", "customization", "examples", "util" }, true, true
    );
    std::string assetList = assets.useQtFileSystemModelToTraverseDir(
        _basePath + "/data/assets"
    );

    std::string initialProfileSelection = _profileBox->currentText().toStdString();
    Profile profile;
    ProfileEdit editor(
        profile,
        assetList,
        _globalConfig.readOnlyProfiles,
        this
    );
    editor.exec();
    if (editor.wasSaved()) {
        std::string saveProfilePath = _basePath + "/data/profiles/";
        saveProfilePath += editor.specifiedFilename() + ".profile";
        saveProfileToFile(saveProfilePath, profile);
        populateProfilesList(editor.specifiedFilename());
    }
    else {
        populateProfilesList(initialProfileSelection);
    }
}

void LauncherWindow::openWindowEdit() {
    FileSystemAccess assets(
        ".asset", { "scene", "global", "customization", "examples", "util" }, true, true
    );
    std::string assetList = assets.useQtFileSystemModelToTraverseDir(
        _basePath + "/data/assets"
    );

    std::string initialProfileSelection = _profileBox->currentText().toStdString();
    std::string profilePath = _basePath + "/data/profiles/";
    int selectedProfileIdx = _profileBox->currentIndex();
    QString profileToSet = _profileBox->itemText(selectedProfileIdx);
    std::string editProfilePath = profilePath + profileToSet.toStdString() + ".profile";

    std::optional<Profile> profile = loadProfileFromFile(editProfilePath);
    if (profile.has_value()) {
        ProfileEdit editor(
            *profile,
            assetList,
            _globalConfig.readOnlyProfiles,
            this
        );
        editor.setProfileName(profileToSet);
        editor.exec();
        if (editor.wasSaved()) {
            profilePath += editor.specifiedFilename() + ".profile";
            saveProfileToFile(profilePath, *profile);
            populateProfilesList(editor.specifiedFilename());
        }
        else {
            populateProfilesList(initialProfileSelection);
        }
    }
}

void LauncherWindow::saveProfileToFile(const std::string& path, const Profile& p) {
    std::ofstream outFile;
    try {
        outFile.open(path, std::ofstream::out);
        outFile << p.serialize();
    }
    catch (const std::ofstream::failure& e) {
        QMessageBox::critical(
            this,
            "Exception",
            QString::fromStdString(fmt::format(
                "Error writing data to file: {} ({})", path, e.what()
            ))
        );
    }
}

std::optional<Profile> LauncherWindow::loadProfileFromFile(std::string filename) {
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
    std::string content;
    std::string line;
    while (std::getline(inFile, line)) {
        content += line;
    }
    try {
        return Profile(content);
    }
    catch (const Profile::ParsingError& e) {
        QMessageBox::critical(
            this,
            "Exception",
            QString::fromStdString(fmt::format(
                "ParsingError exception in {}: {}, {}", filename, e.component, e.message
            ))
        );
        return std::nullopt;
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
        return std::nullopt;
    }
}

bool LauncherWindow::wasLaunchSelected() const {
    return _launch;
}

bool LauncherWindow::isFullyConfiguredFromCliArgs() const {
    return _fullyConfiguredViaCliArgs;
}

std::string LauncherWindow::selectedProfile() const {
    return _profileBox->currentText().toStdString();
}

std::string LauncherWindow::selectedWindowConfig() const {
    return _windowConfigBox->currentText().toStdString();
}

void LauncherWindow::startOpenSpace() {
    _launch = true;
    close();
}
