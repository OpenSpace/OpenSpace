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
#include <ghoul/filesystem/filesystem.h>
#include <QComboBox>
#include <QFile>
#include <QLabel>
#include <QMessageBox>
#include <QPushButton>
#include <filesystem>
#include <iostream>
#include <random>

using namespace openspace;

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

    std::optional<Profile> loadProfileFromFile(QWidget* parent, std::string filename) {
        std::ifstream inFile;
        try {
            inFile.open(filename, std::ifstream::in);
        }
        catch (const std::ifstream::failure& e) {
            throw ghoul::RuntimeError(fmt::format(
                "Exception opening {} profile for read: {}", filename, e.what()
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
                parent,
                "Exception",
                QString::fromStdString(fmt::format(
                    "ParsingError exception in {}: {}, {}",
                    filename, e.component, e.message
                ))
            );
            return std::nullopt;
        }
        catch (const ghoul::RuntimeError& e) {
            QMessageBox::critical(
                parent,
                "Exception",
                QString::fromStdString(fmt::format(
                    "RuntimeError exception in {}, component {}: {}",
                    filename, e.component, e.message
                ))
            );
            return std::nullopt;
        }
    }

    void saveProfile(QWidget* parent, const std::string& path, const Profile& p) {
        std::ofstream outFile;
        try {
            outFile.open(path, std::ofstream::out);
            outFile << p.serialize();
        }
        catch (const std::ofstream::failure& e) {
            QMessageBox::critical(
                parent,
                "Exception",
                QString::fromStdString(fmt::format(
                    "Error writing data to file: {} ({})", path, e.what()
                ))
            );
        }
    }
} // namespace

using namespace openspace;

LauncherWindow::LauncherWindow(bool profileEnabled,
                               const configuration::Configuration& globalConfig,
                               bool sgctConfigEnabled, std::string sgctConfigName,
                               QWidget* parent)
    : QMainWindow(parent)
    , _assetPath(absPath(globalConfig.pathTokens.at("ASSETS")) + '/')
    , _configPath(absPath(globalConfig.pathTokens.at("CONFIG")) + '/')
    , _profilePath(absPath(globalConfig.pathTokens.at("PROFILES")) + '/')
    , _readOnlyProfiles(globalConfig.readOnlyProfiles)
{
    Q_INIT_RESOURCE(resources);

    qInstallMessageHandler(
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


    std::string p = absPath(globalConfig.pathTokens.at("SYNC") + "/http/launcher_images");
    if (std::filesystem::exists(p)) {
        try {
            setBackgroundImage(p);
        }
        catch (const std::exception& e) {
            std::cerr << "Error occurrred while reading background images: " << e.what();
        }
    }
}

QWidget* LauncherWindow::createCentralWidget() {
    QWidget* centralWidget = new QWidget;

    _backgroundImage = new QLabel(centralWidget);
    _backgroundImage->setGeometry(geometry::BackgroundImage);
    _backgroundImage->setPixmap(QPixmap(":/images/launcher-background.png"));

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
    connect(
        startButton, &QPushButton::released,
        [this]() {
            _shouldLaunch = true;
            close();
        }
    );
    startButton->setObjectName("large");
    startButton->setGeometry(geometry::StartButton);
    startButton->setCursor(Qt::PointingHandCursor);

    QPushButton* newButton = new QPushButton("New", centralWidget);
    connect(
        newButton, &QPushButton::released,
        [this]() {
            openProfileEditor("");
        }
    );
    newButton->setObjectName("small");
    newButton->setGeometry(geometry::NewButton);
    newButton->setCursor(Qt::PointingHandCursor);

    QPushButton* editButton = new QPushButton("Edit", centralWidget);
    connect(
        editButton, &QPushButton::released,
        [this]() {
            const std::string selection = _profileBox->currentText().toStdString();
            openProfileEditor(selection);
        }
    );
    editButton->setObjectName("small");
    editButton->setGeometry(geometry::EditButton);
    editButton->setCursor(Qt::PointingHandCursor);

    return centralWidget;
}

void LauncherWindow::setBackgroundImage(const std::string& syncPath) {
    namespace fs = std::filesystem;

    // First, we iterate through all folders in the launcher_images sync folder and we get
    // the folder with the highest number
    struct {
        fs::directory_entry path;
        int version = -1;
    } latest;
    for (const fs::directory_entry& p : fs::directory_iterator(syncPath)) {
        if (!p.is_directory()) {
            continue;
        }
        const std::string versionStr = p.path().stem().string();
        // All folder names in the sync folder should only be a digit, so we should be
        // find to just convert it here
        const int version = std::stoi(versionStr);

        if (version > latest.version) {
            latest.version = version;
            latest.path = p;
        }
    }

    if (latest.version == -1) {
        // The sync folder existed, but nothing was in there. Kinda weird, but still
        return;
    }

    // Now we know which folder to use, we will pick an image at random
    std::vector<std::string> files;
    for (const fs::directory_entry& p : fs::directory_iterator(latest.path)) {
        files.push_back(p.path().string());
    }
    std::random_device rd;
    std::mt19937 g(rd());
    std::shuffle(files.begin(), files.end(), g);
    // We know there has to be at least one folder, so it's fine to just pick the first
    std::string image = files.front();
    _backgroundImage->setPixmap(QPixmap(QString::fromStdString(image)));
}

void LauncherWindow::populateProfilesList(std::string preset) {
    namespace fs = std::filesystem;
    
    _profileBox->clear();

    // Add all the files with the .profile extension to the dropdown
    for (const fs::directory_entry& p : fs::directory_iterator(_profilePath)) {
        if (p.path().extension() != ".profile") {
            continue;
        }
        _profileBox->addItem(QString::fromStdString(p.path().stem().string()));
    }

    // Try to find the requested profile and set it as the current one
    const int idx = _profileBox->findText(QString::fromStdString(std::move(preset)));
    if (idx != -1) {
        _profileBox->setCurrentIndex(idx);

    }
}

void LauncherWindow::populateWindowConfigsList(std::string preset) {
    namespace fs = std::filesystem;

    _windowConfigBox->clear();
    // Add all the files with the .xml extension to the dropdown
    for (const fs::directory_entry& p : fs::directory_iterator(_configPath)) {
        if (p.path().extension() != ".xml") {
            continue;
        }
        _windowConfigBox->addItem(QString::fromStdString(p.path().stem().string()));
    }

    // Try to find the requested configuration file and set it as the current one. As we
    // have support for function-generated configuration files that will not be in the
    // list we need to add a preset that doesn't exist a file for
    const int idx = _windowConfigBox->findText(QString::fromStdString(std::move(preset)));
    if (idx != -1) {
        _windowConfigBox->setCurrentIndex(idx);
    }
    else {
        // Add the requested preset at the top
        _windowConfigBox->insertItem(0, QString::fromStdString(preset));
        _windowConfigBox->setCurrentIndex(0);
    }
}

void LauncherWindow::openProfileEditor(const std::string& profile) {
    std::optional<Profile> p;
    if (profile.empty()) {
        // If the requested profile is the empty string, then we want to create a new one

        p = Profile();
    }
    else {
        // Otherwise, we want to load that profile

        std::string fullProfilePath = _profilePath + profile + ".profile";
        p = loadProfileFromFile(this, fullProfilePath);
        if (!p.has_value()) {
            return;
        }
    }

    ProfileEdit editor(*p, profile, _assetPath, _profilePath, _readOnlyProfiles, this);
    editor.exec();
    if (editor.wasSaved()) {
        const std::string path = _profilePath + editor.specifiedFilename() + ".profile";
        saveProfile(this, path, *p);
        populateProfilesList(editor.specifiedFilename());
    }
    else {
        const std::string current = _profileBox->currentText().toStdString();
        populateProfilesList(current);
    }
}

bool LauncherWindow::wasLaunchSelected() const {
    return _shouldLaunch;
}

std::string LauncherWindow::selectedProfile() const {
    return _profileBox->currentText().toStdString();
}

std::string LauncherWindow::selectedWindowConfig() const {
    return _windowConfigBox->currentText().toStdString();
}
