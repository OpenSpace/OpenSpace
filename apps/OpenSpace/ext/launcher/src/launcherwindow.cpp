/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include "settingsdialog.h"

#include <openspace/engine/configuration.h>
#include <openspace/openspace.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <sgct/readconfig.h>
#include <QComboBox>
#include <QFile>
#include <QLabel>
#include <QMessageBox>
#include <QPushButton>
#include <QStandardItemModel>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <random>

#ifdef WIN32
#include <Windows.h>
#endif // WIN32

using namespace openspace;

namespace {
    constexpr int ScreenWidth = 480;
    constexpr int ScreenHeight = 640;

    constexpr int LeftRuler = 40;
    constexpr int TopRuler = 80;
    constexpr int ItemWidth = 260;
    constexpr int ItemHeight = ItemWidth / 4;
    constexpr int SmallItemWidth = 100;
    constexpr int SmallItemHeight = SmallItemWidth / 4;

    constexpr int SettingsIconSize = 35;

    namespace geometry {
        constexpr QRect BackgroundImage(0, 0, ScreenWidth, ScreenHeight);
        constexpr QRect LogoImage(LeftRuler, TopRuler, ItemWidth, ItemHeight);
        constexpr QRect ChooseLabel(LeftRuler, TopRuler + 80, 151, 24);
        constexpr QRect ProfileBox(LeftRuler, TopRuler + 110, ItemWidth, ItemHeight);
        constexpr QRect NewProfileButton(
            LeftRuler + 160, TopRuler + 180, SmallItemWidth, SmallItemHeight
        );
        constexpr QRect EditProfileButton(
            LeftRuler, TopRuler + 180, SmallItemWidth, SmallItemHeight
        );
        constexpr QRect OptionsLabel(LeftRuler, TopRuler + 230, 151, 24);
        constexpr QRect WindowConfigBox(LeftRuler, TopRuler + 260, ItemWidth, ItemHeight);
        constexpr QRect NewWindowButton(
            LeftRuler + 160, TopRuler + 330, SmallItemWidth, SmallItemHeight
        );
        constexpr QRect EditWindowButton(
            LeftRuler, TopRuler + 330, SmallItemWidth, SmallItemHeight
        );
        constexpr QRect StartButton(
            LeftRuler, TopRuler + 400, ItemWidth, ItemHeight
        );
        constexpr QRect VersionString(
            5, ScreenHeight - SmallItemHeight, ItemWidth, SmallItemHeight
        );
        constexpr QRect SettingsButton(
            ScreenWidth - SettingsIconSize - 5,
            ScreenHeight - SettingsIconSize - 5,
            SettingsIconSize,
            SettingsIconSize
        );
    } // namespace geometry

    std::optional<Profile> loadProfileFromFile(QWidget* parent,
                                               std::filesystem::path filename)
    {
        // Verify that the file actually exists
        if (!std::filesystem::exists(filename)) {
            QMessageBox::critical(
                parent,
                "Exception",
                QString::fromStdString(std::format(
                    "Could not open profile file '{}'", filename
                ))
            );

            return std::nullopt;
        }

        try {
            return Profile(filename);
        }
        catch (const Profile::ParsingError& e) {
            QMessageBox::critical(
                parent,
                "Exception",
                QString::fromStdString(std::format(
                    "ParsingError exception in '{}': {}, {}",
                    filename, e.component, e.message
                ))
            );
            return std::nullopt;
        }
        catch (const ghoul::RuntimeError& e) {
            QMessageBox::critical(
                parent,
                "Exception",
                QString::fromStdString(std::format(
                    "RuntimeError exception in '{}', component {}: {}",
                    filename, e.component, e.message
                ))
            );
            return std::nullopt;
        }
    }

    void saveProfile(QWidget* parent, const std::string& path, const Profile& p) {
        std::ofstream outFile;
        outFile.exceptions(std::ofstream::badbit | std::ofstream::failbit);
        try {
            outFile.open(path, std::ofstream::out);
            outFile << p.serialize();
        }
        catch (const std::ofstream::failure& e) {
#ifdef WIN32
            if (std::filesystem::exists(path)) {
                // Check if the file is hidden, since that causes ofstream to fail
                DWORD res = GetFileAttributesA(path.c_str());
                if (res & FILE_ATTRIBUTE_HIDDEN) {
                    QMessageBox::critical(
                        parent,
                        "Exception",
                        QString::fromStdString(std::format(
                            "Error writing data to file '{}' as file is marked hidden",
                            path
                        ))
                    );
                    return;
                }
            }
#endif // WIN32
            QMessageBox::critical(
                parent,
                "Exception",
                QString::fromStdString(std::format(
                    "Error writing data to file '{}': {}", path, e.what()
                ))
            );
        }
    }

    void saveWindowConfig(QWidget* parent, const std::filesystem::path& path,
                          const sgct::config::Cluster& cluster)
    {
        std::ofstream outFile;
        try {
            outFile.open(path, std::ofstream::out);
            sgct::config::GeneratorVersion genEntry = versionMin;
            outFile << sgct::serializeConfig(
                cluster,
                genEntry
            );
        }
        catch (const std::ofstream::failure& e) {
            QMessageBox::critical(
                parent,
                "Exception",
                QString::fromStdString(std::format(
                    "Error writing data to file '{}': {}", path, e.what()
                ))
            );
        }
    }

    bool versionCheck(sgct::config::GeneratorVersion& v) {
        return
            v.versionCheck(versionMin) ||
            v == versionLegacy18 ||
            v == versionLegacy19;
    }
} // namespace

using namespace openspace;

LauncherWindow::LauncherWindow(bool profileEnabled,
                               const Configuration& globalConfig,
                               bool sgctConfigEnabled, std::string sgctConfigName,
                               QWidget* parent)
    : QMainWindow(parent)
    , _assetPath(absPath(globalConfig.pathTokens.at("ASSETS")) / "")
    , _userAssetPath(absPath(globalConfig.pathTokens.at("USER_ASSETS")) / "")
    , _configPath(absPath(globalConfig.pathTokens.at("CONFIG")) / "")
    , _userConfigPath(absPath(globalConfig.pathTokens.at("USER_CONFIG")) / "")
    , _profilePath(absPath(globalConfig.pathTokens.at("PROFILES")) / "")
    , _userProfilePath(absPath(globalConfig.pathTokens.at("USER_PROFILES")) / "")
    , _sgctConfigName(std::move(sgctConfigName))
{
    Q_INIT_RESOURCE(resources);

    qInstallMessageHandler(
        [](QtMsgType type, const QMessageLogContext&, const QString& msg) {
            if (type == QtCriticalMsg || type == QtFatalMsg || type == QtSystemMsg) {
                std::cerr << msg.toStdString() << '\n';
            }
        }
    );

    setWindowTitle("OpenSpace Launcher");
    setFixedSize(ScreenWidth, ScreenHeight);
    setAutoFillBackground(false);

    {
        QFile file(":/qss/launcher.qss");
        file.open(QFile::ReadOnly);
        const QString styleSheet = QLatin1String(file.readAll());
        setStyleSheet(styleSheet);
    }

    setCentralWidget(createCentralWidget());

    populateProfilesList(globalConfig.profile);
    _profileBox->setEnabled(profileEnabled);

    _windowConfigBox->setEnabled(sgctConfigEnabled);
    populateWindowConfigsList(_sgctConfigName);
    // Trigger currentIndexChanged so the preview file read is performed
    _windowConfigBox->currentIndexChanged(_windowConfigBox->currentIndex());

    const std::filesystem::path p = absPath(
        globalConfig.pathTokens.at("SYNC") + "/http/launcher_images"
    );
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
            if (_profileBox->currentText().isEmpty()) {
                QMessageBox::critical(
                    this,
                    "Empty Profile",
                    "Cannot launch with an empty profile"
                );
            }
            else {
                _shouldLaunch = true;
                close();
            }
        }
    );
    startButton->setObjectName("large");
    startButton->setGeometry(geometry::StartButton);
    startButton->setCursor(Qt::PointingHandCursor);

    QPushButton* newProfileButton = new QPushButton("New", centralWidget);
    connect(
        newProfileButton, &QPushButton::released,
        [this]() {
            openProfileEditor("", true);
        }
    );
    newProfileButton->setObjectName("small");
    newProfileButton->setGeometry(geometry::NewProfileButton);
    newProfileButton->setCursor(Qt::PointingHandCursor);

    QPushButton* editProfileButton = new QPushButton("Edit", centralWidget);
    connect(
        editProfileButton, &QPushButton::released,
        [this]() {
            const std::string selection = _profileBox->currentText().toStdString();
            const int selectedIndex = _profileBox->currentIndex();
            const bool isUserProfile = selectedIndex < _userAssetCount;
            openProfileEditor(selection, isUserProfile);
        }
    );
    editProfileButton->setObjectName("small");
    editProfileButton->setGeometry(geometry::EditProfileButton);
    editProfileButton->setCursor(Qt::PointingHandCursor);

    QPushButton* newWindowButton = new QPushButton("New", centralWidget);
    connect(
        newWindowButton, &QPushButton::released,
        [this]() {
            openWindowEditor("", true);
        }
    );
    newWindowButton->setObjectName("small");
    newWindowButton->setGeometry(geometry::NewWindowButton);
    newWindowButton->setCursor(Qt::PointingHandCursor);

    _editWindowButton = new QPushButton("Edit", centralWidget);
    connect(
        _editWindowButton,
        &QPushButton::released,
        [this]() {
            const std::filesystem::path pathSelected = absPath(selectedWindowConfig());
            const bool isUserConfig = isUserConfigSelected();
            const std::string fileSelected = pathSelected.generic_string();
            if (std::filesystem::is_regular_file(pathSelected)) {
                openWindowEditor(fileSelected, isUserConfig);
            }
        }
    );
    _editWindowButton->setVisible(true);
    _editWindowButton->setObjectName("small");
    _editWindowButton->setGeometry(geometry::EditWindowButton);
    _editWindowButton->setCursor(Qt::PointingHandCursor);


    QLabel* versionLabel = new QLabel(centralWidget);
    versionLabel->setVisible(true);
    versionLabel->setText(
        QString::fromStdString(std::string(openspace::OPENSPACE_VERSION_STRING_FULL))
    );
    versionLabel->setObjectName("version-info");
    versionLabel->setGeometry(geometry::VersionString);

    QPushButton* settingsButton = new QPushButton(centralWidget);
    settingsButton->setObjectName("settings");
    settingsButton->setGeometry(geometry::SettingsButton);
    settingsButton->setIconSize(QSize(SettingsIconSize, SettingsIconSize));
    connect(
        settingsButton,
        &QPushButton::released,
        [this]() {
            using namespace openspace;

            Settings settings = loadSettings();

            SettingsDialog dialog(std::move(settings), this);
            connect(
                &dialog,
                &SettingsDialog::saveSettings,
                [this](Settings s) {
                    saveSettings(s, findSettings());

                    if (s.profile.has_value()) {
                        populateProfilesList(*s.profile);
                    }

                    if (s.configuration.has_value()) {
                        populateWindowConfigsList(*s.configuration);
                    }
                }
            );

            dialog.exec();
        }
    );

    return centralWidget;
}

void LauncherWindow::setBackgroundImage(const std::filesystem::path& syncPath) {
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
    std::vector<std::filesystem::path> files;
    for (const fs::directory_entry& p : fs::directory_iterator(latest.path)) {
        files.push_back(p.path());
    }
    std::random_device rd;
    std::mt19937 g(rd());
    std::shuffle(files.begin(), files.end(), g);
    // We know there has to be at least one folder, so it's fine to just pick the first
    while (!files.empty()) {
        const std::filesystem::path& p = files.front();
        if (p.extension() == ".png") {
            // If the top path starts with the png extension, we have found our candidate
            break;
        }
        else {
            // There shouldn't be any non-png images in here, but you never know. So we 
            // just remove non-image files here
            files.erase(files.begin());
        }
    }

    // There better be at least one file left, but just in in case
    if (!files.empty()) {
        std::string image = files.front().string();
        _backgroundImage->setPixmap(QPixmap(QString::fromStdString(image)));
    }
}

void LauncherWindow::populateProfilesList(const std::string& preset) {
    namespace fs = std::filesystem;
    
    _profileBox->clear();
    _userAssetCount = 0;

    if (!std::filesystem::exists(_profilePath)) {
        LINFOC(
            "LauncherWindow",
            std::format("Could not find profile folder '{}'", _profilePath)
        );
        return;
    }

    _profileBox->addItem(QString::fromStdString("--- User Profiles ---"));
    const QStandardItemModel* model = qobject_cast<const QStandardItemModel*>(
        _profileBox->model()
    );
    model->item(_userAssetCount)->setEnabled(false);
    ++_userAssetCount;

    // Add all the files with the .profile extension to the dropdown
    std::vector<fs::directory_entry> profiles;
    for (const fs::directory_entry& p : fs::directory_iterator(_userProfilePath)) {
        if (p.path().extension() != ".profile") {
            continue;
        }
        profiles.push_back(p);
        ++_userAssetCount;
    }
    std::sort(profiles.begin(), profiles.end());
    for (const fs::directory_entry& profile : profiles) {
        const std::filesystem::path& path = profile.path();
        _profileBox->addItem(
            QString::fromStdString(path.stem().string()),
            QString::fromStdString(path.string())
        );

        // Add tooltip
        std::optional<Profile> p = loadProfileFromFile(this, path);
        const int idx = _profileBox->count() - 1;
        if (p.has_value() && (*p).meta.has_value()) {
            const std::optional<std::string>& d = p->meta.value().description;
            if (d.has_value()) {
                // Tooltip has to be 'rich text' to linebreak properly
                const QString tooltip = QString::fromStdString(std::format(
                    "<p>{}</p>", *d
                ));
                _profileBox->setItemData(idx, tooltip, Qt::ToolTipRole);
            }
        }
    }

    _profileBox->addItem(QString::fromStdString("--- OpenSpace Profiles ---"));
    model = qobject_cast<const QStandardItemModel*>(_profileBox->model());
    model->item(_userAssetCount)->setEnabled(false);
    ++_userAssetCount;

    // Add all the files with the .profile extension to the dropdown
    profiles.clear();
    for (const fs::directory_entry& path : fs::directory_iterator(_profilePath)) {
        if (path.path().extension() != ".profile") {
            continue;
        }
        profiles.push_back(path);
    }
    std::sort(profiles.begin(), profiles.end());

    // Add sorted items to list
    for (const fs::directory_entry& profile : profiles) {
        const std::filesystem::path& path = profile.path();
        _profileBox->addItem(
            QString::fromStdString(path.stem().string()),
            QString::fromStdString(path.string())
        );

        // Add toooltip
        std::optional<Profile> p = loadProfileFromFile(this, path);
        const int idx = _profileBox->count() - 1;
        if (p.has_value() && (*p).meta.has_value()) {
            const std::optional<std::string>& d = p->meta.value().description;
            if (d.has_value()) {
                // Tooltip has to be 'rich text' to linebreak properly
                const QString tooltip = QString::fromStdString(std::format(
                    "<p>{}</p>", *d
                ));
                _profileBox->setItemData(idx, tooltip, Qt::ToolTipRole);
            }
        }
    }

    // Try to find the requested profile and set it as the current one
    int idx = _profileBox->findText(QString::fromStdString(preset));
    if (idx == -1) {
        // We didn't find the preset, so the user probably specified a path in the
        // configuration file that doesn't match any value in the list
        _profileBox->addItem(QString::fromStdString("--- Configuration File ---"));
        model = qobject_cast<const QStandardItemModel*>(_profileBox->model());
        model->item(_profileBox->count() - 1)->setEnabled(false);

        _profileBox->addItem(
            QString::fromStdString(preset),
            QString::fromStdString(preset)
        );
        idx = _profileBox->count() - 1;
    }

    _profileBox->setCurrentIndex(idx);
}

// Returns 'true' if the file was a configuration file, 'false' otherwise
bool handleConfigurationFile(QComboBox& box, const std::filesystem::directory_entry& p) {
    const bool isJson = p.path().extension() == ".json";
    if (!isJson) {
        return false;
    }
    box.addItem(QString::fromStdString(p.path().filename().string()));

    // Add tooltip
    if (isJson) {
        std::string tooltipDescription;
        try {
            const sgct::config::Meta meta = sgct::readMeta(p.path());
            tooltipDescription = meta.description;
        }
        catch (const sgct::Error&) {
            tooltipDescription = "(no description available)";
        }
        if (!tooltipDescription.empty()) {
            const QString toolTip = QString::fromStdString(
                std::format("<p>{}</p>", tooltipDescription)
            );
            box.setItemData(box.count() - 1, toolTip, Qt::ToolTipRole);
        }
    }

    return true;
}

void LauncherWindow::populateWindowConfigsList(const std::string& preset) {
    namespace fs = std::filesystem;

    // Disconnect the signal for new window config selection during population process
    disconnect(
        _windowConfigBox,
        QOverload<int>::of(&QComboBox::currentIndexChanged),
        nullptr,
        nullptr
    );
    _windowConfigBox->clear();

    _userConfigCount = 0;
    _userConfigStartingIdx = 0;
    _preDefinedConfigStartingIdx = 0;
    _windowConfigBox->addItem(QString::fromStdString("--- User Configurations ---"));
    const QStandardItemModel* model =
        qobject_cast<const QStandardItemModel*>(_windowConfigBox->model());

    model->item(_userConfigCount)->setEnabled(false);
    _userConfigCount++;
    _userConfigStartingIdx++;
    _preDefinedConfigStartingIdx++;

    // Sort files
    std::vector<fs::directory_entry> files;
    for (const fs::directory_entry& p : fs::directory_iterator(_userConfigPath)) {
        files.push_back(p);
    }
    std::sort(files.begin(), files.end());

    // Add all the files with the .json extension to the dropdown
    for (const fs::directory_entry& p : files) {
        const bool isConfigFile = handleConfigurationFile(*_windowConfigBox, p);
        if (isConfigFile) {
            _userConfigCount++;
            _userConfigStartingIdx++;
            _preDefinedConfigStartingIdx++;
        }
    }
    _windowConfigBox->addItem(QString::fromStdString("--- OpenSpace Configurations ---"));
    model = qobject_cast<const QStandardItemModel*>(_windowConfigBox->model());
    model->item(_userConfigCount)->setEnabled(false);
    _preDefinedConfigStartingIdx++;

    if (std::filesystem::exists(_configPath)) {
        // Sort files
        files.clear();
        for (const fs::directory_entry& p : fs::directory_iterator(_configPath)) {
            files.push_back(p);
        }
        std::sort(files.begin(), files.end());
        // Add all the files with the .json extension to the dropdown
        for (const fs::directory_entry& p : files) {
            handleConfigurationFile(*_windowConfigBox, p);
        }
    }
    else {
        LINFOC(
            "LauncherWindow",
            std::format("Could not find config folder '{}'", _configPath)
        );
    }

    // Always add the .cfg SGCT default as first item
    _windowConfigBox->insertItem(
        _windowConfigBoxIndexSgctCfgDefault,
        QString::fromStdString(_sgctConfigName)
    );
    const QString defaultTip =
        "<p>The basic default configuration specified in the .cfg file</p>";
    _windowConfigBox->setItemData(
        _windowConfigBoxIndexSgctCfgDefault,
        defaultTip,
        Qt::ToolTipRole
    );
    // Try to find the requested configuration file and set it as the current one. As we
    // have support for function-generated configuration files that will not be in the
    // list we need to add a preset that doesn't exist a file for
    const int idx = _windowConfigBox->findText(QString::fromStdString(preset));
    if (idx != -1) {
        _windowConfigBox->setCurrentIndex(idx);
    }
    else {
        // Add the requested preset at the top
        _windowConfigBox->insertItem(
            _windowConfigBoxIndexSgctCfgDefault + 1,
            QString::fromStdString(preset)
        );
        // Increment the user config count because there is an additional option added
        // before the user config options
        _userConfigCount++;
        _userConfigStartingIdx++;
        _preDefinedConfigStartingIdx++;
        _windowConfigBox->setCurrentIndex(_windowConfigBoxIndexSgctCfgDefault + 1);
    }
    connect(
        _windowConfigBox,
        QOverload<int>::of(&QComboBox::currentIndexChanged),
        this,
        &LauncherWindow::onNewWindowConfigSelection
    );
    // Call combobox selected callback to refresh the file status of the current selection
    onNewWindowConfigSelection(_windowConfigBox->currentIndex());
}

void LauncherWindow::onNewWindowConfigSelection(int newIndex) {
    const std::filesystem::path pathSelected = absPath(selectedWindowConfig());
    if (newIndex == _windowConfigBoxIndexSgctCfgDefault) {
        _editWindowButton->setEnabled(false);
        _editWindowButton->setToolTip(
            "Cannot edit the 'Default' configuration since it is not a file"
        );
    }
    else if (newIndex >= _preDefinedConfigStartingIdx) {
        _editWindowButton->setEnabled(false);
        _editWindowButton->setToolTip(
            QString::fromStdString(std::format(
                "Cannot edit '{}'\nsince it is one of the configuration "
                "files provided in the OpenSpace installation", pathSelected
            ))
        );
    }
    else {
        try {
            sgct::config::GeneratorVersion previewGenVersion =
                sgct::readConfigGenerator(pathSelected);
            if (!versionCheck(previewGenVersion)) {
                _editWindowButton->setEnabled(false);
                _editWindowButton->setToolTip(QString::fromStdString(std::format(
                    "This file does not meet the minimum required version of {}.",
                    versionMin.versionString()
                )));
                return;
            } 
        }
        catch (const std::runtime_error&) {
            // Ignore an exception here because clicking the edit button will
            // bring up an explanatory error message
        }
        _editWindowButton->setEnabled(true);
        _editWindowButton->setToolTip("");
    }
}

void LauncherWindow::openProfileEditor(const std::string& profile, bool isUserProfile) {
    std::optional<Profile> p;
    std::filesystem::path savePath = isUserProfile ? _userProfilePath : _profilePath;
    if (profile.empty()) {
        // If the requested profile is the empty string, then we want to create a new one
        p = Profile();
    }
    else {
        // Otherwise, we want to load that profile
        std::string fullProfilePath = std::format("{}{}.profile", savePath, profile);
        p = loadProfileFromFile(this, std::move(fullProfilePath));
        if (!p.has_value()) {
            return;
        }
    }

    ProfileEdit editor = ProfileEdit(
        *p,
        profile,
        _assetPath,
        _userAssetPath,
        _profilePath,
        savePath,
        this
    );
    editor.exec();
    if (editor.wasSaved()) {
        if (editor.specifiedFilename() != profile) {
            savePath = _userProfilePath;
        }
        const std::string path = std::format(
            "{}{}.profile", savePath, editor.specifiedFilename()
        );
        saveProfile(this, path, *p);
        populateProfilesList(editor.specifiedFilename());
    }
    else {
        const std::string current = _profileBox->currentText().toStdString();
        populateProfilesList(current);
    }
}

void LauncherWindow::editRefusalDialog(const std::string& title, const std::string& msg,
                                       const std::string& detailedText)
{
    QMessageBox msgBox(this);
    msgBox.setText(QString::fromStdString(msg));
    msgBox.setWindowTitle(QString::fromStdString(title));
    msgBox.setDetailedText(QString::fromStdString(detailedText));
    msgBox.setIcon(QMessageBox::Warning);
    msgBox.exec();
}

void LauncherWindow::openWindowEditor(const std::string& winCfg, bool isUserWinCfg) {
    using namespace sgct;

    std::filesystem::path saveWindowPath = isUserWinCfg ? _userConfigPath : _configPath;
    int ret = QDialog::DialogCode::Rejected;
    config::Cluster preview;
    if (winCfg.empty()) {
        SgctEdit editor(this, _userConfigPath);
        ret = editor.exec();
        if (ret == QDialog::DialogCode::Accepted) {
            handleReturnFromWindowEditor(
                editor.cluster(),
                editor.saveFilename(),
                saveWindowPath
            );
        }
    }
    else {
        try {
            config::GeneratorVersion previewGenVersion = readConfigGenerator(winCfg);
            loadFileAndSchemaThenValidate(
                winCfg,
                _configPath / "schema/sgct.schema.json",
                "This configuration file is unable to generate a proper display"
            );
            loadFileAndSchemaThenValidate(
                winCfg,
                _configPath / "schema/sgcteditor.schema.json",
                "This configuration file is valid for generating a display, but "
                "its format does not match the window editor requirements and "
                "cannot be opened in the editor"
            );
            if (versionCheck(previewGenVersion)) {
                try {
                    preview = readConfig(
                        winCfg,
                        "This configuration file is unable to generate a proper display "
                        "due to a problem detected in the readConfig function"
                    );
                }
                catch (const std::runtime_error& e) {
                    //Re-throw an SGCT error exception with the runtime exception message
                    throw std::runtime_error(std::format(
                        "Importing of this configuration file failed because of a "
                        "problem detected in the readConfig function:\n\n{}", e.what()
                    ));
                }
                SgctEdit editor = SgctEdit(
                    preview,
                    winCfg,
                    saveWindowPath,
                    this
                );
                ret = editor.exec();
                if (ret == QDialog::DialogCode::Accepted) {
                    handleReturnFromWindowEditor(
                        editor.cluster(),
                        editor.saveFilename(),
                        saveWindowPath
                    );
                }
            }
            else {
                editRefusalDialog(
                    "File Format Version Error",
                    std::format(
                        "File '{}' does not meet the minimum required version of {}",
                        winCfg, versionMin.versionString()
                    ),
                    ""
                );
            }
        }
        catch (const std::runtime_error& e) {
            editRefusalDialog(
                "Format Validation Error",
                std::format("Parsing error found in file '{}'", winCfg),
                e.what()
            );
        }
    }
}

void LauncherWindow::handleReturnFromWindowEditor(const sgct::config::Cluster& cluster,
                                                  std::filesystem::path savePath,
                                           const std::filesystem::path& saveWindowCfgPath)
{
    savePath.replace_extension(".json");
    saveWindowConfig(this, savePath, cluster);
    // Truncate path to convert this back to path relative to _userConfigPath
    const std::filesystem::path p = std::filesystem::proximate(
        savePath,
        saveWindowCfgPath
    );
    populateWindowConfigsList(p.string());
}

bool LauncherWindow::wasLaunchSelected() const {
    return _shouldLaunch;
}

std::string LauncherWindow::selectedProfile() const {
    // The user data stores the full path to the profile
    return _profileBox->currentData().toString().toStdString();
}

std::string LauncherWindow::selectedWindowConfig() const {
    const int idx = _windowConfigBox->currentIndex();
    if (idx == 0) {
        return _sgctConfigName;
    }
    else if (idx > _userConfigCount) {
        return "${CONFIG}/" + _windowConfigBox->currentText().toStdString();
    }
    else {
        return "${USER_CONFIG}/" + _windowConfigBox->currentText().toStdString();
    }
}

bool LauncherWindow::isUserConfigSelected() const {
    const int selectedIndex = _windowConfigBox->currentIndex();
    return (selectedIndex <= _userConfigCount);
}
