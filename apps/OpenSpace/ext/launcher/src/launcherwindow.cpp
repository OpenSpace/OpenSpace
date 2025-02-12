/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
#include "backgroundimage.h"
#include "settingsdialog.h"
#include "splitcombobox.h"
#include <openspace/openspace.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <sgct/config.h>
#include <QFile>
#include <QKeyEvent>
#include <QLabel>
#include <QMessageBox>
#include <QPushButton>
#include <QStandardItemModel>
#include <filesystem>
#include <fstream>
#include <iostream>

#ifdef WIN32
#include <Windows.h>
#endif // WIN32

using namespace openspace;

namespace {
    constexpr std::string_view _loggerCat = "LauncherWindow";

    constexpr int ScreenWidth = 480;
    constexpr int ScreenHeight = 640;

    constexpr int LeftRuler = 40;
    constexpr int TopRuler = 80;
    constexpr int ItemWidth = 260;
    constexpr int ItemHeight = ItemWidth / 4;
    constexpr int SmallItemWidth = 100;
    constexpr int SmallItemHeight = SmallItemWidth / 4;

    constexpr int SettingsIconSize = 35;

    int WindowConfigBoxIndexSgctCfgDefault = 0;

    namespace geometry {
        constexpr QRect BackgroundImage(0, 0, ScreenWidth, ScreenHeight);
        constexpr QRect LogoImage(LeftRuler, TopRuler, ItemWidth, ItemHeight);
        constexpr QRect ChooseLabel(LeftRuler + 10, TopRuler + 80, 151, 24);
        constexpr QRect ProfileBox(LeftRuler, TopRuler + 110, ItemWidth, ItemHeight);
        constexpr QRect NewProfileButton(
            LeftRuler + 160, TopRuler + 180, SmallItemWidth, SmallItemHeight
        );
        constexpr QRect EditProfileButton(
            LeftRuler, TopRuler + 180, SmallItemWidth, SmallItemHeight
        );
        constexpr QRect OptionsLabel(LeftRuler + 10, TopRuler + 230, 151, 24);
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


    void editRefusalDialog(QWidget* parent, const std::string& title,
                           const std::string& msg, const std::string& detailedText = "")
    {
        QMessageBox msgBox(parent);
        msgBox.setText(QString::fromStdString(msg));
        msgBox.setWindowTitle(QString::fromStdString(title));
        msgBox.setDetailedText(QString::fromStdString(detailedText));
        msgBox.setIcon(QMessageBox::Warning);
        msgBox.exec();
    }

    bool versionCheck(const sgct::config::GeneratorVersion& v) {
        return
            v.versionCheck(VersionMin) ||
            v == VersionLegacy18 ||
            v == VersionLegacy19;
    }
} // namespace

LauncherWindow::LauncherWindow(bool profileEnabled, const Configuration& globalConfig,
                               bool sgctConfigEnabled, std::string sgctConfigName)
    // The extra `/ ""` at the end here is necessary as we assume the paths to have a
    // terminating `/` character at the end
    : _assetPath(absPath(globalConfig.pathTokens.at("ASSETS")) / "")
    , _userAssetPath(absPath(globalConfig.pathTokens.at("USER_ASSETS")) / "")
    , _configPath(absPath(globalConfig.pathTokens.at("CONFIG")) / "")
    , _userConfigPath(absPath(globalConfig.pathTokens.at("USER_CONFIG")) / "")
    , _profilePath(absPath(globalConfig.pathTokens.at("PROFILES")) / "")
    , _userProfilePath(absPath(globalConfig.pathTokens.at("USER_PROFILES")) / "")
{
    Q_INIT_RESOURCE(resources);

    qInstallMessageHandler(
        [](QtMsgType type, const QMessageLogContext&, const QString& msg) {
            if (type == QtCriticalMsg || type == QtFatalMsg || type == QtCriticalMsg) {
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

    QWidget* centralWidget = new QWidget;

    {
        // Yes, this looks weird, but we just create the image here and pass the central
        // widget as the parent. This will assign the BackgroundImage as a child of the
        // central widget, thus making it appear and also be destroyed at the end
        new BackgroundImage(
            geometry::BackgroundImage,
            absPath(globalConfig.pathTokens.at("SYNC") + "/http/launcher_images"),
            centralWidget
        );
    }
    
    {
        QLabel* logoImage = new QLabel(centralWidget);
        logoImage->setObjectName("clear");
        logoImage->setGeometry(geometry::LogoImage);
        logoImage->setPixmap(QPixmap(":/images/openspace-horiz-logo-small.png"));
    }

    //
    // Profile chooser
    //

    {
        QLabel* labelChoose = new QLabel("Choose Profile", centralWidget);
        labelChoose->setGeometry(geometry::ChooseLabel);
        labelChoose->setObjectName("label_choose");
    }

    _editProfileButton = new QPushButton("Edit", centralWidget);
    _editProfileButton->setObjectName("small");
    _editProfileButton->setGeometry(geometry::EditProfileButton);
    _editProfileButton->setCursor(Qt::PointingHandCursor);
    _editProfileButton->setAutoDefault(true);
    _editProfileButton->setAccessibleName("Edit profile");
    connect(
        _editProfileButton, &QPushButton::released,
        this, &LauncherWindow::editProfile
    );

    {
        QPushButton* newProfileButton = new QPushButton("New", centralWidget);
        newProfileButton->setObjectName("small");
        newProfileButton->setGeometry(geometry::NewProfileButton);
        newProfileButton->setCursor(Qt::PointingHandCursor);
        newProfileButton->setAutoDefault(true);
        newProfileButton->setAccessibleName("New profile");
        connect(
            newProfileButton, &QPushButton::released,
            this, &LauncherWindow::newProfile
        );
    }

    // Creating the profile box _after_ the Edit and New buttons as the comboboxes
    // `selectionChanged` signal will trigger that will try to make changes to the edit
    // button
    _profileBox = new SplitComboBox(
        centralWidget,
        _userProfilePath,
        "--- User Profiles ---",
        _profilePath,
        "--- OpenSpace Profiles ---",
        "",
        [](const std::filesystem::path& p) { return p.extension() == ".profile"; },
        [](const std::filesystem::path& p) {
            try {
                Profile profile(p);
                if (profile.meta.has_value() && profile.meta->description.has_value()) {
                    return *profile.meta->description;
                }
            }
            catch (...) {}
            return std::string();
        }
    );
    _profileBox->setObjectName("config");
    _profileBox->setGeometry(geometry::ProfileBox);
    _profileBox->setAccessibleName("Choose profile");
    _profileBox->populateList(globalConfig.profile);
    _profileBox->setEnabled(profileEnabled);
    connect(
        _profileBox, &SplitComboBox::selectionChanged,
        this, &LauncherWindow::selectedProfile
    );

    {
        // Set up the default value for the edit button
        std::string selection = std::get<1>(_profileBox->currentSelection());
        _editProfileButton->setEnabled(std::filesystem::exists(selection));
    }



    //
    // Window configuration chooser
    //
    {
        QLabel* optionsLabel = new QLabel("Window Options", centralWidget);
        optionsLabel->setGeometry(geometry::OptionsLabel);
        optionsLabel->setObjectName("label_options");
    }

    _editWindowButton = new QPushButton("Edit", centralWidget);
    _editWindowButton->setVisible(true);
    _editWindowButton->setObjectName("small");
    _editWindowButton->setGeometry(geometry::EditWindowButton);
    _editWindowButton->setCursor(Qt::PointingHandCursor);
    _editWindowButton->setAutoDefault(true);
    _editWindowButton->setAccessibleName("Edit window configuration");
    connect(
        _editWindowButton, &QPushButton::released,
        this, &LauncherWindow::editConfiguration
    );

    {
        QPushButton* newWindowButton = new QPushButton("New", centralWidget);
        newWindowButton->setObjectName("small");
        newWindowButton->setGeometry(geometry::NewWindowButton);
        newWindowButton->setCursor(Qt::PointingHandCursor);
        newWindowButton->setAutoDefault(true);
        newWindowButton->setAccessibleName("New window configuration");
        connect(
            newWindowButton, &QPushButton::released,
            this, &LauncherWindow::newConfiguration
        );
    }

    _windowConfigBox = new SplitComboBox(
        centralWidget,
        _userConfigPath,
        "--- User Configurations ---",
        _configPath,
        "--- OpenSpace Configuration ---",
        sgctConfigName,
        [](const std::filesystem::path& p) {
            // @TODO (abock, 2025-02-10) Remove once the schema is baked into the code
            return p.extension() == ".json" && p.filename() != "sgct.schema.json";
        },
        [](const std::filesystem::path& p) {
            try {
                sgct::config::Cluster cluster = sgct::readConfig(p);
                if (cluster.meta && cluster.meta->description.has_value()) {
                    return *cluster.meta->description;
                }
            }
            catch (...) {}
            return std::string();
        }

    );
    _windowConfigBox->setObjectName("config");
    _windowConfigBox->setGeometry(geometry::WindowConfigBox);
    _windowConfigBox->setAccessibleName("Select window configuration");
    _windowConfigBox->setEnabled(sgctConfigEnabled);
    _windowConfigBox->populateList(sgctConfigName);
    // Trigger currentIndexChanged so the preview file read is performed
    _windowConfigBox->currentIndexChanged(_windowConfigBox->currentIndex());
    connect(
        _windowConfigBox, &SplitComboBox::selectionChanged,
        this, &LauncherWindow::selectConfiguration
    );

    {
        // Set up the default value for the edit button
        std::string selection = std::get<1>(_windowConfigBox->currentSelection());
        _editWindowButton->setEnabled(std::filesystem::exists(selection));
    }


    //
    // Start button
    //
    _startButton = new QPushButton("START", centralWidget);
    connect(
        _startButton, &QPushButton::released,
        this, &LauncherWindow::start
    );
    _startButton->setObjectName("start");
    _startButton->setGeometry(geometry::StartButton);
    _startButton->setCursor(Qt::PointingHandCursor);
    _startButton->setAutoDefault(true);
    _startButton->setAccessibleName("Start OpenSpace");
    _startButton->setFocus(Qt::OtherFocusReason);


    //
    // Version information
    //
    {
        QLabel* versionLabel = new QLabel(centralWidget);
        versionLabel->setVisible(true);
        versionLabel->setText(
            QString::fromStdString(std::string(OPENSPACE_VERSION_STRING_FULL))
        );
        versionLabel->setObjectName("version-info");
        versionLabel->setGeometry(geometry::VersionString);
    }


    //
    // Settings button
    //
    {
        QPushButton* settingsButton = new QPushButton(centralWidget);
        settingsButton->setObjectName("settings");
        settingsButton->setGeometry(geometry::SettingsButton);
        settingsButton->setIconSize(QSize(SettingsIconSize, SettingsIconSize));
        settingsButton->setAutoDefault(true);
        settingsButton->setAccessibleName("Settings");
        connect(
            settingsButton, &QPushButton::released,
            this, &LauncherWindow::openSettings
        );
    }

    setCentralWidget(centralWidget);
}

void LauncherWindow::editProfile() {
    auto [selection, path] = _profileBox->currentSelection();
    ghoul_assert(std::filesystem::exists(path), "Path not found");
    const bool isUserProfile = path.starts_with(_userProfilePath.string());
    ghoul_assert(
        isUserProfile || path.starts_with(_profilePath.string()),
        "Misshapen profile path. Must be in profile or user/profile folder"
    );
    openProfileEditor(selection, isUserProfile);
}

void LauncherWindow::newProfile() {
    openProfileEditor("", true);
}

void LauncherWindow::selectProfile(std::optional<std::string> selection) {
    ghoul_assert(selection.has_value(), "No special item in the profiles");
    if (selection.has_value()) {
        // Having the `if` statement here to satisfy the MSVC code analysis
        _editProfileButton->setEnabled(std::filesystem::exists(*selection));
    }
}

void LauncherWindow::selectConfiguration(std::optional<std::string> selection) {
    if (!selection.has_value()) {
        // The first entry is the value read from the openspace.cfg file
        _editWindowButton->setEnabled(false);
        _editWindowButton->setToolTip(
            "Cannot edit the default configuration since it is not a file"
        );
    }
    else if (selection->starts_with(_configPath.string())) {
        // If the configuration is a default configuration, we don't allow editing
        _editWindowButton->setEnabled(false);
        _editWindowButton->setToolTip(
            "Cannot edit since the selected configuration is one of the files "
            "provided by OpenSpace"
        );
    }
    else {
        try {
            sgct::config::Cluster c = sgct::readConfig(*selection);
            if (!c.generator || !versionCheck(*c.generator)) {
                _editWindowButton->setEnabled(false);
                _editWindowButton->setToolTip(QString::fromStdString(std::format(
                    "This file does not meet the minimum required version of {}.",
                    VersionMin.versionString()
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

void LauncherWindow::editConfiguration() {
    const std::string path = selectedWindowConfig();
    ghoul_assert(!path.empty(), "There must be a configuration to edit");
    ghoul_assert(std::filesystem::is_regular_file(path), "Path not found");
    ghoul_assert(path.starts_with(_userConfigPath.string()), "No user config");

    int ret = QDialog::DialogCode::Rejected;
    sgct::config::Cluster cluster;
    try {
        cluster = sgct::readConfig(path);
    }
    catch (const std::runtime_error& e) {
        // Re-throw an SGCT error exception with the runtime exception message
        throw std::runtime_error(std::format(
            "Importing of this configuration file failed because of a "
            "problem detected in the readConfig function:\n\n{}", e.what()
        ));
    }

    assert(cluster.generator);
    std::string err = sgct::validateConfigAgainstSchema(
        path,
        _configPath / "schema/sgct.schema.json"
    );
    if (!err.empty()) {
        editRefusalDialog(
            this,
            "Format Validation Error",
            std::format("Parsing error found in file '{}'", path),
            std::format(
                "{}\n\nThis configuration file is unable to generate a proper "
                "display",
                err
            )
        );
        return;
    }

    if (!versionCheck(*cluster.generator)) {
        editRefusalDialog(
            this,
            "File Format Version Error",
            std::format(
                "File '{}' does not meet the minimum required version of {}",
                path, VersionMin.versionString()
            )
        );
        return;
    }

    SgctEdit editor = SgctEdit(cluster, path, _userConfigPath, this);
    ret = editor.exec();
    if (ret == QDialog::DialogCode::Accepted) {
        handleReturnFromWindowEditor(editor.cluster(), editor.saveFilename());
    }
}

void LauncherWindow::newConfiguration() {
    SgctEdit editor = SgctEdit(sgct::config::Cluster(), "", _userConfigPath, this);
    int ret = editor.exec();
    if (ret == QDialog::DialogCode::Accepted) {
        handleReturnFromWindowEditor(editor.cluster(), editor.saveFilename());
    }
}

void LauncherWindow::start() {
    std::string selection = std::get<0>(_profileBox->currentSelection());
    if (selection.empty()) {
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

void LauncherWindow::openSettings() {
    Settings settings = loadSettings();
    SettingsDialog dialog = SettingsDialog(std::move(settings), this);
    connect(
        &dialog,
        &SettingsDialog::saveSettings,
        [this](Settings s) {
            saveSettings(s, findSettings());

            if (s.profile.has_value()) {
                _profileBox->populateList(*s.profile);
            }

            if (s.configuration.has_value()) {
                _windowConfigBox->populateList(*s.configuration);
            }
        }
    );

    dialog.exec();
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
        // Verify that the file actually exists
        if (!std::filesystem::exists(fullProfilePath)) {
            QMessageBox::critical(
                this,
                "Exception",
                QString::fromStdString(std::format(
                    "Could not open profile file '{}'", fullProfilePath
                ))
            );
            return;
        }

        try {
            p = Profile(fullProfilePath);
        }
        catch (const Profile::ParsingError& e) {
            QMessageBox::critical(
                this,
                "Exception",
                QString::fromStdString(std::format(
                    "ParsingError exception in '{}': {}, {}",
                    fullProfilePath, e.component, e.message
                ))
            );
            return;
        }
        catch (const ghoul::RuntimeError& e) {
            QMessageBox::critical(
                this,
                "Exception",
                QString::fromStdString(std::format(
                    "RuntimeError exception in '{}', component {}: {}",
                    fullProfilePath, e.component, e.message
                ))
            );
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

    // Check whether there are unsaved changes from the profile editor
    connect(
        &editor,
        &ProfileEdit::raiseExitWindow,
        [this, &editor, &savePath, &p, &profile]() {
            const std::string origPath = std::format("{}{}.profile", savePath, profile);
            // If this is a new profile we want to prompt the user
            if (!std::filesystem::exists(origPath)) {
                editor.promptUserOfUnsavedChanges();
                return;
            }

            // Check if the profile is the same as current existing file
            if (*p != Profile(origPath)) {
                editor.promptUserOfUnsavedChanges();
            }
            else {
                editor.closeWithoutSaving();
            }
        }
    );

    editor.exec();
    if (editor.wasSaved()) {
        if (editor.specifiedFilename() != profile) {
            savePath = _userProfilePath;
        }
        const std::string path = std::format(
            "{}{}.profile", savePath, editor.specifiedFilename()
        );

        // The user might specify subdirectories in the name which we want to create
        std::filesystem::create_directories(std::filesystem::path(path).parent_path());

        try {
            std::ofstream outFile;
            outFile.exceptions(std::ofstream::badbit | std::ofstream::failbit);
            outFile.open(path, std::ofstream::out);
            outFile << p->serialize();
        }
        catch (const std::ofstream::failure& e) {
#ifdef WIN32
            if (std::filesystem::exists(path)) {
                // Check if the file is hidden, since that causes ofstream to fail
                DWORD res = GetFileAttributesA(path.c_str());
                if (res & FILE_ATTRIBUTE_HIDDEN) {
                    QMessageBox::critical(
                        this,
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
                this,
                "Exception",
                QString::fromStdString(std::format(
                    "Error writing data to file '{}': {}", path, e.what()
                ))
            );
        }

        _profileBox->populateList(editor.specifiedFilename());
    }
}

void LauncherWindow::handleReturnFromWindowEditor(const sgct::config::Cluster& cluster,
                                                  std::filesystem::path savePath)
{
    std::ofstream outFile;
    outFile.open(savePath, std::ofstream::out);

    if (outFile.good()) {
        sgct::config::GeneratorVersion genEntry = VersionMin;
        outFile << sgct::serializeConfig(cluster, genEntry);
    }
    else {
        QMessageBox::critical(
            this,
            "Exception",
            QString::fromStdString(std::format(
                "Error writing data to file '{}'", savePath
            ))
        );
    }

    // Truncate path to convert this back to path relative to _userConfigPath
    std::filesystem::path p = std::filesystem::proximate(savePath, _userConfigPath);

    // Remove the file extension as the drop down menu only displays the raw names
    p.replace_extension();
    _windowConfigBox->populateList(p.string());
}

bool LauncherWindow::hasValidSelection() const {
    std::string profilePath = std::get<1>(_profileBox->currentSelection());
    std::string configPath = std::get<1>(_windowConfigBox->currentSelection());

    return true;
}

bool LauncherWindow::wasLaunchSelected() const {
    return _shouldLaunch;
}

std::string LauncherWindow::selectedProfile() const {
    // The user data stores the full path to the profile
    return std::get<1>(_profileBox->currentSelection());
}

std::string LauncherWindow::selectedWindowConfig() const {
    return std::get<1>(_windowConfigBox->currentSelection());
}

void LauncherWindow::keyPressEvent(QKeyEvent* evt) {
    if (evt->key() == Qt::Key_Escape) {
        _shouldLaunch = false;
        close();
        return;
    }
    QMainWindow::keyPressEvent(evt);
}
