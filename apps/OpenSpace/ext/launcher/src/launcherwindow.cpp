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
#include <QComboBox>
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

    std::optional<Profile> loadProfile(QWidget* parent, std::filesystem::path filename) {
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

    // Returns 'true' if the file was a configuration file, 'false' otherwise
    bool handleConfigurationFile(QComboBox& box, const std::filesystem::path& p,
                                 const std::filesystem::path& rel)
    {
        std::filesystem::path relPath = std::filesystem::relative(p, rel);
        relPath.replace_extension();

        box.addItem(
            QString::fromStdString(relPath.string()),
            QString::fromStdString(p.string())
        );

        // Add tooltip
        std::string tooltipDescription = "(no description available)";
        try {
            sgct::config::Cluster cluster = sgct::readConfig(p);
            if (cluster.meta && cluster.meta->description.has_value()) {
                tooltipDescription = *cluster.meta->description;
            }
        }
        catch (const sgct::Error&) {}
        const QString toolTip = QString::fromStdString(
            std::format("<p>{}</p>", tooltipDescription)
        );
        box.setItemData(box.count() - 1, toolTip, Qt::ToolTipRole);

        return true;
    }

    bool versionCheck(const sgct::config::GeneratorVersion& v) {
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
    // The extra `/ ""` at the end here is necessary as we assume the paths to have a
    // terminating `/` character at the end
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

    QWidget* central = createCentralWidget(
        absPath(globalConfig.pathTokens.at("SYNC") + "/http/launcher_images")
    );
    setCentralWidget(central);
    QPushButton* startButton = centralWidget()->findChild<QPushButton*>("start");
    if (startButton) {
        startButton->setFocus(Qt::OtherFocusReason);
    }

    _profileBox->populateList(globalConfig.profile);
    _profileBox->setEnabled(profileEnabled);

    _windowConfigBox->setEnabled(sgctConfigEnabled);
    _windowConfigBox->populateList(_sgctConfigName);
    // Trigger currentIndexChanged so the preview file read is performed
    _windowConfigBox->currentIndexChanged(_windowConfigBox->currentIndex());
}

QWidget* LauncherWindow::createCentralWidget(std::filesystem::path syncFolder) {
    QWidget* centralWidget = new QWidget;

    new BackgroundImage(geometry::BackgroundImage, syncFolder, centralWidget);

    QLabel* logoImage = new QLabel(centralWidget);
    logoImage->setObjectName("clear");
    logoImage->setGeometry(geometry::LogoImage);
    logoImage->setPixmap(QPixmap(":/images/openspace-horiz-logo-small.png"));

    QLabel* labelChoose = new QLabel("Choose Profile", centralWidget);
    labelChoose->setGeometry(geometry::ChooseLabel);
    labelChoose->setObjectName("label_choose");

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

    connect(
        _profileBox,
        &SplitComboBox::selectionChanged,
        [this](std::optional<std::string> selection) {
            ghoul_assert(selection.has_value(), "No special item in the profiles");
            _editProfileButton->setEnabled(std::filesystem::exists(*selection));
        }
    );

    _editProfileButton = new QPushButton("Edit", centralWidget);
    connect(
        _editProfileButton,
        &QPushButton::released,
        [this]() {
            auto [selection, path] = _profileBox->currentSelection();
            ghoul_assert(std::filesystem::exists(path), "Path not found");
            const bool isUserProfile = path.starts_with(_userProfilePath.string());
            ghoul_assert(
                isUserProfile || path.starts_with(_profilePath.string()),
                "Misshapen profile path. Must be in profile or user/profile folder"
            );
            openProfileEditor(selection, isUserProfile);
        }
    );
    _editProfileButton->setObjectName("small");
    _editProfileButton->setGeometry(geometry::EditProfileButton);
    _editProfileButton->setCursor(Qt::PointingHandCursor);
    _editProfileButton->setAutoDefault(true);
    _editProfileButton->setAccessibleName("Edit profile");

    QPushButton* newProfileButton = new QPushButton("New", centralWidget);
    connect(
        newProfileButton,
        &QPushButton::released,
        [this]() { openProfileEditor("", true); }
    );
    newProfileButton->setObjectName("small");
    newProfileButton->setGeometry(geometry::NewProfileButton);
    newProfileButton->setCursor(Qt::PointingHandCursor);
    newProfileButton->setAutoDefault(true);
    newProfileButton->setAccessibleName("New profile");

    QLabel* optionsLabel = new QLabel("Window Options", centralWidget);
    optionsLabel->setGeometry(geometry::OptionsLabel);
    optionsLabel->setObjectName("label_options");

    _windowConfigBox = new SplitComboBox(
        centralWidget,
        _userConfigPath,
        "--- User Configurations ---",
        _configPath,
        "--- OpenSpace Configuration ---",
        _sgctConfigName,
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

    _editWindowButton = new QPushButton("Edit", centralWidget);
    connect(
        _editWindowButton,
        &QPushButton::released,
        [this]() {
            const std::string path = selectedWindowConfig();
            ghoul_assert(std::filesystem::is_regular_file(path), "Path not found");
            const bool isUserConfig = path.starts_with(_userConfigPath.string());
            ghoul_assert(
                isUserConfig || path.starts_with(_configPath.string()),
                "Misshapen config path. Must be in config or user/config folder"
            );
            openWindowEditor(path, isUserConfig);
        }
    );
    _editWindowButton->setVisible(true);
    _editWindowButton->setObjectName("small");
    _editWindowButton->setGeometry(geometry::EditWindowButton);
    _editWindowButton->setCursor(Qt::PointingHandCursor);
    _editWindowButton->setAutoDefault(true);
    _editWindowButton->setAccessibleName("Edit window configuration");

    QPushButton* newWindowButton = new QPushButton("New", centralWidget);
    connect(
        newWindowButton,
        &QPushButton::released,
        [this]() { openWindowEditor("", true); }
    );
    newWindowButton->setObjectName("small");
    newWindowButton->setGeometry(geometry::NewWindowButton);
    newWindowButton->setCursor(Qt::PointingHandCursor);
    newWindowButton->setAutoDefault(true);
    newWindowButton->setAccessibleName("New window configuration");

    QPushButton* startButton = new QPushButton("START", centralWidget);
    connect(
        startButton,
        &QPushButton::released,
        [this]() {
            auto [selection, path] = _profileBox->currentSelection();
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
    );
    startButton->setObjectName("start");
    startButton->setGeometry(geometry::StartButton);
    startButton->setCursor(Qt::PointingHandCursor);
    startButton->setAutoDefault(true);
    startButton->setAccessibleName("Start OpenSpace");

    QLabel* versionLabel = new QLabel(centralWidget);
    versionLabel->setVisible(true);
    versionLabel->setText(
        QString::fromStdString(std::string(openspace::OPENSPACE_VERSION_STRING_FULL))
    );
    versionLabel->setObjectName("version-info");
    versionLabel->setGeometry(geometry::VersionString);

    QPushButton* settingsButton = new QPushButton(centralWidget);
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
                        _profileBox->populateList(*s.profile);
                    }

                    if (s.configuration.has_value()) {
                        _windowConfigBox->populateList(*s.configuration);
                    }
                }
            );

            dialog.exec();
        }
    );
    settingsButton->setObjectName("settings");
    settingsButton->setGeometry(geometry::SettingsButton);
    settingsButton->setIconSize(QSize(SettingsIconSize, SettingsIconSize));
    settingsButton->setAutoDefault(true);
    settingsButton->setAccessibleName("Settings");

    return centralWidget;
}

void LauncherWindow::onNewWindowConfigSelection(int newIndex) {
    const std::string pathSelected = selectedWindowConfig();
    if (newIndex == WindowConfigBoxIndexSgctCfgDefault) {
        // The first entry is the value read from the openspace.cfg file
        _editWindowButton->setEnabled(false);
        _editWindowButton->setToolTip(
            "Cannot edit the 'Default' configuration since it is not a file"
        );
    }
    else if (pathSelected.starts_with(_configPath.string())) {
        // If the configuration is a default configuration, we don't allow editing
        _editWindowButton->setEnabled(false);
        _editWindowButton->setToolTip(
            "Cannot edit since the selected configuration is one of the files provided "
            "by OpenSpace"
        );
    }
    else {
        try {
            sgct::config::Cluster c = sgct::readConfig(pathSelected);
            if (!c.generator || !versionCheck(*c.generator)) {
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
        p = loadProfile(this, std::move(fullProfilePath));
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
    else {
        auto [selection, path] = _profileBox->currentSelection();
        _profileBox->populateList(selection);
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
    std::filesystem::path saveWindowPath = isUserWinCfg ? _userConfigPath : _configPath;
    int ret = QDialog::DialogCode::Rejected;
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
            sgct::config::Cluster cluster;
            try {
                cluster = sgct::readConfig(winCfg);
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
                winCfg,
                _configPath / "schema/sgct.schema.json"
            );
            if (!err.empty()) {
                editRefusalDialog(
                    "Format Validation Error",
                    std::format("Parsing error found in file '{}'", winCfg),
                    std::format(
                        "{}\n\nThis configuration file is unable to generate a proper "
                        "display",
                        err
                    )
                );
                return;
            }

            if (versionCheck(*cluster.generator)) {
                SgctEdit editor = SgctEdit(
                    cluster,
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

    try {
        std::ofstream outFile;
        outFile.open(savePath, std::ofstream::out);
        sgct::config::GeneratorVersion genEntry = versionMin;
        outFile << sgct::serializeConfig(
            cluster,
            genEntry
        );
    }
    catch (const std::ofstream::failure& e) {
        QMessageBox::critical(
            this,
            "Exception",
            QString::fromStdString(std::format(
                "Error writing data to file '{}': {}", savePath, e.what()
            ))
        );
    }

    // Truncate path to convert this back to path relative to _userConfigPath
    const std::filesystem::path p = std::filesystem::proximate(
        savePath,
        saveWindowCfgPath
    );
    _windowConfigBox->populateList(p.string());
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
