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

#ifndef __OPENSPACE_UI_LAUNCHER___SGCTEDIT___H__
#define __OPENSPACE_UI_LAUNCHER___SGCTEDIT___H__

#include <QDialog>

#include <sgct/config.h>
#include <sgctedit/windowcontrol.h>
#include <QColor>
#include <array>
#include <filesystem>
#include <string>

class DisplayWindowUnion;
class SettingsWidget;
class QBoxLayout;
class QWidget;

const sgct::config::GeneratorVersion versionMin { "SgctWindowConfig", 1, 1 };
const sgct::config::GeneratorVersion versionLegacy18 { "OpenSpace", 0, 18 };
const sgct::config::GeneratorVersion versionLegacy19 { "OpenSpace", 0, 19 };

class SgctEdit final : public QDialog {
Q_OBJECT
public:
    /**
     * Constructor for SgctEdit class, the underlying class for the full window
     * configuration editor. Used when creating a new config.
     *
     * \param parent The Qt QWidget parent object
     * \param userConfigPath A string containing the file path of the user config
     *        directory where all window configs are stored
     */
    SgctEdit(QWidget* parent, std::filesystem::path userConfigPath);

    /**
     * Constructor for SgctEdit class, the underlying class for the full window
     * configuration editor. Used when editing an existing config.
     *
     * \param cluster The #sgct::config::Cluster object containing all data of the
     *                imported window cluster configuration.
     * \param configName The name of the window configuration filename
     * \param configBasePath The path to the folder where default config files reside
     * \param parent Pointer to parent Qt widget
     */
    SgctEdit(sgct::config::Cluster& cluster, std::string configName,
        std::filesystem::path& configBasePath, QWidget* parent);

    /**
     * Returns the saved filename.
     *
     * \return The saved filename in std::string
     */
    std::filesystem::path saveFilename() const;

    /**
     * Returns the generated Cluster object.
     *
     * \return The generated Cluster object
     */
    sgct::config::Cluster cluster() const;

    /**
     * Called when the number of windows that should be displayed changes.
     *
     * \param newCount The new number of windows included
     */
    void nWindowsDisplayedChanged(int newCount);

    /**
     * Called when the checkbox for GUI only on first window is clicked.
     *
     * \param checked `true` if GUI is selected for first window only.
     */
    void firstWindowGuiOptionClicked(bool checked);

    /**
     * Called when the QComboBox is selected and has a new value
     *
     * \param text The QString of the selected value
     */
    void firstWindowGraphicsSelectionChanged(const QString& text);

private:
    std::vector<QRect> createMonitorInfoSet();
    void createWidgets(const std::vector<QRect>& monitorSizes, unsigned int nWindows,
        bool setToDefaults);
    void generateConfiguration();
    void generateConfigSetupVsync();
    void generateConfigUsers();
    void generateConfigAddresses(sgct::config::Node& node);
    void generateConfigResizeWindowsAccordingToSelected(sgct::config::Node& node);
    void generateConfigIndividualWindowSettings(sgct::config::Node& node);
    void setupProjectionTypeInGui(sgct::config::Viewport& vPort, WindowControl* wCtrl);
    void setupStateOfUiOnFirstWindow(size_t nWindows);
    void deleteFromTags(sgct::config::Window& window);

    void save();
    void apply();

    DisplayWindowUnion* _displayWidget = nullptr;
    SettingsWidget* _settingsWidget = nullptr;
    sgct::config::Cluster _cluster;
    const std::filesystem::path _userConfigPath;
    const std::array<QColor, 4> _colorsForWindows = {
        QColor(0x2B, 0x9E, 0xC3),
        QColor(0xFC, 0xAB, 0x10),
        QColor(0x44, 0xAF, 0x69),
        QColor(0xF8, 0x33, 0x3C)
    };
    std::string _configurationFilename;

    QBoxLayout* _layoutButtonBox = nullptr;
    QPushButton* _saveButton = nullptr;
    QPushButton* _cancelButton = nullptr;
    QPushButton* _applyButton = nullptr;
    std::string _saveTarget;
    bool _didImportValues = false;
};

#endif // __OPENSPACE_UI_LAUNCHER___SGCTEDIT___H__
