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

#ifndef __OPENSPACE_UI_LAUNCHER___SGCTEDIT___H__
#define __OPENSPACE_UI_LAUNCHER___SGCTEDIT___H__

#include <QDialog>

#include <sgct/config.h>
#include <filesystem>
#include <string>

class DisplayWindowUnion;
class QCheckBox;
class QComboBox;
class QLineEdit;

const sgct::config::GeneratorVersion VersionMin { "SgctWindowConfig", 1, 1 };
const sgct::config::GeneratorVersion VersionLegacy18 { "OpenSpace", 0, 18 };
const sgct::config::GeneratorVersion VersionLegacy19 { "OpenSpace", 0, 19 };

class SgctEdit final : public QDialog {
Q_OBJECT
public:
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
    SgctEdit(sgct::config::Cluster cluster, std::string configName,
        std::filesystem::path configBasePath, QWidget* parent);

    /**
     * Returns the saved filename.
     *
     * \return The saved filename in std::string
     */
    std::filesystem::path saveFilename() const;

private:
    void saveCluster();
    void apply();

    DisplayWindowUnion* _displayWidget = nullptr;

    QCheckBox* _checkBoxVsync = nullptr;
    QLineEdit* _linePitch = nullptr;
    QLineEdit* _lineRoll = nullptr;
    QLineEdit* _lineYaw = nullptr;

    sgct::config::Cluster _cluster;
    const std::filesystem::path _userConfigPath;

    std::string _configurationFilename;
};

#endif // __OPENSPACE_UI_LAUNCHER___SGCTEDIT___H__
