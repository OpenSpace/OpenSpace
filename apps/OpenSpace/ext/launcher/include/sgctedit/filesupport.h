/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_UI_LAUNCHER___FILESUPPORT___H__
#define __OPENSPACE_UI_LAUNCHER___FILESUPPORT___H__

#include <QWidget>

#include <ghoul/filesystem/filesystem.h>
#include <sgctedit/displaywindowunion.h>
#include <sgctedit/orientation.h>
#include <sgct/config.h>
#include <QFileDialog>
#include <QFrame>
#include <QLabel>
#include <QLayout>
#include <QPushButton>
#include <QVector>
#include <filesystem>
#include <memory>
#include <vector>

using ProjectionOptions = std::variant<
    sgct::config::NoProjection,
    sgct::config::CylindricalProjection,
    sgct::config::EquirectangularProjection,
    sgct::config::FisheyeProjection,
    sgct::config::PlanarProjection,
    sgct::config::ProjectionPlane,
    sgct::config::SphericalMirrorProjection,
    sgct::config::SpoutOutputProjection,
    sgct::config::SpoutFlatProjection
>;

struct SgctConfigElements {
    std::vector<sgct::config::Window>& windowList;
    sgct::config::Cluster& cluster;
};

struct UserConfigurationElements {
    std::vector<QRect>& monitorList;
    std::shared_ptr<DisplayWindowUnion> display;
    Orientation* orientation;
    const std::string configSavePath;
};

class FileSupport : public QWidget {
Q_OBJECT
public:
    /**
     * Constructor for FileSupport class, which saves the window configuration settings
     * into the SGCT json structure according to the sgct code
     *
     * \param parentLayout Qt vertical (QVBoxLayout) layout where controls are added
     * \param cfgElements struct of elements needed to read user settings from GUI
     * \param sgctElements struct of the window and cluster objects needed for saving
     */
    FileSupport(QVBoxLayout* parentLayout, UserConfigurationElements& cfgElements,
        SgctConfigElements& sgctElements);
    
    std::string saveFilename() const;

signals:
    void accept();
    void reject();

private slots:
    void save();
    void apply();

private:
    void createWidgets(QVBoxLayout* layout);

    std::optional<unsigned int> findGuiWindow() const;
    void saveConfigToSgctFormat();
    void saveCluster();
    void saveWindows();
    void saveUser();
    sgct::config::Window generateWindow(const WindowControl& wCtrl) const;
    void saveWindowsWebGui(unsigned int wIdx, sgct::config::Window& win);

    QHBoxLayout* _layoutButtonBox = nullptr;
    QPushButton* _saveButton = nullptr;
    QPushButton* _cancelButton = nullptr;
    QPushButton* _applyButton = nullptr;
    std::shared_ptr<DisplayWindowUnion> _displayWidget;
    Orientation* _orientationWidget;
    std::vector<QRect>& _monitors;
    sgct::config::Cluster& _cluster;
    std::vector<sgct::config::Window>& _windowList;
    const std::string _userConfigPath;
    std::string _saveTarget;
};

#endif // __OPENSPACE_UI_LAUNCHER___FILESUPPORT___H__
