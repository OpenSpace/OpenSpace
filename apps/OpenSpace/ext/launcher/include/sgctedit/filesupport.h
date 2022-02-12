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

#include <sgctedit/display.h>
#include <sgctedit/orientation.h>
#include <sgct/config.h>
#include <QFrame>
#include <QLabel>
#include <QLayout>
#include <QPushButton>
#include <QVector>
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
    sgct::config::SpoutOutputProjection
>;

class FileSupport : public QWidget {
Q_OBJECT
public:
    FileSupport(QVBoxLayout* parentLayout, std::vector<QRect>& monitorList,
        std::shared_ptr<Display> display, Orientation* orientation,
        std::vector<sgct::config::Window>& windowList, sgct::config::Cluster& cluster,
        std::function<void(bool)> cb);
    std::string saveFilename();

private slots:
    void filenameEdited(const QString& newString);
    void cancel();
    void save();

private:
    bool isWindowFullscreen(unsigned int monitorIdx, sgct::ivec2 wDims);
    std::optional<unsigned int> findGuiWindow();
    void saveCluster();
    void saveWindows();
    void saveUser();
    ProjectionOptions saveProjectionInformation(
        std::shared_ptr<WindowControl> winControl);
    ProjectionOptions saveProjectionSpout(std::shared_ptr<WindowControl> winControl);
    ProjectionOptions saveProjectionNoSpout(std::shared_ptr<WindowControl> winControl);
    sgct::config::Viewport generateViewport();
    sgct::config::Window saveWindowsDimensions(std::shared_ptr<WindowControl> wCtrl);
    void saveWindowsWebGui(unsigned int wIdx, sgct::config::Window& win);

    QHBoxLayout* _layoutButtonBox = nullptr;
    QPushButton* _saveButton = nullptr;
    QPushButton* _cancelButton = nullptr;
    std::shared_ptr<Display> _displayWidget;
    Orientation* _orientationWidget;
    std::vector<QRect>& _monitors;
    sgct::config::Cluster& _cluster;
    std::vector<sgct::config::Window>& _windowList;
    QLineEdit* _lineFilename = nullptr;
    std::function<void(bool)> _finishedCallback;
};

#endif // __OPENSPACE_UI_LAUNCHER___FILESUPPORT___H__
