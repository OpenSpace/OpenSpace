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
//
//#include "sgctedit/filesupport.h"
//
//#include <ghoul/misc/assert.h>
//
//FileSupport::FileSupport(QVBoxLayout* parentLayout,
//                         UserConfigurationElements& cfgElements,
//                         SgctConfigElements& sgctElements)
//    : _displayWidget(cfgElements.display)
//    , _orientationWidget(cfgElements.orientation)
//    , _monitors(cfgElements.monitorList)
//    , _cluster(sgctElements.cluster)
//    , _windowList(sgctElements.windowList)
//    , _userConfigPath(cfgElements.configSavePath)
//{
//    createWidgets(parentLayout);
//}
//
//void FileSupport::saveCluster() {
//    if (!_orientationWidget) {
//        return;
//    }
//
//    sgct::config::Scene initScene;
//    initScene.orientation = _orientationWidget->orientationValue();
//    _cluster.nodes.clear();
//    sgct::config::Node tmpNode;
//    tmpNode.address = "localhost";
//    tmpNode.port = 20401;
//    _cluster.nodes.push_back(tmpNode);
//    _cluster.masterAddress = "localhost";
//    _cluster.scene = std::move(initScene);
//    _cluster.firmSync = _orientationWidget->vsyncValue();
//}
//
//void FileSupport::saveUser() {
//    if (!_orientationWidget) {
//        return;
//    }
//    sgct::config::User user;
//    user.eyeSeparation = 0.065f;
//    user.position = { 0.f, 0.f, 4.f };
//    _cluster.users.push_back(user);
//}
//
//std::optional<unsigned int> FileSupport::findGuiWindow() const {
//    for (unsigned int w = 0; w < _displayWidget->nWindows(); ++w) {
//        if (_displayWidget->windowControls()[w]->isGuiWindow()) {
//            return w;
//        }
//    }
//    return std::nullopt;
//}
//
//void FileSupport::saveWindows() {
//    unsigned int windowIndex = 0;
//    for (unsigned int w = 0; w < _displayWidget->nWindows(); ++w) {
//        std::shared_ptr<WindowControl> wCtrl = _displayWidget->windowControls()[w];
//        sgct::config::Window tmpWindow = generateWindow(*wCtrl);
//        tmpWindow.viewports.push_back(generateViewport());
//        tmpWindow.viewports.back().projection = saveProjectionInformation(*wCtrl);
//        tmpWindow.isDecorated = wCtrl->isDecorated();
//        tmpWindow.isFullScreen = isWindowFullscreen(
//            _monitors[wCtrl->monitorNum()],
//            wCtrl->windowSize()
//        );
//        if (tmpWindow.isFullScreen) {
//            tmpWindow.monitor = wCtrl->monitorNum();
//        }
//        saveWindowsWebGui(windowIndex, tmpWindow);
//        if (!wCtrl->windowName().empty()) {
//            tmpWindow.name = wCtrl->windowName();
//        }
//        tmpWindow.id = windowIndex++;
//        _windowList.push_back(tmpWindow);
//    }
//}
//
//sgct::config::Window FileSupport::generateWindow(const WindowControl& wCtrl) const {
//    sgct::config::Window tmpWindow;
//    tmpWindow.size = wCtrl.windowSize();
//    tmpWindow.pos = {
//        _monitors[wCtrl.monitorNum()].x() + wCtrl.windowPos().x,
//        _monitors[wCtrl.monitorNum()].y() + wCtrl.windowPos().y,
//    };
//    return tmpWindow;
//}
//
//void FileSupport::saveWindowsWebGui(unsigned int wIdx, sgct::config::Window& win) {
//    win.viewports.back().isTracked = true;
//    std::optional<unsigned int> webGuiWindowIndex = findGuiWindow();
//    bool isOneOfWindowsSetAsWebGui = webGuiWindowIndex.has_value();
//    if (isOneOfWindowsSetAsWebGui) {
//        if (wIdx == webGuiWindowIndex.value()) {
//            win.viewports.back().isTracked = false;
//            win.tags.push_back("GUI");
//        }
//        win.draw2D = (wIdx == webGuiWindowIndex.value());
//        win.draw3D = !(win.draw2D.value());
//    }
//}
//
//std::string FileSupport::saveFilename() const {
//    return _saveTarget;
//}
//
//void FileSupport::apply() {
//    std::string userCfgTempDir = _userConfigPath;
//    if (userCfgTempDir.back() != '/') {
//        userCfgTempDir += '/';
//    }
//    userCfgTempDir += "temp";
//    if (!std::filesystem::is_directory(absPath(userCfgTempDir))) {
//        std::filesystem::create_directories(absPath(userCfgTempDir));
//    }
//    _saveTarget = userCfgTempDir + "/" + "apply-without-saving.json";
//    saveConfigToSgctFormat();
//    emit accept();
//}
//
//void FileSupport::saveConfigToSgctFormat() {
//    saveCluster();
//    saveWindows();
//    saveUser();
//}
