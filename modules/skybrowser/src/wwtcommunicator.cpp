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

#include <modules/skybrowser/include/wwtcommunicator.h>

#include <modules/webbrowser/webbrowsermodule.h>
#include <modules/webbrowser/include/webkeyboardhandler.h>
#include <modules/webbrowser/include/browserinstance.h>
#include <ghoul/misc/dictionaryjsonformatter.h>
#include <modules/skybrowser/include/utility.h>

namespace {
    constexpr const char* _loggerCat = "WwtCommunicator";
} // namespace

namespace openspace {

WwtCommunicator::WwtCommunicator(const ghoul::Dictionary& dictionary)
    : Browser(dictionary) {}

WwtCommunicator::~WwtCommunicator() {}

void WwtCommunicator::displayImage(const std::string& url, int i) {   
    // Ensure there are no duplicates
    auto it = std::find(_selectedImages.begin(), _selectedImages.end(), i);
    if (it == _selectedImages.end()) {
        // Push newly selected image to front
        _selectedImages.push_front(i);
        // Index of image is used as layer ID as it is unique in the image data set
        sendMessageToWwt(addImageMessage(std::to_string(i), url));
        sendMessageToWwt(setImageOpacityMessage(std::to_string(i), 1.0));
    }
}

void WwtCommunicator::removeSelectedImage(int i) {
    // Remove from selected list
    auto it = std::find(_selectedImages.begin(), _selectedImages.end(), i);

    if (it != std::end(_selectedImages)) {
        _selectedImages.erase(it);
        sendMessageToWwt(removeImageMessage(std::to_string(i)));
    }
}

void WwtCommunicator::sendMessageToWwt(const ghoul::Dictionary& msg) {
    std::string script = "sendMessageToWWT(" + ghoul::formatJson(msg) + ");";
    executeJavascript(script);
}

const std::deque<int>& WwtCommunicator::getSelectedImages() const {
    return _selectedImages;
}

void WwtCommunicator::setTargetRoll(double roll)
{
    _targetRoll = roll;
}

void WwtCommunicator::setVerticalFov(double vfov) {
    _verticalFov = vfov;
    _equatorialAimIsDirty = true;
}

void WwtCommunicator::setWebpageBorderColor(glm::ivec3 color) {
    std::string stringColor = std::to_string(color.x) + ","
        + std::to_string(color.y) + "," + std::to_string(color.z);
    std::string script = "document.body.style.backgroundColor = 'rgb("
        + stringColor + ")';";
    executeJavascript(script);
}

void WwtCommunicator::setIsSyncedWithWwt(bool isSynced) {
    _isSyncedWithWwt = isSynced;
}

void WwtCommunicator::setEquatorialAim(glm::dvec2 equatorial) {
    _equatorialAim = std::move(equatorial);
    _equatorialAimIsDirty = true;
}

void WwtCommunicator::setBorderColor(glm::ivec3 color) {
    _borderColor = std::move(color);
    _borderColorIsDirty = true;
}

void WwtCommunicator::highlight(const glm::ivec3& addition) {
    setWebpageBorderColor(_borderColor + addition);
}

void WwtCommunicator::removeHighlight(const glm::ivec3& removal) {
    setWebpageBorderColor(_borderColor - removal);
}

void WwtCommunicator::updateBorderColor() {
    setWebpageBorderColor(_borderColor);
}

void WwtCommunicator::updateAim() {
    // Message WorldWide Telescope current view
    ghoul::Dictionary message = moveCameraMessage(
        _equatorialAim,
        _verticalFov,
        _targetRoll
    );
    sendMessageToWwt(message);
}

glm::dvec2 WwtCommunicator::fieldsOfView() const {
    glm::dvec2 browserFov = glm::dvec2(verticalFov() * browserRatio(), verticalFov());

    return browserFov;
}

bool WwtCommunicator::hasLoadedImages() const {
    return _hasLoadedImages;
}

glm::dvec2 WwtCommunicator::equatorialAim() const {
    return _equatorialAim;
}

void WwtCommunicator::setImageOrder(int i, int order) {
    // Find in selected images list
    auto current = std::find(
        _selectedImages.begin(),
        _selectedImages.end(),
        i
    );
    auto target = std::begin(_selectedImages) + order;

    // Make sure the image was found in the list
    if (current != std::end(_selectedImages) && target != std::end(_selectedImages)) {
        // Swap the two images
        std::iter_swap(current, target);
    }

    int reverseOrder = _selectedImages.size() - order - 1;
    ghoul::Dictionary message = setLayerOrderMessage(std::to_string(i), reverseOrder);
    sendMessageToWwt(message);
}

void WwtCommunicator::loadImageCollection(const std::string& collection) {
    sendMessageToWwt(loadCollectionMessage(collection));
    _hasLoadedImages = true;
}

void WwtCommunicator::setImageOpacity(int i, float opacity) {
    ghoul::Dictionary msg = setImageOpacityMessage(std::to_string(i), opacity);
    sendMessageToWwt(msg);
}

void WwtCommunicator::hideChromeInterface(bool shouldHide) {
    ghoul::Dictionary msg = hideChromeGuiMessage(shouldHide);
    sendMessageToWwt(msg);
}

void WwtCommunicator::update() {
    Browser::update();        
    // Cap how messages are passed
    std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
    std::chrono::system_clock::duration timeSinceLastUpdate = now - _lastUpdateTime;

    if (timeSinceLastUpdate > TimeUpdateInterval) {
        if (_equatorialAimIsDirty) {
            updateAim();
            _equatorialAimIsDirty = false;
        }
        if (_borderColorIsDirty) {
            updateBorderColor();
            _borderColorIsDirty = false;
        }
        _lastUpdateTime = std::chrono::system_clock::now();
    }
}

void WwtCommunicator::render() {
    Browser::render();
}

void WwtCommunicator::initializeGL() {
    Browser::initializeGL();
}

void WwtCommunicator::deinitializeGL() {
    Browser::deinitializeGL();
}

void WwtCommunicator::setHasLoadedImages(bool isLoaded) {
    _hasLoadedImages = isLoaded;
}

void WwtCommunicator::setIdInBrowser(const std::string& id) {
    // Send ID to it's browser
    executeJavascript("setId('" + id + "')");
}

glm::ivec3 WwtCommunicator::borderColor() const {
    return _borderColor;
}

double WwtCommunicator::verticalFov() const {
    return _verticalFov;
}

// WWT messages
ghoul::Dictionary WwtCommunicator::moveCameraMessage(const glm::dvec2& celestCoords, 
                                                     double fov, double roll, 
                                                     bool shouldMoveInstantly) 
{
    using namespace std::string_literals;
    ghoul::Dictionary msg;

    msg.setValue("event", "center_on_coordinates"s);
    msg.setValue("ra", celestCoords.x);
    msg.setValue("dec", celestCoords.y);
    msg.setValue("fov", fov);
    msg.setValue("roll", roll);
    msg.setValue("instant", shouldMoveInstantly);

    return msg;
}

ghoul::Dictionary WwtCommunicator::loadCollectionMessage(const std::string& url) {
    using namespace std::string_literals;
    ghoul::Dictionary msg;
    msg.setValue("event", "load_image_collection"s);
    msg.setValue("url", url);
    msg.setValue("loadChildFolders", true);

    return msg;
}

ghoul::Dictionary WwtCommunicator::setForegroundMessage(const std::string& name) {
    using namespace std::string_literals;
    ghoul::Dictionary msg;
    msg.setValue("event", "set_foreground_by_name"s);
    msg.setValue("name", name);

    return msg;
}

ghoul::Dictionary WwtCommunicator::addImageMessage(const std::string& id, 
                                                   const std::string& url) 
{
    using namespace std::string_literals;
    ghoul::Dictionary msg;
    msg.setValue("event", "image_layer_create"s);
    msg.setValue("id", id);
    msg.setValue("url", url);
    msg.setValue("mode", "preloaded"s);
    msg.setValue("goto", false);

    return msg;
}

ghoul::Dictionary WwtCommunicator::removeImageMessage(const std::string& imageId) {
    using namespace std::string_literals;
    ghoul::Dictionary msg;
    msg.setValue("event", "image_layer_remove"s);
    msg.setValue("id", imageId);

    return msg;
}

ghoul::Dictionary WwtCommunicator::setImageOpacityMessage(const std::string& imageId, 
                                                          double opacity) 
{
    using namespace std::string_literals;
    ghoul::Dictionary msg;
    msg.setValue("event", "image_layer_set"s);
    msg.setValue("id", imageId);
    msg.setValue("setting", "opacity"s);
    msg.setValue("value", opacity);

    return msg;
}

ghoul::Dictionary WwtCommunicator::setLayerOrderMessage(const std::string& id, int order) {
    // The lower the layer order, the more towards the back the image is placed
    // 0 is the background
    using namespace std::string_literals;
    ghoul::Dictionary msg;
    msg.setValue("event", "image_layer_order"s);
    msg.setValue("id", id);
    msg.setValue("order", order);
    msg.setValue("version", messageCounter);

    messageCounter++;

    return msg;
}

ghoul::Dictionary WwtCommunicator::hideChromeGuiMessage(bool isHidden) {
    using namespace std::string_literals;
    ghoul::Dictionary msg;
    msg.setValue("event", "modify_settings"s);
    msg.setValue("settings", "[[\"hideAllChrome\", true]]"s);
    msg.setValue("target", "app"s);

    return msg;
}
} // namespace openspace
