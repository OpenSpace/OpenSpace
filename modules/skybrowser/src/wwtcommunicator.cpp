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

#include <modules/webbrowser/include/browserinstance.h>
#include <modules/skybrowser/include/utility.h>
#include <modules/webbrowser/include/webkeyboardhandler.h>
#include <modules/webbrowser/webbrowsermodule.h>
#include <ghoul/misc/dictionaryjsonformatter.h>

namespace {
    constexpr std::string_view _loggerCat = "WwtCommunicator";
} // namespace

namespace openspace {

WwtCommunicator::WwtCommunicator(const ghoul::Dictionary& dictionary)
    : Browser(dictionary)
{}

WwtCommunicator::~WwtCommunicator() {}

void WwtCommunicator::selectImage(const std::string& url, int i) {
    // Ensure there are no duplicates
    auto it = findSelectedImage(i);

    if (it == _selectedImages.end()) {
        // Push newly selected image to front
        _selectedImages.push_front(std::pair<int, double>(i, 1.0));

        // If wwt has not loaded the collection yet, wait with passing the message
        if (_isImageCollectionLoaded) {
            addImageLayerToWwt(url, i);
        }
    }
}

void WwtCommunicator::addImageLayerToWwt(const std::string& url, int i) {
    // Index of image is used as layer ID as it is unique in the image data set
    sendMessageToWwt(addImageMessage(std::to_string(i), url));
    sendMessageToWwt(setImageOpacityMessage(std::to_string(i), 1.0));
}

void WwtCommunicator::removeSelectedImage(int i) {
    // Remove from selected list
    auto it = findSelectedImage(i);

    if (it != _selectedImages.end()) {
        _selectedImages.erase(it);
        sendMessageToWwt(removeImageMessage(std::to_string(i)));
    }
}

void WwtCommunicator::sendMessageToWwt(const ghoul::Dictionary& msg) const {
    std::string script = "sendMessageToWWT(" + ghoul::formatJson(msg) + ");";
    executeJavascript(script);
}

std::vector<int> WwtCommunicator::selectedImages() const {
    std::vector<int> selectedImagesVector;
    for (const std::pair<int, double>& image : _selectedImages) {
        selectedImagesVector.push_back(image.first);
    }
    return selectedImagesVector;
}

std::vector<double> WwtCommunicator::opacities() const {
    std::vector<double> opacities;
    for (const std::pair<int, double>& image : _selectedImages) {
        opacities.push_back(image.second);
    }
    return opacities;
}

double WwtCommunicator::borderRadius() const {
    return _borderRadius;
}

void WwtCommunicator::setTargetRoll(double roll) {
    _targetRoll = roll;
}

void WwtCommunicator::setVerticalFov(double vfov) {
    _verticalFov = vfov;
    _equatorialAimIsDirty = true;
}

void WwtCommunicator::setWebpageBorderColor(glm::ivec3 color) const {
    std::string stringColor = fmt::format("{},{},{}", color.x, color.y, color.z);
    std::string scr = "setBackgroundColor('rgb(" + stringColor + ")');";
    executeJavascript(scr);
}

void WwtCommunicator::setEquatorialAim(glm::dvec2 equatorial) {
    _equatorialAim = std::move(equatorial);
    _equatorialAimIsDirty = true;
}

void WwtCommunicator::setBorderColor(glm::ivec3 color) {
    _borderColor = std::move(color);
    _borderColorIsDirty = true;
}

void WwtCommunicator::setBorderRadius(double radius) {
    _borderRadius = radius;
    std::string scr = "setBorderRadius(" + std::to_string(radius) + ");";
    executeJavascript(scr);
}

void WwtCommunicator::updateBorderColor() const {
    setWebpageBorderColor(_borderColor);
}

void WwtCommunicator::updateAim() const {
    // Message WorldWide Telescope current view
    ghoul::Dictionary msg = moveCameraMessage(_equatorialAim, _verticalFov, _targetRoll);
    sendMessageToWwt(msg);
}

glm::dvec2 WwtCommunicator::fieldsOfView() const {
    glm::dvec2 browserFov = glm::dvec2(verticalFov() * browserRatio(), verticalFov());
    return browserFov;
}

bool WwtCommunicator::isImageCollectionLoaded() const {
    return _isImageCollectionLoaded;
}

std::deque<std::pair<int, double>>::iterator WwtCommunicator::findSelectedImage(int i) {
    auto it = std::find_if(_selectedImages.begin(), _selectedImages.end(),
        [i](std::pair<int, double>& pair) {
            return (pair.first == i);
        });
    return it;
}

glm::dvec2 WwtCommunicator::equatorialAim() const {
    return _equatorialAim;
}

void WwtCommunicator::setImageOrder(int i, int order) {
    // Find in selected images list
    auto current = findSelectedImage(i);
    auto target = _selectedImages.begin() + order;

    // Make sure the image was found in the list
    if (current != _selectedImages.end() && target != _selectedImages.end()) {
        // Swap the two images
        std::iter_swap(current, target);
    }

    int reverseOrder = static_cast<int>(_selectedImages.size()) - order - 1;
    ghoul::Dictionary message = setLayerOrderMessage(std::to_string(i), reverseOrder);
    sendMessageToWwt(message);
}

void WwtCommunicator::loadImageCollection(const std::string& collection) {
    if (!_isImageCollectionLoaded) {
        sendMessageToWwt(loadCollectionMessage(collection));
    }
}

void WwtCommunicator::setImageOpacity(int i, float opacity) {
    auto it = findSelectedImage(i);
    it->second = opacity;

    ghoul::Dictionary msg = setImageOpacityMessage(std::to_string(i), opacity);
    sendMessageToWwt(msg);
}

void WwtCommunicator::hideChromeInterface() const {
    std::string script = "sendMessageToWWT({event : \"modify_settings\", "
        "settings : [[\"hideAllChrome\", true]], target: \"app\"});";
    executeJavascript(script);
}

void WwtCommunicator::update() {
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
    if (_shouldReload) {
        _isImageCollectionLoaded = false;
    }
    Browser::update();
}

void WwtCommunicator::setImageCollectionIsLoaded(bool isLoaded) {
    _isImageCollectionLoaded = isLoaded;
}

void WwtCommunicator::setIdInBrowser(const std::string& id) const {
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
                                                     bool shouldMoveInstantly) const
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

ghoul::Dictionary WwtCommunicator::loadCollectionMessage(const std::string& url) const {
    using namespace std::string_literals;

    ghoul::Dictionary msg;
    msg.setValue("event", "load_image_collection"s);
    msg.setValue("url", url);
    msg.setValue("loadChildFolders", true);
    return msg;
}

ghoul::Dictionary WwtCommunicator::setForegroundMessage(const std::string& name) const {
    using namespace std::string_literals;

    ghoul::Dictionary msg;
    msg.setValue("event", "set_foreground_by_name"s);
    msg.setValue("name", name);
    return msg;
}

ghoul::Dictionary WwtCommunicator::addImageMessage(const std::string& id,
                                                   const std::string& url) const
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

ghoul::Dictionary WwtCommunicator::removeImageMessage(const std::string& imageId) const {
    using namespace std::string_literals;

    ghoul::Dictionary msg;
    msg.setValue("event", "image_layer_remove"s);
    msg.setValue("id", imageId);
    return msg;
}

ghoul::Dictionary WwtCommunicator::setImageOpacityMessage(const std::string& imageId,
                                                          double opacity) const
{
    using namespace std::string_literals;

    ghoul::Dictionary msg;
    msg.setValue("event", "image_layer_set"s);
    msg.setValue("id", imageId);
    msg.setValue("setting", "opacity"s);
    msg.setValue("value", opacity);
    return msg;
}

ghoul::Dictionary WwtCommunicator::setLayerOrderMessage(const std::string& id, int order)
{
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

} // namespace openspace
