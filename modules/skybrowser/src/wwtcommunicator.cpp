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

#include <modules/skybrowser/include/wwtcommunicator.h>

#include <modules/webbrowser/include/browserinstance.h>
#include <modules/skybrowser/include/utility.h>
#include <modules/webbrowser/include/webkeyboardhandler.h>
#include <modules/webbrowser/webbrowsermodule.h>
#include <ghoul/misc/dictionaryjsonformatter.h>
#include <deque>

namespace {
    // WWT messages
    ghoul::Dictionary moveCameraMessage(const glm::dvec2& celestCoords, double fov,
                                        double roll)
    {
        using namespace std::string_literals;

        ghoul::Dictionary msg;
        msg.setValue("event", "center_on_coordinates"s);
        msg.setValue("ra", celestCoords.x);
        msg.setValue("dec", celestCoords.y);
        msg.setValue("fov", fov);
        msg.setValue("roll", roll);
        msg.setValue("instant", true);
        return msg;
    }

    ghoul::Dictionary loadCollectionMessage(const std::string& url) {
        using namespace std::string_literals;

        ghoul::Dictionary msg;
        msg.setValue("event", "load_image_collection"s);
        msg.setValue("url", url);
        msg.setValue("loadChildFolders", true);
        return msg;
    }

    ghoul::Dictionary addImageMessage(const std::string& url) {
        using namespace std::string_literals;

        ghoul::Dictionary msg;
        msg.setValue("event", "image_layer_create"s);
        msg.setValue("id", url);
        msg.setValue("url", url);
        msg.setValue("mode", "preloaded"s);
        msg.setValue("goto", false);
        return msg;
    }

    ghoul::Dictionary removeImageMessage(const std::string& imageId) {
        using namespace std::string_literals;

        ghoul::Dictionary msg;
        msg.setValue("event", "image_layer_remove"s);
        msg.setValue("id", imageId);
        return msg;
    }

    ghoul::Dictionary setImageOpacityMessage(const std::string& imageId, double opacity) {
        using namespace std::string_literals;

        ghoul::Dictionary msg;
        msg.setValue("event", "image_layer_set"s);
        msg.setValue("id", imageId);
        msg.setValue("setting", "opacity"s);
        msg.setValue("value", opacity);
        return msg;
    }

    ghoul::Dictionary setLayerOrderMessage(const std::string& imageUrl, int order) {
        static int MessageCounter = 0;

        // The lower the layer order, the more towards the back the image is placed
        // 0 is the background
        using namespace std::string_literals;

        ghoul::Dictionary msg;
        msg.setValue("event", "image_layer_order"s);
        msg.setValue("id", imageUrl);
        msg.setValue("order", order);
        msg.setValue("version", MessageCounter);

        MessageCounter++;
        return msg;
    }

    constexpr openspace::properties::Property::PropertyInfo VerticalFovInfo = {
        "VerticalFov",
        "Vertical Field Of View",
        "The vertical field of view of the target.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(WwtCommunicator)]] Parameters {
        // [[codegen::verbatim(VerticalFovInfo.description)]]
        std::optional<double> verticalFov;
    };
    #include "wwtcommunicator_codegen.cpp"
} // namespace

namespace openspace {

WwtCommunicator::WwtCommunicator(const ghoul::Dictionary& dictionary)
    : Browser(dictionary)
    , _verticalFov(VerticalFovInfo, 10.0, 0.00000000001, 70.0)
{
    // Handle target dimension property
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _verticalFov = p.verticalFov.value_or(_verticalFov);
    _verticalFov.setReadOnly(true);
}

void WwtCommunicator::update() {
    // Cap how messages are passed
    const std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
    const std::chrono::system_clock::duration timeSinceLastUpdate = now - _lastUpdateTime;

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

void WwtCommunicator::selectImage(const std::string& url) {
    // Ensure there are no duplicates
    auto it = findSelectedImage(url);

    if (it == _selectedImages.end()) {
        // Push newly selected image to front
        _selectedImages.emplace_front(url, 1.0);

        // If wwt has not loaded the collection yet, wait with passing the message
        if (_isImageCollectionLoaded) {
            addImageLayerToWwt(url);
        }
    }
}

void WwtCommunicator::addImageLayerToWwt(const std::string& imageUrl) {
    // Index of image is used as layer ID as it is unique in the image data set
    sendMessageToWwt(addImageMessage(imageUrl));
    sendMessageToWwt(setImageOpacityMessage(imageUrl, 1.0));
}

void WwtCommunicator::removeSelectedImage(const std::string& imageUrl) {
    // Remove from selected list
    auto it = findSelectedImage(imageUrl);
    if (it != _selectedImages.end()) {
        _selectedImages.erase(it);
        sendMessageToWwt(removeImageMessage(imageUrl));
    }
}

void WwtCommunicator::sendMessageToWwt(const ghoul::Dictionary& msg) const {
    std::string m = ghoul::formatJson(msg);
    executeJavascript(std::format("sendMessageToWWT({});", m));
}

std::vector<std::string> WwtCommunicator::selectedImages() const {
    std::vector<std::string> selectedImagesVector;
    selectedImagesVector.resize(_selectedImages.size());
    std::transform(
        _selectedImages.cbegin(),
        _selectedImages.cend(),
        selectedImagesVector.begin(),
        [](const std::pair<std::string, double>& image) { return image.first; }
    );
    return selectedImagesVector;
}

std::vector<double> WwtCommunicator::opacities() const {
    std::vector<double> opacities;
    opacities.resize(_selectedImages.size());
    std::transform(
        _selectedImages.cbegin(),
        _selectedImages.cend(),
        opacities.begin(),
        [](const std::pair<std::string, double>& image) { return image.second; }
    );
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

void WwtCommunicator::setEquatorialAim(glm::dvec2 equatorial) {
    _equatorialAim = std::move(equatorial);
    _equatorialAimIsDirty = true;
}

void WwtCommunicator::setBorderColor(glm::ivec3 color) {
    _wwtBorderColor = std::move(color);
    _borderColorIsDirty = true;
}

void WwtCommunicator::setBorderRadius(double radius) {
    _borderRadius = radius;
    const std::string scr = std::format("setBorderRadius({});", radius);
    executeJavascript(scr);
}

void WwtCommunicator::updateBorderColor() const {
    const std::string script = std::format(
        "setBackgroundColor('rgb({},{},{})');",
        _wwtBorderColor.x, _wwtBorderColor.y, _wwtBorderColor.z
    );
    executeJavascript(script);
}

void WwtCommunicator::updateAim() const {
    // Message WorldWide Telescope current view
    const ghoul::Dictionary msg = moveCameraMessage(
        _equatorialAim,
        _verticalFov,
        _targetRoll
    );
    sendMessageToWwt(msg);
}

glm::dvec2 WwtCommunicator::fieldsOfView() const {
    const double vFov = verticalFov();
    const double hFov = vFov * browserRatio();
    return glm::dvec2(hFov, vFov);
}

bool WwtCommunicator::isImageCollectionLoaded() const {
    return _isImageCollectionLoaded;
}

SelectedImageDeque::iterator WwtCommunicator::findSelectedImage(
                                                              const std::string& imageUrl)
{
    auto it = std::find_if(
        _selectedImages.begin(),
        _selectedImages.end(),
        [imageUrl](const std::pair<std::string, double>& pair) {
            return pair.first == imageUrl;
        }
    );
    return it;
}

glm::dvec2 WwtCommunicator::equatorialAim() const {
    return _equatorialAim;
}

void WwtCommunicator::setImageOrder(const std::string& imageUrl, int order) {
    // Find in selected images list
    auto current = findSelectedImage(imageUrl);
    const int currentIndex = static_cast<int>(
        std::distance(_selectedImages.begin(), current)
    );

    std::deque<std::pair<std::string, double>> newDeque;

    for (int i = 0; i < static_cast<int>(_selectedImages.size()); i++) {
        if (i == currentIndex) {
            continue;
        }
        else if (i == order) {
            if (order < currentIndex) {
                newDeque.push_back(*current);
                newDeque.push_back(_selectedImages[i]);
            }
            else {
                newDeque.push_back(_selectedImages[i]);
                newDeque.push_back(*current);
            }
        }
        else {
            newDeque.push_back(_selectedImages[i]);
        }
    }

    _selectedImages = newDeque;
    const int reverseOrder = static_cast<int>(_selectedImages.size()) - order - 1;
    const ghoul::Dictionary message = setLayerOrderMessage(imageUrl, reverseOrder);
    sendMessageToWwt(message);
}

void WwtCommunicator::loadImageCollection(const std::string& collection) {
    if (!_isImageCollectionLoaded) {
        sendMessageToWwt(loadCollectionMessage(collection));
    }
}

void WwtCommunicator::setImageOpacity(const std::string& imageUrl, float opacity) {
    auto it = findSelectedImage(imageUrl);
    it->second = opacity;

    const ghoul::Dictionary msg = setImageOpacityMessage(imageUrl, opacity);
    sendMessageToWwt(msg);
}

void WwtCommunicator::hideChromeInterface() const {
    const std::string script = "sendMessageToWWT({event : \"modify_settings\", "
        "settings : [[\"hideAllChrome\", true]], target: \"app\"});";
    executeJavascript(script);
}

void WwtCommunicator::setImageCollectionIsLoaded(bool isLoaded) {
    _isImageCollectionLoaded = isLoaded;
}

void WwtCommunicator::setIdInBrowser(const std::string& id) const {
    // Send ID to its browser
    executeJavascript(std::format("setId('{}')", id));
}

glm::ivec3 WwtCommunicator::borderColor() const {
    return _wwtBorderColor;
}

double WwtCommunicator::verticalFov() const {
    return _verticalFov;
}

} // namespace openspace
