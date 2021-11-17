/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <ghoul/misc/dictionaryjsonformatter.h> // formatJson
#include <modules/skybrowser/include/utility.h>


namespace {
    constexpr const char* _loggerCat = "WwtCommunicator";

    constexpr const openspace::properties::Property::PropertyInfo BorderColorInfo =
    {
        "BorderColor",
        "Border Color",
        "The color of the border of the sky browser."
    };
    constexpr const openspace::properties::Property::PropertyInfo VerticalFovInfo =
    {
        "VerticalFieldOfView",
        "Vertical Field Of View",
        "The vertical field of view in degrees."
    };

} // namespace

namespace openspace {

    WwtCommunicator::WwtCommunicator(const ghoul::Dictionary& dictionary)
        : Browser(dictionary),
        _verticalFov(VerticalFovInfo, 10.f, 0.01f, 70.0f),
        _borderColor(BorderColorInfo, glm::ivec3(200), glm::ivec3(0), glm::ivec3(255))
    {

    }

    WwtCommunicator::~WwtCommunicator() {

    }

    void WwtCommunicator::displayImage(const std::string& url, int i)
    {   
        // Ensure there are no duplicates
        auto it = std::find(std::begin(_selectedImages), std::end(_selectedImages), i);
        if (it == std::end(_selectedImages)) {
            // Push newly selected image to front
            _selectedImages.push_front(i);
            // Index of image is used as layer ID as it is unique in the image data set
            sendMessageToWwt(wwtmessage::addImage(std::to_string(i), url));
            sendMessageToWwt(wwtmessage::setImageOpacity(std::to_string(i), 1.0));
        }
    }

    void WwtCommunicator::removeSelectedImage(const int i) {
        // Remove from selected list
        auto it = std::find(std::begin(_selectedImages), std::end(_selectedImages), i);

        if (it != std::end(_selectedImages)) {
            _selectedImages.erase(it);
            sendMessageToWwt(wwtmessage::removeImage(std::to_string(i)));
        }
    }

    void WwtCommunicator::sendMessageToWwt(const ghoul::Dictionary& msg) {
        std::string script = "sendMessageToWWT(" + ghoul::formatJson(msg) + ");";
        executeJavascript(script);
    }

    const std::deque<int>& WwtCommunicator::getSelectedImages() {
        return _selectedImages;
    }

    void WwtCommunicator::setVerticalFov(float vfov) {
        _verticalFov = vfov;
    }

    void WwtCommunicator::setWebpageBorderColor(glm::ivec3 color) {
        std::string stringColor = std::to_string(color.x) + ","
            + std::to_string(color.y) + "," + std::to_string(color.z);
        std::string script = "document.body.style.backgroundColor = 'rgb("
            + stringColor + ")';";
        executeJavascript(script);
    }

    void WwtCommunicator::highlight(glm::ivec3 addition)
    {
        glm::ivec3 color = glm::ivec3(_borderColor.value());
        setWebpageBorderColor(color + addition);
    }

    void WwtCommunicator::removeHighlight(glm::ivec3 removal)
    {
        glm::ivec3 color = glm::ivec3(_borderColor.value());
        setWebpageBorderColor(color - removal);
    }

    glm::dvec2 WwtCommunicator::fieldsOfView() {
        float browserRatio = _dimensions.value().x / _dimensions.value().y;
        glm::dvec2 browserFov = glm::dvec2(verticalFov() * browserRatio, verticalFov());

        return browserFov;
    }

    bool WwtCommunicator::hasLoadedImages() const {
        return _hasLoadedImages;
    }

    void WwtCommunicator::setImageOrder(int i, int order) {
        // Find in selected images list
        auto current = std::find(
            std::begin(_selectedImages),
            std::end(_selectedImages),
            i
        );
        auto target = std::begin(_selectedImages) + order;

        // Make sure the image was found in the list
        if (current != std::end(_selectedImages) && target != std::end(_selectedImages)) {
            // Swap the two images
            std::iter_swap(current, target);
        }

        int reverseOrder = _selectedImages.size() - order - 1;
        ghoul::Dictionary message = wwtmessage::setLayerOrder(std::to_string(i),
            reverseOrder);
        sendMessageToWwt(message);
    }

    void WwtCommunicator::loadImageCollection(const std::string& collection) {
        sendMessageToWwt(wwtmessage::loadCollection(collection));
        _hasLoadedImages = true;
    }

    void WwtCommunicator::setImageOpacity(const int i, float opacity) {
        ghoul::Dictionary msg = wwtmessage::setImageOpacity(std::to_string(i), opacity);
        sendMessageToWwt(msg);
    }

    void WwtCommunicator::setHasLoadedImages(bool isLoaded) {
        _hasLoadedImages = isLoaded;
    }

    void WwtCommunicator::setIdInBrowser(const std::string& id) {
        // Send ID to it's browser
        executeJavascript("setId('" + id + "')");
    }

    void WwtCommunicator::executeJavascript(const std::string& script) const {
        // Make sure that the browser has a main frame
        const bool browserExists = _browserInstance && _browserInstance->getBrowser();
        const bool frameIsLoaded = browserExists &&
            _browserInstance->getBrowser()->GetMainFrame();

        if (frameIsLoaded) {
            CefRefPtr<CefFrame> frame = _browserInstance->getBrowser()->GetMainFrame();
            frame->ExecuteJavaScript(script, frame->GetURL(), 0);
        }
    }

    glm::ivec3 WwtCommunicator::borderColor() const {
        return _borderColor.value();
    }

    float WwtCommunicator::verticalFov() const {
        return _verticalFov.value();
    }

} // namespace openspace
