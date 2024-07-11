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

    ghoul::Dictionary hideChromeInterfaceMessage() {
        using namespace std::string_literals;

        ghoul::Dictionary msg;
        msg.setValue("event", "modify_settings"s);
        msg.setValue("settings", "[[\"hideAllChrome\", true]]"s);
        msg.setValue("target", "app"s);
        return msg;
    }
} // namespace

namespace openspace {

WwtCommunicator::WwtCommunicator(const ghoul::Dictionary& dictionary)
    : Browser(dictionary)
{}

void WwtCommunicator::addImageLayerToWwt(const std::string& imageUrl) {
    // Index of image is used as layer ID as it is unique in the image data set
    sendMessageToWwt(addImageMessage(imageUrl));
    sendMessageToWwt(setImageOpacityMessage(imageUrl, 1.0));
}

void WwtCommunicator::removeSelectedImage(const std::string& imageUrl) {
    sendMessageToWwt(removeImageMessage(imageUrl));
}

void WwtCommunicator::sendMessageToWwt(const ghoul::Dictionary& msg) const {
    std::string m = ghoul::formatJson(msg);
    executeJavascript(std::format("sendMessageToWWT({});", m));
}

void WwtCommunicator::setBorderRadius(double radius) {
    const std::string scr = std::format("setBorderRadius({});", radius);
    executeJavascript(scr);
}

void WwtCommunicator::setBorderColor(glm::ivec3 borderColor) {
    const std::string script = std::format(
        "setBackgroundColor('rgb({},{},{})');",
        borderColor.x, borderColor.y, borderColor.z
    );
    executeJavascript(script);
}

void WwtCommunicator::setAim(glm::dvec2 aim, double vfov, double roll) {
    // Message WorldWide Telescope current view
    const ghoul::Dictionary msg = moveCameraMessage(
        aim,
        vfov,
        roll
    );
    sendMessageToWwt(msg);
}

void WwtCommunicator::setImageOrder(const std::string& imageUrl, int reverseOrder) {
    const ghoul::Dictionary message = setLayerOrderMessage(imageUrl, reverseOrder);
    sendMessageToWwt(message);
}

void WwtCommunicator::loadImageCollection(const std::string& collection) {
    sendMessageToWwt(loadCollectionMessage(collection));
}

void WwtCommunicator::setImageOpacity(const std::string& imageUrl, float opacity) {
    const ghoul::Dictionary msg = setImageOpacityMessage(imageUrl, opacity);
    sendMessageToWwt(msg);
}

void WwtCommunicator::hideChromeInterface() const {
     const std::string script = "sendMessageToWWT({event : \"modify_settings\", "
         "settings : [[\"hideAllChrome\", true]], target: \"app\"});";
    executeJavascript(script);
    // TODO ylvse 2024-07-10 see if i can get this to work
    //sendMessageToWwt(hideChromeInterfaceMessage());
}

void WwtCommunicator::setIdInBrowser(const std::string& id) const {
    // Send ID to its browser
    executeJavascript(std::format("setId('{}')", id));
}

} // namespace openspace
