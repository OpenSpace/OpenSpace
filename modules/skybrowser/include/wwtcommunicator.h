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

#ifndef __OPENSPACE_MODULE_SKYBROWSER___WWTCOMMUNICATOR___H__
#define __OPENSPACE_MODULE_SKYBROWSER___WWTCOMMUNICATOR___H__

#include <modules/skybrowser/include/browser.h>

#include <openspace/documentation/documentation.h>
#include <openspace/properties/vector/dvec2property.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/ivec3property.h>
#include <deque>

namespace openspace {

class WwtCommunicator : public Browser {
public:
    explicit WwtCommunicator(const ghoul::Dictionary& dictionary);
    WwtCommunicator(const WwtCommunicator&) = default;
    ~WwtCommunicator();

    void update();

    // WorldWide Telescope communication
    void displayImage(const std::string& url, int i);
    void removeSelectedImage(int i);
    void setImageOrder(int i, int order);
    void loadImageCollection(const std::string& collection);
    void setImageOpacity(int i, float opacity) const;
    void hideChromeInterface(bool shouldHide) const;

    bool hasLoadedImages() const;
    double verticalFov() const;
    glm::ivec3 borderColor() const;
    glm::dvec2 equatorialAim() const;
    glm::dvec2 fieldsOfView() const;
    const std::deque<int>& getSelectedImages() const;

    void setHasLoadedImages(bool isLoaded);
    void setVerticalFov(double vfov);
    void setIsSyncedWithWwt(bool isSynced);
    void setEquatorialAim(glm::dvec2 equatorial);
    void setBorderColor(glm::ivec3 color);
    void setTargetRoll(double roll);

    void highlight(const glm::ivec3& addition) const;
    // The removal parameter decides what will be removed from the border color
    void removeHighlight(const glm::ivec3& removal) const;
    void updateBorderColor() const;
    void updateAim() const;

protected:
    void setIdInBrowser(const std::string& id) const;

    double _verticalFov = 10.0f;
    glm::ivec3 _borderColor = glm::ivec3(70);
    glm::dvec2 _equatorialAim = glm::dvec2(0.0);
    double _targetRoll = 0.0;
    bool _hasLoadedImages = false;
    std::deque<int> _selectedImages;

private:
    void setWebpageBorderColor(glm::ivec3 color) const;
    void sendMessageToWwt(const ghoul::Dictionary& msg) const;

    // WorldWide Telescope messages
    ghoul::Dictionary moveCameraMessage(const glm::dvec2& celestCoords, double fov,
        double roll, bool shouldMoveInstantly = true) const;
    ghoul::Dictionary loadCollectionMessage(const std::string& url) const;
    ghoul::Dictionary setForegroundMessage(const std::string& name) const;
    ghoul::Dictionary addImageMessage(const std::string& id,
        const std::string& url) const;
    ghoul::Dictionary removeImageMessage(const std::string& id) const;
    ghoul::Dictionary setImageOpacityMessage(const std::string& id, double opacity) const;
    ghoul::Dictionary setLayerOrderMessage(const std::string& id, int version);

    bool _isSyncedWithWwt = false;
    bool _borderColorIsDirty = false;
    bool _equatorialAimIsDirty = false;
    int messageCounter = 0;

    // Time variables
    // For capping the message passing to WWT
    constexpr static const std::chrono::milliseconds TimeUpdateInterval =
        std::chrono::milliseconds(10);
    std::chrono::system_clock::time_point _lastUpdateTime;
};
} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___WWTCOMMUNICATOR___H__
