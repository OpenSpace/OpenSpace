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

#ifndef __OPENSPACE_MODULE_SKYBROWSER___WWTCOMMUNICATOR___H__
#define __OPENSPACE_MODULE_SKYBROWSER___WWTCOMMUNICATOR___H__

#include <modules/skybrowser/include/browser.h>
#include <openspace/properties/vector/ivec3property.h>
#include <openspace/properties/vector/dvec2property.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/documentation/documentation.h>
#include <deque>
#include <thread>

namespace openspace {

class ImageData;

class WwtCommunicator : public Browser {

public:

    WwtCommunicator(const ghoul::Dictionary& dictionary);
    WwtCommunicator(WwtCommunicator const&) = default;
    virtual ~WwtCommunicator();

    // WorldWide Telescope communication
    void displayImage(const std::string& url, int i);
    void removeSelectedImage(const int i);
    void setImageOrder(int i, int order);
    void loadImageCollection(const std::string& collection);
    void setImageOpacity(int i, float opacity);
    void update();

    // Getters
    const std::deque<int>& getSelectedImages();
    glm::ivec3 borderColor() const;
    float verticalFov() const;
    glm::dvec2 fieldsOfView();
    bool hasLoadedImages() const;
    glm::dvec3 equatorialAim() const;

    // Setters
    void setHasLoadedImages(bool isLoaded);
    void setVerticalFov(float vfov);
    void setIsSyncedWithWwt(bool isSynced);
    void setEquatorialAim(glm::dvec3 cartesian);

    // Display
    void highlight(glm::ivec3 addition);
    void removeHighlight(glm::ivec3 removal);
    void updateBorderColor();
    

protected:
    // Web page communication
    void setIdInBrowser(const std::string& id);

    properties::DVec2Property _equatorialAim;
    properties::FloatProperty _verticalFov;
    properties::IVec3Property _borderColor;
    

    std::deque<int> _selectedImages;
    bool _hasLoadedImages{ false };

private:
    bool _isSyncedWithWwt{ false };
    const std::chrono::microseconds interval = std::chrono::microseconds(10000);
    std::chrono::time_point<std::chrono::high_resolution_clock> latestCall;

    void setWebpageBorderColor(glm::ivec3 color);
    void sendMessageToWwt(const ghoul::Dictionary& msg);

    int messageCounter{ 0 };

    ghoul::Dictionary moveCamera(const glm::dvec2& celestCoords, const double fov,
        const double roll, const bool shouldMoveInstantly = true);
    ghoul::Dictionary loadCollection(const std::string& url);
    ghoul::Dictionary setForeground(const std::string& name);
    ghoul::Dictionary addImage(const std::string& id, const std::string& url);
    ghoul::Dictionary removeImage(const std::string& id);
    ghoul::Dictionary setImageOpacity(const std::string& id, double opacity);
    ghoul::Dictionary setForegroundOpacity(double val);
    ghoul::Dictionary setLayerOrder(const std::string& id, int version);

};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___WWTCOMMUNICATOR___H__
