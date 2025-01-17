/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
#include <openspace/properties/scalar/doubleproperty.h>

#include <deque>

namespace openspace {
using SelectedImageDeque = std::deque<std::pair<std::string, double>>;

class WwtCommunicator : public Browser {
public:
    explicit WwtCommunicator(const ghoul::Dictionary& dictionary);
    ~WwtCommunicator() override = default;

    void update();

    // WorldWide Telescope communication
    void selectImage(const std::string& imageUrl);
    void addImageLayerToWwt(const std::string& imageUrl);
    void removeSelectedImage(const std::string& imageUrl);
    void setImageOrder(const std::string& imageUrl, int order);
    void loadImageCollection(const std::string& collection);
    void setImageOpacity(const std::string& imageUrl, float opacity);
    void hideChromeInterface() const;

    bool isImageCollectionLoaded() const;

    double verticalFov() const;
    glm::ivec3 borderColor() const;
    glm::dvec2 equatorialAim() const;
    glm::dvec2 fieldsOfView() const;
    std::vector<std::string> selectedImages() const;
    std::vector<double> opacities() const;
    double borderRadius() const;

    void setImageCollectionIsLoaded(bool isLoaded);
    void setVerticalFov(double vfov);
    void setEquatorialAim(glm::dvec2 equatorial);
    void setBorderColor(glm::ivec3 color);
    void setBorderRadius(double radius);
    void setTargetRoll(double roll);

    void updateBorderColor() const;
    void updateAim() const;

protected:
    void setIdInBrowser(const std::string& id) const;
    SelectedImageDeque::iterator findSelectedImage(const std::string& imageUrl);

    properties::DoubleProperty _verticalFov;

    double _borderRadius = 0.0;
    glm::ivec3 _wwtBorderColor = glm::ivec3(70);
    glm::dvec2 _equatorialAim = glm::dvec2(0.0);
    double _targetRoll = 0.0;
    bool _isImageCollectionLoaded = false;
    SelectedImageDeque _selectedImages;

private:
    void sendMessageToWwt(const ghoul::Dictionary& msg) const;

    bool _borderColorIsDirty = false;
    bool _equatorialAimIsDirty = false;

    // Time variables
    // For capping the message passing to WWT
    static constexpr std::chrono::milliseconds TimeUpdateInterval =
        std::chrono::milliseconds(10);
    std::chrono::system_clock::time_point _lastUpdateTime;
};
} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___WWTCOMMUNICATOR___H__
