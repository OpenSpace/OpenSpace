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

#ifndef __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__
#define __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__

#include <openspace/rendering/screenspacerenderable.h>
#include <modules/skybrowser/include/wwtcommunicator.h>

#include <openspace/documentation/documentation.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/vector/dvec2property.h>
#include <openspace/properties/list/stringlistproperty.h>
#include <openspace/properties/list/doublelistproperty.h>

namespace openspace {
using SelectedImageDeque = std::deque<std::pair<std::string, double>>;

class ScreenSpaceSkyBrowser : public ScreenSpaceRenderable {

public:
    explicit ScreenSpaceSkyBrowser(const ghoul::Dictionary& dictionary);
    ~ScreenSpaceSkyBrowser() override;

    bool initializeGL() override;
    bool deinitializeGL() override;
    glm::mat4 scaleMatrix() override;
    void render(const RenderData& renderData) override;
    void update() override;
    float opacity() const noexcept override;

    void setAsPaired();

    // Images in WorldWide Telescope
    void selectImage(const std::string& imageUrl);
    void removeSelectedImage(const std::string& imageUrl);
    void setImageOpacity(const std::string& imageUrl, float opacity);
    void addImageLayerToWwt(std::string imageUrl);
    void setImageOrder(const std::string& imageUrl, int order);
    std::vector<std::string> selectedImages() const;
    std::vector<double> opacities() const;

    // Aim and browser ratio
    void setVerticalFov(double vfov);
    double verticalFov() const;
    void setEquatorialAim(glm::dvec2 equatorial);
    glm::dvec2 equatorialAim() const;
    void setTargetRoll(double roll);
    void setRatio(float ratio);
    float browserRatio() const;
    glm::dvec2 fieldsOfView() const;

    // Look of browser window
    glm::ivec3 borderColor() const;
    double borderRadius() const;
    void hideChromeInterface();
    void setBorderColor(glm::ivec3 color);
    void setBorderRadius(double radius);
    double setVerticalFovWithScroll(float scroll);

    void reload();
    glm::dvec2 fineTuneVector(const glm::dvec2& drag);

    // Initialization process
    void setIdInBrowser() const;
    void loadImageCollection(const std::string& collection);
    bool isImageCollectionLoaded() const;
    void setImageCollectionIsLoaded(bool isLoaded);
    bool isInitialized() const;
    void setIsInitialized(bool isInitialized);

    // Display copies
    void addDisplayCopy(const glm::vec3& raePosition, int nCopies);
    void removeDisplayCopy();
    std::vector<std::pair<std::string, glm::dvec3>> displayCopies() const;
    std::vector<std::pair<std::string, bool>> showDisplayCopies() const;

    static documentation::Documentation Documentation();

private:
    void updateTextureResolution();

    SelectedImageDeque::iterator findSelectedImage(const std::string& imageUrl);
    SelectedImageDeque _selectedImages;

    properties::StringListProperty _selectedImagesUrls;
    properties::DoubleListProperty _selectedImagesOpacities;
    properties::DoubleProperty _borderRadius;
    properties::DoubleProperty _roll;
    properties::DVec2Property _equatorialAim;

    properties::FloatProperty _textureQuality;
    properties::BoolProperty _isHidden;
    properties::DoubleProperty _verticalFov;
    std::vector<std::unique_ptr<properties::Vec3Property>> _displayCopies;
    std::vector<std::unique_ptr<properties::BoolProperty>> _showDisplayCopies;

    int _borderRadiusTimer = -1;
    static constexpr int RadiusTimeOut = 25;
    float _lastTextureQuality = 1.f;

    // Flags
    bool _borderColorIsDirty = false;
    bool _equatorialAimIsDirty = false;
    bool _radiusIsDirty = false;
    bool _isImageCollectionLoaded = false;
    bool _isInitialized = false;

    void bindTexture() override;

    WwtCommunicator _wwtCommunicator;

    // Time variables
    // For capping the message passing to WWT
    static constexpr std::chrono::milliseconds TimeUpdateInterval =
        std::chrono::milliseconds(10);
    std::chrono::system_clock::time_point _lastUpdateTime;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__
