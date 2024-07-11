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

    double verticalFov() const;
    glm::ivec3 borderColor() const;
    glm::dvec2 equatorialAim() const;
    glm::dvec2 fieldsOfView() const;
    float browserRatio() const;
    std::vector<std::string> selectedImages() const;
    std::vector<double> opacities() const;
    double borderRadius() const;

    void selectImage(const std::string& imageUrl);
    void setVerticalFov(double vfov);
    void setEquatorialAim(glm::dvec2 equatorial);
    void setImageOpacity(const std::string& imageUrl, float opacity);
    void setTargetRoll(double roll);
    void setBorderColor(glm::ivec3 color);
    void removeSelectedImage(const std::string& imageUrl);

    void hideChromeInterface();
    void addImageLayerToWwt(std::string imageUrl);
    void reload();
    void setBorderRadius(double radius);
    void setRatio(float ratio);

    bool isImageCollectionLoaded() const;

    void setImageCollectionIsLoaded(bool isLoaded);
    void setImageOrder(const std::string& imageUrl, int order);
    void loadImageCollection(const std::string& collection);
    glm::dvec2 fineTuneVector(const glm::dvec2& drag);
    bool isInitialized() const;
    bool isPointingSpacecraft() const;
    bool shouldUpdateWhileTargetAnimates() const;

    double setVerticalFovWithScroll(float scroll);
    void setIdInBrowser() const;
    void setIsInitialized(bool isInitialized);
    void setPointSpaceCraft(bool shouldPoint);

    void updateTextureResolution();
    void updateBorderColor();

    // Copies rendered
    void addDisplayCopy(const glm::vec3& raePosition, int nCopies);
    void removeDisplayCopy();
    std::vector<std::pair<std::string, glm::dvec3>> displayCopies() const;
    std::vector<std::pair<std::string, bool>> showDisplayCopies() const;

    static documentation::Documentation Documentation();

private:
    SelectedImageDeque::iterator findSelectedImage(const std::string& imageUrl);

    static constexpr int RadiusTimeOut = 25;
    properties::FloatProperty _textureQuality;
    properties::BoolProperty _isHidden;
    properties::BoolProperty _isPointingSpacecraft;
    properties::BoolProperty _updateDuringTargetAnimation;
    properties::DoubleProperty _verticalFov;
    std::vector<std::unique_ptr<properties::Vec3Property>> _displayCopies;
    std::vector<std::unique_ptr<properties::BoolProperty>> _showDisplayCopies;

    bool _borderColorIsDirty = false;
    bool _equatorialAimIsDirty = false;
    bool _isImageCollectionLoaded = false;
    double _borderRadius = 0.0;
    glm::ivec3 _wwtBorderColor = glm::ivec3(70);
    glm::dvec2 _equatorialAim = glm::dvec2(0.0);
    double _targetRoll = 0.0;
    SelectedImageDeque _selectedImages;

    void bindTexture() override;

    // Flags
    bool _isInitialized = false;
    bool _radiusIsDirty = false;
    int _borderRadiusTimer = -1;

    float _lastTextureQuality = 1.f;

    WwtCommunicator _wwtCommunicator;

    // Time variables
    // For capping the message passing to WWT
    static constexpr std::chrono::milliseconds TimeUpdateInterval =
        std::chrono::milliseconds(10);
    std::chrono::system_clock::time_point _lastUpdateTime;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__
