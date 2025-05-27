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

#ifndef __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__
#define __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__

#include <modules/webbrowser/include/screenspacebrowser.h>
#include <modules/skybrowser/include/wwtcommunicator.h>

#include <openspace/documentation/documentation.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>

namespace openspace {

class ScreenSpaceSkyBrowser : public ScreenSpaceBrowser {
public:
    explicit ScreenSpaceSkyBrowser(const ghoul::Dictionary& dictionary);
    ~ScreenSpaceSkyBrowser() override;

    void render(const RenderData& renderData) override;
    void update() override;

    // Getters
    bool isInitialized() const;
    bool isPointingSpacecraft() const;
    bool shouldUpdateWhileTargetAnimates() const;
    double verticalFov() const;
    glm::ivec3 borderColor() const;
    double borderRadius() const;
    void setTargetRoll(double roll);
    glm::dvec2 fieldsOfView() const;
    glm::dvec2 equatorialAim() const;
    double browserRatio() const;

    // Setters
    void setIsInitialized(bool isInitialized);
    void setPointSpaceCraft(bool shouldPoint);
    void setVerticalFov(double vfov);
    void setEquatorialAim(glm::dvec2 equatorial);
    void setBorderColor(glm::ivec3 color);
    double setVerticalFovWithScroll(float scroll);
    void setBorderRadius(double radius);
    void setRatio(double ratio);
    void setIdInBrowser() const;

    glm::dvec2 fineTuneVector(const glm::dvec2& drag);
    void updateBorderColor();
    void updateTextureResolution();
    void reload();

    // Display copies rendered
    void addDisplayCopy(const glm::vec3& raePosition, int nCopies);
    void removeDisplayCopy();
    std::vector<std::pair<std::string, glm::dvec3>> displayCopies() const;
    std::vector<std::pair<std::string, bool>> showDisplayCopies() const;

    ghoul::Dictionary data() const;

    WwtCommunicator* worldWideTelescope();

    static documentation::Documentation Documentation();

private:
    static constexpr int RadiusTimeOut = 25;
    properties::BoolProperty _isHidden;
    properties::BoolProperty _isPointingSpacecraft;
    properties::BoolProperty _updateDuringTargetAnimation;
    std::vector<std::unique_ptr<properties::Vec3Property>> _displayCopies;
    std::vector<std::unique_ptr<properties::BoolProperty>> _showDisplayCopies;

    properties::DoubleProperty _verticalFov;

    double _borderRadius = 0.0;
    glm::ivec3 _wwtBorderColor = glm::ivec3(70);
    glm::dvec2 _equatorialAim = glm::dvec2(0.0);
    double _targetRoll = 0.0;

    WwtCommunicator _wwtCommunicator;
    // Time variables
    // For capping the message passing to WWT
    static constexpr std::chrono::milliseconds TimeUpdateInterval =
        std::chrono::milliseconds(10);
    std::chrono::system_clock::time_point _lastUpdateTime;

    bool _borderColorIsDirty = false;
    bool _equatorialAimIsDirty = false;
    bool _isDimensionsDirty = false;

    // Flags
    bool _isInitialized = false;
    bool _radiusIsDirty = false;
    int _borderRadiusTimer = -1;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__
