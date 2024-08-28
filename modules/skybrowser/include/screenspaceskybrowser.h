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

class ScreenSpaceSkyBrowser : public ScreenSpaceRenderable, public WwtCommunicator {
public:
    explicit ScreenSpaceSkyBrowser(const ghoul::Dictionary& dictionary);
    ~ScreenSpaceSkyBrowser() override;

    bool initializeGL() override;
    bool deinitializeGL() override;
    glm::mat4 scaleMatrix() override;
    void render(const RenderData& renderData) override;
    void update() override;

    float opacity() const noexcept override;
    glm::dvec2 fineTuneVector(const glm::dvec2& drag);
    bool isInitialized() const;
    bool isPointingSpacecraft() const;
    bool shouldUpdateWhileTargetAnimates() const;

    double setVerticalFovWithScroll(float scroll);
    void setIdInBrowser() const;
    void setIsInitialized(bool isInitialized);
    void setPointSpaceCraft(bool shouldPoint);

    void updateTextureResolution();

    // Copies rendered
    void addDisplayCopy(const glm::vec3& raePosition, int nCopies);
    void removeDisplayCopy();
    std::vector<std::pair<std::string, glm::dvec3>> displayCopies() const;
    std::vector<std::pair<std::string, bool>> showDisplayCopies() const;

    static documentation::Documentation Documentation();

private:
    static constexpr int RadiusTimeOut = 25;
    properties::FloatProperty _textureQuality;
    properties::BoolProperty _isHidden;
    properties::BoolProperty _isPointingSpacecraft;
    properties::BoolProperty _updateDuringTargetAnimation;
    std::vector<std::unique_ptr<properties::Vec3Property>> _displayCopies;
    std::vector<std::unique_ptr<properties::BoolProperty>> _showDisplayCopies;

    void bindTexture() override;

    // Flags
    bool _isInitialized = false;
    bool _radiusIsDirty = false;
    int _borderRadiusTimer = -1;

    float _lastTextureQuality = 1.f;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__
