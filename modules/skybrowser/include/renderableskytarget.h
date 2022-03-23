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

#ifndef __OPENSPACE_MODULE_SKYBROWSER___RENDERABLESKYTARGET___H__
#define __OPENSPACE_MODULE_SKYBROWSER___RENDERABLESKYTARGET___H__

#include <modules/base/rendering/renderableplane.h>

#include <openspace/documentation/documentation.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>

namespace openspace::documentation { struct Documentation; }

namespace openspace {

class ScreenSpaceSkyBrowser;

class RenderableSkyTarget : public RenderablePlane {
public:
    constexpr static const float DeltaTimeThreshold = 0.03f; 

    explicit RenderableSkyTarget(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;
    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;
    void bindTexture() override; // Empty function but has to be defined

    glm::ivec3 borderColor() const;
    float opacity() const;
    double animationSpeed() const;
    double stopAnimationThreshold() const;

    void setDimensions(glm::vec2 dimensions);
    void setColor(glm::ivec3 color);
    void setOpacity(float opacity);
    void setVerticalFov(double fov);

    // Display
    void highlight(const glm::ivec3& addition);
    void removeHighlight(const glm::ivec3& removal);

private:
    // Properties
    properties::FloatProperty _crossHairSize;
    properties::FloatProperty _showRectangleThreshold;
    properties::FloatProperty _lineWidth;
    properties::DoubleProperty _stopAnimationThreshold;
    properties::DoubleProperty _animationSpeed;

    double _verticalFov = 10.0;

    glm::ivec3 _borderColor = glm::ivec3(230);
    glm::vec2 _dimensions = glm::vec2(1.f);
};
} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___RENDERABLESKYTARGET___H__
