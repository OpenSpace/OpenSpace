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

#ifndef __OPENSPACE_MODULE_SKYBROWSER___RENDERABLESKYTARGET___H__
#define __OPENSPACE_MODULE_SKYBROWSER___RENDERABLESKYTARGET___H__

#include <modules/base/rendering/renderableplane.h>

#include <openspace/documentation/documentation.h>
#include <openspace/properties/scalar/floatproperty.h>

namespace openspace::documentation { struct Documentation; }

namespace openspace {

class ScreenSpaceSkyBrowser;

class RenderableSkyTarget : public RenderablePlane {
public:
    explicit RenderableSkyTarget(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void bindTexture() override;

    glm::ivec3 borderColor() const;

    void setRatio(float ratio);
    void setColor(glm::ivec3 color);
    void setVerticalFov(double fov);
    void setBorderRadius(double radius);

    // Display
    void highlight(const glm::ivec3& addition);
    void removeHighlight(const glm::ivec3& removal);

    static documentation::Documentation Documentation();

    void applyRoll();
    glm::dvec3 rightVector() const;
    glm::dvec3 upVector() const;

private:
    properties::FloatProperty _crossHairSize;
    properties::FloatProperty _showRectangleThreshold;
    properties::FloatProperty _lineWidth;
    properties::DoubleProperty _verticalFov;
    properties::BoolProperty _applyRoll;

    bool _isInitialized = false;
    double _borderRadius = 0.0;
    glm::ivec3 _borderColor = glm::ivec3(230);
    float _ratio = 1.f;
    glm::dvec3 _rightVector;
    glm::dvec3 _upVector;
    glm::dvec3 _worldPosition;
};
} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___RENDERABLESKYTARGET___H__
