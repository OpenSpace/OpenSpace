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

#ifndef __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYTARGET___H__
#define __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYTARGET___H__

#include <openspace/rendering/screenspacerenderable.h>

#include <openspace/documentation/documentation.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>

namespace openspace::documentation { struct Documentation; }

namespace openspace {

class ScreenSpaceSkyBrowser;

class ScreenSpaceSkyTarget : public ScreenSpaceRenderable {
    
public:
    constexpr static const float DeltaTimeThreshold = 0.03f; 

    explicit ScreenSpaceSkyTarget(const ghoul::Dictionary& dictionary);
    virtual ~ScreenSpaceSkyTarget();

    bool initializeGL() override;
    bool deinitializeGL() override;
    bool isReady() const override;
    void render() override;
    void update() override;
    glm::mat4 scaleMatrix() override;
    void bindTexture() override; // Empty function but has to be defined
    void createShaders();

    glm::ivec3 borderColor() const;
    float opacity() const;
    glm::dvec2 lockedCoordinates() const;

    void setScaleFromVfov(float verticalFov);
    void setFovFromScale();
    void setDimensions(glm::vec2 dimensions);
    void setColor(glm::ivec3 color);
    void setOpacity(float opacity);
    void setLock(bool isLocked);
    void setEquatorialAim(const glm::dvec2& aim);

    // Target directions
    glm::dvec3 directionGalactic() const;
    glm::dvec2 equatorialAim() const;

    // Locking functionality
    bool isLocked() const;

    // Animation
    bool isAnimated();
    void startAnimation(glm::dvec3 end, bool shouldLockAfter = true);
    void incrementallyAnimateToCoordinate(float deltaTime);
    // Display
    void highlight(glm::ivec3 addition);
    void removeHighlight(glm::ivec3 removal);

private:
    // Properties
    properties::FloatProperty _showCrosshairThreshold;
    properties::FloatProperty _showRectangleThreshold;
    properties::FloatProperty _lineWidth;
    properties::DoubleProperty _stopAnimationThreshold;
    properties::DoubleProperty _animationSpeed;

    // Flags
    bool _isLocked = false;
    bool _isAnimated = false;
    bool _shouldLockAfterAnimation = false;

    // Shader
    UniformCache(modelTransform, viewProj, showCrosshair, showRectangle, lineWidth, 
        dimensions, lineColor) _uniformCache;
    GLuint _vertexArray = 0;
    GLuint _vertexBuffer = 0;
        
    // Sky browser
    float _verticalFov = 70.0f;
    glm::dvec2 _equatorialAim = glm::dvec2(0.0);
    glm::ivec3 _borderColor = glm::ivec3(255);

    // Lock target to a coordinate on the sky
    glm::dvec3 _lockedCoordinates;   // Cartesian equatorial coordinates

    // Animation
    glm::dvec3 _animationEnd;        // Cartesian equatorial coordinates
    glm::dvec3 _animationStart;      // Cartesian equatorial coordinates
};
}

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYTARGET___H__

