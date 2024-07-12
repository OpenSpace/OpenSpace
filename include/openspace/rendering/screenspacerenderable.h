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

#ifndef __OPENSPACE_CORE___SCREENSPACERENDERABLE___H__
#define __OPENSPACE_CORE___SCREENSPACERENDERABLE___H__

#include <openspace/properties/propertyowner.h>
#include <openspace/rendering/fadeable.h>

#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/vector/vec4property.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <memory>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

namespace documentation { struct Documentation; }

/**
 * The base class for screen space images and screen space framebuffers. This base class
 * handles general functionality specific to planes that are rendered in front of the
 * camera. It implements protected methods and properties for converting the planes from
 * Spherical to Cartesian coordinates and back. It also specifies the interface that its
 * children need to implement.
 */
class ScreenSpaceRenderable : public properties::PropertyOwner, public Fadeable {
public:
    static std::unique_ptr<ScreenSpaceRenderable> createFromDictionary(
        const ghoul::Dictionary& dictionary);

    static constexpr std::string_view KeyName = "Name";
    static constexpr std::string_view KeyIdentifier = "Identifier";

    ScreenSpaceRenderable(const ghoul::Dictionary& dictionary);
    virtual ~ScreenSpaceRenderable() override;

    struct RenderData {
        float blackoutFactor;
        float hue;
        float value;
        float saturation;
        float gamma;
    };
    virtual void render(const RenderData& renderData);

    virtual bool initialize();
    virtual bool initializeGL();
    virtual bool deinitialize();
    virtual bool deinitializeGL();

    virtual void update();
    virtual bool isReady() const;
    bool isEnabled() const;
    bool isUsingRaeCoords() const;
    bool isFacingCamera() const;
    void setEnabled(bool isEnabled);
    float depth();
    float scale() const;

    // Screen space functionality in these coords: [-1,1][-ratio,ratio]
    glm::vec2 screenSpacePosition();
    glm::vec2 screenSpaceDimensions();
    glm::vec2 upperRightCornerScreenSpace();
    glm::vec2 lowerLeftCornerScreenSpace();
    bool isIntersecting(const glm::vec2& coord);
    void translate(glm::vec2 translation, glm::vec2 position);
    void setCartesianPosition(const glm::vec3& position);
    void setRaeFromCartesianPosition(const glm::vec3& position);
    glm::vec3 raePosition() const;

    static documentation::Documentation Documentation();

protected:
    void createShaders();
    std::string makeUniqueIdentifier(std::string name);

    virtual glm::mat4 scaleMatrix();
    glm::mat4 globalRotationMatrix();
    glm::mat4 translationMatrix();
    glm::mat4 localRotationMatrix();

    glm::vec3 raeToCartesian(const glm::vec3& rae) const;
    glm::vec3 cartesianToRae(const glm::vec3& cartesian) const;

    void draw(const glm::mat4& modelTransform, const RenderData& renderData);

    virtual void bindTexture() = 0;
    virtual void unbindTexture();

    glm::vec3 sphericalToRae(const glm::vec3& spherical) const;
    glm::vec3 raeToSpherical(const glm::vec3& rae) const;
    glm::vec3 cartesianToSpherical(const glm::vec3& cartesian) const;
    glm::vec3 sphericalToCartesian(glm::vec3 spherical) const;
    glm::vec3 sanitizeSphericalCoordinates(glm::vec3 spherical) const;

    properties::BoolProperty _enabled;
    properties::BoolProperty _renderDuringBlackout;
    properties::BoolProperty _usePerspectiveProjection;
    properties::BoolProperty _useRadiusAzimuthElevation;
    properties::BoolProperty _faceCamera;

    // x, y, z
    properties::Vec3Property _cartesianPosition;

    // Radius, azimuth, elevation,
    // where azimuth is relative to negative y axis and
    // elevation is angle from plane with normal z.
    properties::Vec3Property _raePosition;

    // Local rotation (roll, pitch, yaw)
    properties::Vec3Property _localRotation;

    // Border
    properties::FloatProperty _borderWidth;
    properties::Vec3Property _borderColor;

    properties::FloatProperty _scale;
    properties::FloatProperty _gammaOffset;
    properties::Vec3Property _multiplyColor;
    properties::Vec4Property _backgroundColor;
    properties::TriggerProperty _delete;

    glm::ivec2 _objectSize = glm::ivec2(0);
    UniformCache(color, opacity, blackoutFactor, hue, value, saturation, mvpMatrix, tex,
        backgroundColor, gamma, borderColor, borderWidth) _uniformCache;
    std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___SCREENSPACERENDERABLE___H__
