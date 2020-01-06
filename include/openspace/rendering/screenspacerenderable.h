/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <memory>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

namespace documentation { struct Documentation; }

/**
 * The base class for screen space images and screen space framebuffers.
 * This base class handles general functionality specific to planes that are rendered in
 * front of the camera. It implements protected methods and properties for converting
 * the planes from Spherical to Cartesian coordinates and back. It also specifies the
 * interface that its children need to implement.
 */
class ScreenSpaceRenderable : public properties::PropertyOwner {
public:
    static std::unique_ptr<ScreenSpaceRenderable> createFromDictionary(
        const ghoul::Dictionary& dictionary);

    static constexpr const char* KeyName = "Name";
    static constexpr const char* KeyIdentifier = "Identifier";

    ScreenSpaceRenderable(const ghoul::Dictionary& dictionary);
    virtual ~ScreenSpaceRenderable();

    virtual void render();

    virtual bool initialize();
    virtual bool initializeGL();
    virtual bool deinitialize();
    virtual bool deinitializeGL();

    virtual void update();
    virtual bool isReady() const;
    bool isEnabled() const;
    float depth();

    static documentation::Documentation Documentation();

protected:
    void createShaders();
    std::string makeUniqueIdentifier(std::string name);

    glm::mat4 scaleMatrix();
    glm::mat4 globalRotationMatrix();
    glm::mat4 translationMatrix();
    glm::mat4 localRotationMatrix();

    void draw(glm::mat4 modelTransform);

    virtual void bindTexture() = 0;
    virtual void unbindTexture();

    properties::BoolProperty _enabled;
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

    properties::FloatProperty _scale;
    properties::FloatProperty _alpha;
    properties::TriggerProperty _delete;

    glm::ivec2 _objectSize;
    UniformCache(alpha, modelTransform, viewProj, texture) _uniformCache;
    std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___SCREENSPACERENDERABLE___H__
