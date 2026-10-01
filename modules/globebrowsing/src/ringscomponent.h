/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___RINGSCOMPONENT___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___RINGSCOMPONENT___H__

#include <openspace/properties/propertyowner.h>
#include <openspace/rendering/fadeable.h>

#include <modules/globebrowsing/src/shadowcomponent.h>
#include <openspace/filesystem/file.h>
#include <openspace/glm.h>
#include <openspace/misc/dictionary.h>
#include <openspace/opengl/gl.h>
#include <openspace/opengl/texture.h>
#include <openspace/opengl/uniformcache.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <functional>

namespace openspace {

namespace opengl { class ProgramObject; }
struct Documentation;
struct RenderData;
struct UpdateData;

class RingsComponent : public PropertyOwner, public Fadeable {
public:
    // Callback for when readiness state changes
    using ReadinessChangeCallback = std::function<void()>;

    explicit RingsComponent(const Dictionary& dictionary);

    void initialize();
    void initializeGL();
    void deinitializeGL();

    bool isReady() const;

    void draw(const RenderData& data,
        const ShadowComponent::ShadowMapData& shadowData = {}
    );
    void update(const UpdateData& data);
    bool isEnabled() const;

    static openspace::Documentation Documentation();
    double size() const;

    // Readiness change callback
    void onReadinessChange(ReadinessChangeCallback callback);

    // Texture access methods for globe rendering
    opengl::Texture* textureForwards() const;
    opengl::Texture* textureBackwards() const;
    opengl::Texture* textureUnlit() const;
    opengl::Texture* textureColor() const;
    opengl::Texture* textureTransparency() const;
    glm::vec2 textureOffset() const;
    glm::vec3 sunPositionObj() const;
    glm::vec3 camPositionObj() const;

    void setEllipsoidRadii(glm::vec3 radii);

private:
    void loadTexture();
    void compileShadowShader();
    void checkAndNotifyReadinessChange();

    StringProperty _texturePath;
    StringProperty _textureFwrdPath;
    StringProperty _textureBckwrdPath;
    StringProperty _textureUnlitPath;
    StringProperty _textureColorPath;
    StringProperty _textureTransparencyPath;
    FloatProperty _size;
    Vec2Property _offset;
    FloatProperty _nightFactor;
    FloatProperty _colorFilter;
    BoolProperty _enabled;
    FloatProperty _zFightingPercentage;
    IntProperty _nShadowSamples;

    std::unique_ptr<opengl::ProgramObject> _shader;
    std::unique_ptr<opengl::ProgramObject> _geometryOnlyShader;
    UniformCache(modelViewProjectionMatrix, textureOffset, colorFilterValue, nightFactor,
        sunPosition, sunPositionObj, ringTexture,
        opacity, ellipsoidRadii
    ) _uniformCache;
    UniformCache(modelViewProjectionMatrix, textureOffset, colorFilterValue, nightFactor,
        sunPosition, sunPositionObj, camPositionObj, textureForwards, camPositionObjRaw, textureBackwards,
        textureUnlit, textureColor, textureTransparency,
        opacity, ellipsoidRadii
    ) _uniformCacheAdvancedRings;
    UniformCache(modelViewProjectionMatrix, textureOffset, ringTexture) _geomUniformCache;

    std::unique_ptr<opengl::Texture> _texture;
    std::unique_ptr<opengl::Texture> _textureForwards;
    std::unique_ptr<opengl::Texture> _textureBackwards;
    std::unique_ptr<opengl::Texture> _textureUnlit;
    std::unique_ptr<opengl::Texture> _textureTransparency;
    std::unique_ptr<opengl::Texture> _textureColor;
    std::unique_ptr<filesystem::File> _textureFile;
    std::unique_ptr<filesystem::File> _textureFileForwards;
    std::unique_ptr<filesystem::File> _textureFileBackwards;
    std::unique_ptr<filesystem::File> _textureFileUnlit;
    std::unique_ptr<filesystem::File> _textureFileColor;
    std::unique_ptr<filesystem::File> _textureFileTransparency;

    Dictionary _ringsDictionary;
    bool _textureIsDirty = false;
    bool _isAdvancedTextureEnabled = false;
    GLuint _vao = 0;
    GLuint _vbo = 0;
    bool _planeIsDirty = true;

    glm::vec3 _sunPosition = glm::vec3(0.f);
    glm::vec3 _camPositionObjectSpace = glm::vec3(0.f);
    glm::vec3 _camPositionObjectSpaceRaw = glm::vec3(0.f);
    glm::vec3 _ellipsoidRadii = glm::vec3(1.f);

    // Callback for readiness state changes
    ReadinessChangeCallback _readinessChangeCallback;
    bool _wasReady = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___RINGSCOMPONENT___H__
