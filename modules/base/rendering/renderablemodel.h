/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLEMODEL___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLEMODEL___H__

#include <openspace/rendering/renderable.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/matrix/dmat4property.h>
#include <openspace/properties/matrix/mat3property.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/util/distanceconversion.h>
#include <ghoul/misc/managedmemoryuniqueptr.h>
#include <ghoul/io/model/modelreader.h>
#include <ghoul/opengl/uniformcache.h>
#include <memory>

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace ghoul::modelgeometry { class ModelGeometry; }

namespace openspace {

struct RenderData;
struct UpdateData;
class LightSource;

namespace documentation { struct Documentation; }

class RenderableModel : public Renderable {
public:
    RenderableModel(const ghoul::Dictionary& dictionary);
    ~RenderableModel() override = default;

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    enum class AnimationMode {
        Once = 0,
        LoopFromStart,
        LoopInfinitely,
        BounceFromStart,
        BounceInfinitely
    };

    std::unique_ptr<ghoul::modelgeometry::ModelGeometry> _geometry;
    double _modelScale = 1.0;
    bool _invertModelScale = false;
    bool _forceRenderInvisible = false;
    bool _notifyInvisibleDropped = true;
    std::string _animationStart;
    AnimationMode _animationMode = AnimationMode::Once;
    properties::BoolProperty _enableAnimation;

    properties::FloatProperty _ambientIntensity;
    properties::FloatProperty _diffuseIntensity;
    properties::FloatProperty _specularIntensity;

    properties::BoolProperty _performShading;
    properties::BoolProperty _enableFaceCulling;
    properties::DMat4Property _modelTransform;
    properties::Vec3Property _rotationVec;

    properties::BoolProperty _enableDepthTest;
    properties::OptionProperty _blendingFuncOption;

    std::string _vertexShaderPath;
    std::string _fragmentShaderPath;
    ghoul::opengl::ProgramObject* _program = nullptr;
    UniformCache(nLightSources, lightDirectionsViewSpace, lightIntensities,
        modelViewTransform, normalTransform, projectionTransform,
        performShading, ambientIntensity, diffuseIntensity,
        specularIntensity, performManualDepthTest, gBufferDepthTexture) _uniformCache;

    std::vector<std::unique_ptr<LightSource>> _lightSources;

    // Buffers for uniform uploading
    std::vector<float> _lightIntensitiesBuffer;
    std::vector<glm::vec3> _lightDirectionsViewSpaceBuffer;

    properties::PropertyOwner _lightSourcePropertyOwner;

    // Framebuffer and screen space quad
    GLuint _framebuffer;
    GLuint _quadVao;
    GLuint _quadVbo;

    // Opacity program
    ghoul::opengl::ProgramObject* _quadProgram = nullptr;
    UniformCache(opacity, colorTexture, depthTexture, positionTexture,
        normalTexture) _uniformOpacityCache;

    // Store the original RenderBin
    Renderable::RenderBin _originalRenderBin;
};

}  // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLEMODEL___H__
