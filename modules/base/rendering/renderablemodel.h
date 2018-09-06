/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/matrix/mat3property.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <ghoul/opengl/uniformcache.h>
#include <memory>

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

struct RenderData;
struct UpdateData;
class LightSource;

namespace documentation { struct Documentation; }
namespace modelgeometry { class ModelGeometry; }

class RenderableModel : public Renderable {
public:
    RenderableModel(const ghoul::Dictionary& dictionary);
    ~RenderableModel();

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

protected:
    void loadTexture();

private:
    std::unique_ptr<modelgeometry::ModelGeometry> _geometry;

    properties::StringProperty _colorTexturePath;

    properties::FloatProperty _ambientIntensity;
    properties::FloatProperty _diffuseIntensity;
    properties::FloatProperty _specularIntensity;

    properties::BoolProperty _performShading;
    properties::Mat3Property _modelTransform;

    ghoul::opengl::ProgramObject* _program = nullptr;
    UniformCache(opacity, nLightSources, lightDirectionsViewSpace, lightIntensities,
        modelViewTransform, projectionTransform, performShading, texture,
        ambientIntensity, diffuseIntensity, specularIntensity) _uniformCache;

    std::unique_ptr<ghoul::opengl::Texture> _texture;
    std::vector<std::unique_ptr<LightSource>> _lightSources;

    // Buffers for uniform uploading
    std::vector<float> _lightIntensitiesBuffer;
    std::vector<glm::vec3> _lightDirectionsViewSpaceBuffer;

    properties::PropertyOwner _lightSourcePropertyOwner;
};

}  // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLEMODEL___H__
