/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/base/rendering/renderabledisc.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    constexpr const char _loggerCat[] = "RenderableDisc";

    constexpr const std::array<const char*, 4> UniformNames = {
        "modelViewProjectionTransform", "opacity", "width", "colorTexture"
    };

    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "This value is the path to a texture on disk that contains a one-dimensional "
        "texture to be used for the color."
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size",
        "This value specifies the outer radius of the disc in meter."
    };

    constexpr openspace::properties::Property::PropertyInfo WidthInfo = {
        "Width",
        "Width",
        "This value is used to set the width of the disc. The actual width is set "
        "based on the given size and this value should be set between 0 and 1. A value "
        "of 1 results in a full circle and 0.5 a disc with an inner radius of 0.5*size."
    };
} // namespace

namespace openspace {

documentation::Documentation RenderableDisc::Documentation() {
    using namespace documentation;
    return {
        "Renderable Disc",
        "renderable_disc",
        {
            {
                TextureInfo.identifier,
                new StringVerifier,
                Optional::No,
                TextureInfo.description
            },
            {
                SizeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                SizeInfo.description
            },
            {
                WidthInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                WidthInfo.description
            }
        }
    };
}

RenderableDisc::RenderableDisc(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _texturePath(TextureInfo)
    , _size(SizeInfo, 1.f, 0.f, 1e13f)
    , _width(WidthInfo, 0.5f, 0.f, 1.f)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableDisc"
    );

    _texturePath = absPath(dictionary.value<std::string>(TextureInfo.identifier));
    _texturePath.onChange([&]() { _texture->loadFromFile(_texturePath); });
    addProperty(_texturePath);

    if (dictionary.hasKey(SizeInfo.identifier)) {
        _size = static_cast<float>(dictionary.value<double>(SizeInfo.identifier));
    }
    setBoundingSphere(_size);
    _size.onChange([&]() { _planeIsDirty = true; });
    addProperty(_size);

    if (dictionary.hasKey(WidthInfo.identifier)) {
        _width = static_cast<float>(dictionary.value<double>(WidthInfo.identifier));
    }
    addProperty(_width);
    addProperty(_opacity);

    setRenderBin(Renderable::RenderBin::PostDeferredTransparent);
}

bool RenderableDisc::isReady() const {
    return _shader && _texture && _plane;
}

void RenderableDisc::initialize() {
    _texture = std::make_unique<TextureComponent>();
    _texture->setFilterMode(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
    _texture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToEdge);
    _texture->setShouldWatchFileForChanges(true);

    _plane = std::make_unique<PlaneGeometry>(planeSize());
}

void RenderableDisc::initializeGL() {
    initializeShader();

    _texture->loadFromFile(_texturePath);
    _texture->uploadToGpu();

    _plane->initialize();
}

void RenderableDisc::deinitializeGL() {
    _plane->deinitialize();
    _plane = nullptr;
    _texture = nullptr;

    global::renderEngine->removeRenderProgram(_shader.get());
    _shader = nullptr;
}

void RenderableDisc::render(const RenderData& data, RendererTasks&) {
    _shader->activate();

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    _shader->setUniform(
        _uniformCache.modelViewProjection,
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform)
    );
    _shader->setUniform(_uniformCache.width, _width);

    _shader->setUniform(_uniformCache.opacity, _opacity);

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _shader->setUniform(_uniformCache.texture, unit);

    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDepthMask(false);
    glDisable(GL_CULL_FACE);

    _plane->render();

    _shader->deactivate();

    // Restores GL State
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
    global::renderEngine->openglStateCache().resetPolygonAndClippingState();
}

void RenderableDisc::update(const UpdateData&) {
    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
        updateUniformLocations();
    }

    if (_planeIsDirty) {
        _plane->updateSize(planeSize());
        _planeIsDirty = false;
    }

    _texture->update();
}

void RenderableDisc::initializeShader() {
    _shader = global::renderEngine->buildRenderProgram(
        "DiscProgram",
        absPath("${MODULE_BASE}/shaders/disc_vs.glsl"),
        absPath("${MODULE_BASE}/shaders/disc_fs.glsl")
    );
    updateUniformLocations();
}

void RenderableDisc::updateUniformLocations() {
   ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
}

float RenderableDisc::planeSize() const {
    return _size;
}

} // namespace openspace
