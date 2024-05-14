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
#include <filesystem>
#include <optional>

namespace {
    constexpr std::array<const char*, 4> UniformNames = {
        "modelViewProjectionTransform", "opacity", "width", "colorTexture"
    };

    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "The path to a file with a one-dimensional texture to be used for the disc "
        "color. The leftmost color will be innermost color when rendering the disc, "
        "and the rightmost color will be the outermost color.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size",
        "The outer radius of the disc, in meters.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo WidthInfo = {
        "Width",
        "Width",
        "The disc width, given as a ratio of the full disc radius. For example, a value "
        "of 1 results in a full circle, while 0.5 results in a disc where the inner "
        "radius is half of the full radius.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableDisc)]] Parameters {
        // [[codegen::verbatim(TextureInfo.description)]]
        std::filesystem::path texture;

        // [[codegen::verbatim(SizeInfo.description)]]
        std::optional<float> size;

        // [[codegen::verbatim(WidthInfo.description)]]
        std::optional<float> width [[codegen::inrange(0.0, 1.0)]];
    };
#include "renderabledisc_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableDisc::Documentation() {
    return codegen::doc<Parameters>("base_renderable_disc");
}

RenderableDisc::RenderableDisc(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _texturePath(TextureInfo)
    , _size(SizeInfo, 1.f, 0.f, 1e13f)
    , _width(WidthInfo, 0.5f, 0.f, 1.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _texturePath = p.texture.string();
    _texturePath.onChange([this]() { _texture->loadFromFile(_texturePath.value()); });
    addProperty(_texturePath);

    _size.setExponent(13.f);
    _size = p.size.value_or(_size);
    setBoundingSphere(_size);
    _size.onChange([this]() { _planeIsDirty = true; });
    addProperty(_size);

    _width = p.width.value_or(_width);
    addProperty(_width);

    addProperty(Fadeable::_opacity);

    setRenderBin(Renderable::RenderBin::PostDeferredTransparent);
}

bool RenderableDisc::isReady() const {
    return _shader && _texture && _plane;
}

void RenderableDisc::initialize() {
    _texture = std::make_unique<TextureComponent>(1);
    _texture->setFilterMode(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
    _texture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToEdge);
    _texture->setShouldWatchFileForChanges(true);

    _plane = std::make_unique<PlaneGeometry>(planeSize());
}

void RenderableDisc::initializeGL() {
    initializeShader();

    _texture->loadFromFile(_texturePath.value());
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

    const glm::dmat4 modelViewProjectionTransform =
        calcModelViewProjectionTransform(data);

    _shader->setUniform(
        _uniformCache.modelViewProjectionTransform,
        glm::mat4(modelViewProjectionTransform)
    );
    _shader->setUniform(_uniformCache.width, _width);
    _shader->setUniform(_uniformCache.opacity, opacity());

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _shader->setUniform(_uniformCache.colorTexture, unit);

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
