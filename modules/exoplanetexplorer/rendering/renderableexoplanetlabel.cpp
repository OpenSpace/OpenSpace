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

#include <modules/exoplanetexplorer/rendering/renderableexoplanetlabel.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/font/font.h>
#include <openspace/font/fontmanager.h>
#include <openspace/font/fontrenderer.h>
#include <openspace/opengl/openglstatecache.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <cmath>
#include <optional>

namespace {
    using namespace openspace;

    constexpr StringPropertyInfo TextInfo = {
        .identifier = "Text",
        .name = "Text",
        .description = "The text displayed at the node position.",
        .visibility = Visibility::NoviceUser
    };

    constexpr Vec3PropertyInfo ColorInfo = {
        .identifier = "Color",
        .name = "Color",
        .description = "The label text color.",
        .visibility = Visibility::NoviceUser,
        .defaults = {
            .value = glm::vec3(1.f),
            .minMax = { glm::vec3(0.f), glm::vec3(1.f) }
        }
    };

    constexpr FloatPropertyInfo AngularSizeInfo = {
        .identifier = "AngularSize",
        .name = "Angular size",
        .description = "The angular line height in degrees, independent of distance "
            "and viewport resolution.",
        .visibility = Visibility::User,
        .defaults = {
            .value = 0.5f,
            .minMax = { 0.01f, 10.f }
        }
    };

    struct [[codegen::Dictionary(RenderableExoplanetLabel)]] Parameters {
        std::optional<std::string> text [[codegen::Info(TextInfo)]];
        std::optional<glm::vec3> color [[codegen::Info(ColorInfo), codegen::color()]];
        std::optional<float> angularSize
            [[codegen::Info(AngularSizeInfo), codegen::inrange(0.01f, 10.f)]];
    };
} // namespace
#include "renderableexoplanetlabel_codegen.cpp"

namespace openspace::exoplanets {

Documentation RenderableExoplanetLabel::Documentation() {
    return codegen::doc<Parameters>(
        "exoplanetexplorer_renderable_label",
        Renderable::Documentation()
    );
}

RenderableExoplanetLabel::RenderableExoplanetLabel(const Dictionary& dictionary)
    : Renderable(dictionary, { .automaticallyUpdateRenderBin = false })
    , _text(TextInfo)
    , _color(ColorInfo)
    , _angularSize(AngularSizeInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);

    _text = p.text.value_or(_text);
    addProperty(_text);

    _color = p.color.value_or(_color);
    _color.setViewOption(Property::ViewOptions::Color);
    addProperty(_color);

    _angularSize = p.angularSize.value_or(_angularSize);
    addProperty(_angularSize);
}

void RenderableExoplanetLabel::initialize() {
    // Always render these labels on top
    setRenderBin(Renderable::RenderBin::Overlay);
}

void RenderableExoplanetLabel::initializeGL() {
    _font = global::fontManager->font(
        "Mono",
        50.f,
        fontrendering::FontManager::Outline::Yes,
        fontrendering::FontManager::LoadGlyphs::No
    );
}

void RenderableExoplanetLabel::deinitializeGL() {
    _font.reset();
}

void RenderableExoplanetLabel::render(const RenderData& data, RendererTasks&) {
    if (!_font || _text.value().empty()) {
        return;
    }

    const glm::dvec3 position = data.modelTransform.translation;
    const glm::dvec3 toCamera = data.camera.position() - position;
    const double distance = glm::length(toCamera);
    if (!std::isfinite(distance) || distance <= 0.0) {
        return;
    }

    const glm::dvec3 normal = toCamera / distance;
    glm::dvec3 right = glm::cross(data.camera.lookUpVectorWorldSpace(), normal);
    if (glm::dot(right, right) < 1e-12) {
        const glm::dvec3 reference = std::abs(normal.z) < 0.9 ?
            glm::dvec3(0.0, 0.0, 1.0) : glm::dvec3(0.0, 1.0, 0.0);
        right = glm::cross(reference, normal);
    }
    right = glm::normalize(right);

    const glm::dvec3 up = glm::cross(normal, right);

    const double height = 2.0 * distance *
        std::tan(glm::radians(static_cast<double>(_angularSize.value())) * 0.5);

    const float scale = static_cast<float>(height / _font->height());

    if (!std::isfinite(scale) || scale <= 0.f) {
        return;
    }

    const fontrendering::FontRenderer::ProjectedLabelsInformation labelInfo = {
        // Depth information is not necessary since we always render the labels on top
        .enableDepth = false,
        // Disable the pixel size control and instead control the size ourselves
        .enablePixelSizeControl = false,
        .scale = scale,
        .renderType = 0,
        .mvpMatrix = calcModelViewProjectionTransform(data, glm::dmat4(1.0)),
        .orthoRight = glm::vec3(right),
        .orthoUp = glm::vec3(up),
        .cameraPos = data.camera.position(),
        .cameraLookUp = data.camera.lookUpVectorWorldSpace()
    };

    // Same here, no depth test needed
    glDepthMask(false);

    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    fontrendering::FontRenderer::defaultProjectionRenderer().render(
        *_font,
        glm::vec3(position),
        _text.value(),
        glm::vec4(_color.value(), opacity()),
        labelInfo
    );
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
}

} // namespace openspace::exoplanets
