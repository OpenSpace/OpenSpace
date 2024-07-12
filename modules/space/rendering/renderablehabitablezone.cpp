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

#include <modules/space/rendering/renderablehabitablezone.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <optional>

namespace {
    constexpr std::array<const char*, 6> UniformNames = {
        "modelViewProjectionTransform", "opacity", "width", "transferFunctionTexture",
        "conservativeBounds", "showOptimistic"
    };

    constexpr openspace::properties::Property::PropertyInfo EffectiveTemperatureInfo = {
        "EffectiveTemperature",
        "Effective Temperature",
        "The effective temperature of the corresponding star, in Kelvin. Used to compute "
        "the width and size of the disc.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LuminosityInfo = {
        "Luminosity",
        "Luminosity",
        "The luminosity of the corresponding star, in units of solar luminosities. Used "
        "to compute the width and size of the disc.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo OptimisticInfo = {
        "Optimistic",
        "Optimistic" ,
        "If true, the habitable zone disc is rendered with the optimistic boundaries "
        "rather than the conservative ones.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo KopparapuTeffIntervalInfo = {
        "KopparapuTeffInterval",
        "Kopparapu TEFF",
        "The effective temperature interval for which Kopparapu's formula is used for "
        "the habitable zone computation. For stars with temperatures outside the range, "
        "a simpler method by Tom E. Harris is used. This method only uses the star "
        "luminosity and does not include computation of the optimistic boundaries.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(RenderableHabitableZone)]] Parameters {
        // [[codegen::verbatim(EffectiveTemperatureInfo.description)]]
        float effectiveTemperature;

        // [[codegen::verbatim(LuminosityInfo.description)]]
        float luminosity;

        // [[codegen::verbatim(OptimisticInfo.description)]]
        std::optional<bool> optimistic;

        // [[codegen::verbatim(KopparapuTeffIntervalInfo.description)]]
        std::optional<glm::vec2> kopparapuTeffInterval;
    };
#include "renderablehabitablezone_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableHabitableZone::Documentation() {
    return codegen::doc<Parameters>(
        "space_renderablehabitablezone",
        RenderableDisc::Documentation()
    );
}

RenderableHabitableZone::RenderableHabitableZone(const ghoul::Dictionary& dictionary)
    : RenderableDisc(dictionary)
    , _teff(EffectiveTemperatureInfo, 5780.f, 0.f, 7.5e4f)
    , _luminosity(LuminosityInfo, 1.f, 0.f, 1e8f)
    , _showOptimistic(OptimisticInfo, false)
    , _kopparapuTeffInterval(KopparapuTeffIntervalInfo, glm::vec2(1000.f, 10000.f))
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _teff = p.effectiveTemperature;
    _teff.onChange([this]() { computeZone(); });
    addProperty(_teff);

    _luminosity = p.luminosity;
    _luminosity.onChange([this]() { computeZone(); });
    addProperty(_luminosity);

    _showOptimistic = p.optimistic.value_or(_showOptimistic);
    addProperty(_showOptimistic);

    // The user should not be able to change this property. It's just used to communicate
    // the different rendering that happens outside of this interval
    addProperty(_kopparapuTeffInterval);
    _kopparapuTeffInterval.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    _kopparapuTeffInterval.setReadOnly(true);

    // Make parent's size related properties read only. We want to set them based on the
    // given temperature and luminosity
    _size.setReadOnly(true);
    _width.setReadOnly(true);

    computeZone();
    setBoundingSphere(_size);
}

void RenderableHabitableZone::render(const RenderData& data, RendererTasks&) {
    _shader->activate();

    _shader->setUniform(
        _uniformCache.modelViewProjection,
        glm::mat4(calcModelViewProjectionTransform(data))
    );
    _shader->setUniform(_uniformCache.width, _width);
    _shader->setUniform(_uniformCache.opacity, opacity());
    _shader->setUniform(_uniformCache.conservativeBounds, _conservativeBounds);
    _shader->setUniform(_uniformCache.showOptimistic, _showOptimistic);

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

void RenderableHabitableZone::initializeShader() {
    _shader = global::renderEngine->buildRenderProgram(
        "HabitableZoneProgram",
        absPath("${MODULE_SPACE}/shaders/habitablezone_vs.glsl"),
        absPath("${MODULE_SPACE}/shaders/habitablezone_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
}

void RenderableHabitableZone::updateUniformLocations() {
    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
}

void RenderableHabitableZone::computeZone() {
    glm::dvec4 distancesInAu = computeKopparapuZoneBoundaries(_teff, _luminosity);
    constexpr double AU = distanceconstants::AstronomicalUnit;
    const double inner = distancesInAu[0] * AU;
    const double innerConservative = distancesInAu[1] * AU;
    const double outerConservative = distancesInAu[2] * AU;
    const double outer = distancesInAu[3] * AU;

    double discWidth = 0.0;
    if (outer > 0.0) {
        discWidth = (outer - inner) / outer;
    }

    _size = static_cast<float>(outer);
    _width = static_cast<float>(discWidth);

    // Compute the coservative bounds normalized by the size of the disc, i.e. in [0, 1]
    _conservativeBounds = glm::vec2(innerConservative, outerConservative);
    _conservativeBounds /= _size;
}

glm::dvec4 RenderableHabitableZone::computeKopparapuZoneBoundaries(float teff,
                                                                   float luminosity)
{
    // Kopparapu's formula only considers stars with teff in range [2600, 7200] K.
    // However, we want to use the formula for more stars, so add some flexibility to
    // the teff boundaries (see constructor).
    // OBS! This also prevents problems with too large teff values in the computation
    const glm::vec2 teffBounds = _kopparapuTeffInterval;
    if (teff > teffBounds.y || teff < teffBounds.x) {
        // For the other stars, use a method by Tom E. Morris:
        // https://www.planetarybiology.com/calculating_habitable_zone.html
        const double L = static_cast<double>(luminosity);
        const double inner = std::sqrt(L / 1.1);
        const double outer = std::sqrt(L / 0.53);
        return glm::dvec4(inner, inner, outer, outer);
    }

    struct Coefficients {
        double seffSun;
        double a;
        double b;
        double c;
        double d;
    };

    // Coefficients for planets of 1 Earth mass. Received from:
    // https://depts.washington.edu/naivpl/sites/default/files/HZ_coefficients.dat
    constexpr std::array<Coefficients, 4> coefficients = {
        // Optimistic Inner boundary - Recent Venus
        Coefficients{ 1.77600E+00, 2.13600E-04, 2.53300E-08, -1.33200E-11, -3.09700E-15 },
        // Conservative Inner boundary - Runaway greenhouse
        Coefficients{ 1.10700E+00, 1.33200E-04, 1.58000E-08, -8.30800E-12, -1.93100E-15 },
        // Conservative Outer boundary - Maximum greenhouse
        Coefficients{ 3.56000E-01, 6.17100E-05, 1.69800E-09, -3.19800E-12, -5.57500E-16 },
        // Optimistic Outer boundary - Early Mars
        Coefficients{ 3.20000E-01, 5.54700E-05, 1.52600E-09, -2.87400E-12, -5.01100E-16 }
    };

    const double tstar = static_cast<double>(teff - 5780.f);
    const double tstar2 = tstar * tstar;
    const double L = static_cast<double>(luminosity);

    glm::dvec4 distances;
    for (int i = 0; i < 4; i++) {
        const Coefficients& coeffs = coefficients[i];
        const double seff = coeffs.seffSun + (coeffs.a * tstar) + (coeffs.b * tstar2) +
            (coeffs.c * tstar * tstar2) + (coeffs.d * tstar2 * tstar2);

        distances[i] = std::pow(L / seff, 0.5);
    }

    return distances;
}

} // namespace openspace
