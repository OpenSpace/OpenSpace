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

#include <modules/exoplanets/rendering/renderablehabitablezone.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/distanceconstants.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    constexpr const char* _loggerCat = "RenderableHabitableZone";

    constexpr openspace::properties::Property::PropertyInfo EffectiveTemperatureInfo = {
        "EffectiveTemperature",
        "Effective Temperature",
        "The effective temperature of the corresponding star, in Kelvin."
        "Used to compute the width and size of the disc."
    };

    constexpr openspace::properties::Property::PropertyInfo LuminosityInfo = {
        "Luminosity",
        "Luminosity",
        "The luminosity of the corresponding star, in units of solar luminosities."
        "Used to compute the width and size of the disc."
    };
} // namespace

namespace openspace {

documentation::Documentation RenderableHabitableZone::Documentation() {
    using namespace documentation;
    documentation::Documentation doc {
        "Renderable Habitable Zone",
        "exoplanets_renderable_habitable_zone",
        {
            {
                "Type",
                new StringEqualVerifier("RenderableHabitableZone"),
                Optional::No
            },
            {
                EffectiveTemperatureInfo.identifier,
                new DoubleVerifier,
                Optional::No,
                EffectiveTemperatureInfo.description
            },
            {
                LuminosityInfo.identifier,
                new DoubleVerifier,
                Optional::No,
                LuminosityInfo.description
            }
        }
    };

    // @TODO cleanup
    // Insert the parents documentation entries until we have a verifier that can deal
    // with class hierarchy
    documentation::Documentation parentDoc = RenderableDisc::Documentation();
    doc.entries.insert(
        doc.entries.end(),
        parentDoc.entries.begin(),
        parentDoc.entries.end()
    );

    return doc;
}

RenderableHabitableZone::RenderableHabitableZone(const ghoul::Dictionary& dictionary)
    : RenderableDisc(dictionary)
    , _teff(EffectiveTemperatureInfo, 5780.f, 0.f, 7.5e4f)
    , _luminosity(LuminosityInfo, 1.f, 0.f, 1e8f)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableHabitableZone"
    );

    if (dictionary.hasKey(EffectiveTemperatureInfo.identifier)) {
        _teff = static_cast<float>(
            dictionary.value<double>(EffectiveTemperatureInfo.identifier)
        );
    }
    _teff.onChange([this]() { computeZone(); });
    addProperty(_teff);

    if (dictionary.hasKey(LuminosityInfo.identifier)) {
        _luminosity = static_cast<float>(
            dictionary.value<double>(LuminosityInfo.identifier)
        );
    }
    _luminosity.onChange([this]() { computeZone(); });
    addProperty(_luminosity);

    // Make parent's size related properties read only. We want to set them based on the
    // given temperature and luminosity
    _size.setReadOnly(true);
    _width.setReadOnly(true);

    computeZone();
}

void RenderableHabitableZone::computeZone() {
    glm::vec2 distancesInAu = computeKopparapuZoneBoundaries(_teff, _luminosity);
    constexpr float AU = static_cast<float>(distanceconstants::AstronomicalUnit);
    const float inner = distancesInAu[0] * AU;
    const float outer = distancesInAu[1] * AU;

    float discWidth = 0.f;
    if (outer > 0.f) {
        discWidth = (outer - inner) / outer;
    }

    _size.set(outer);
    _width.set(discWidth);
}

glm::vec2 RenderableHabitableZone::computeKopparapuZoneBoundaries(float teff,
                                                                  float luminosity)
{
    // Kopparapu's formula only considers stars with teff in range [2600, 7200] K.
    // However, we want to use the formula for more stars, so add some flexibility to
    // the teff boundaries
    if (teff > 8000.f || teff < 2000.f) {
        // For the other stars, use a method by Tom E. Morris:
        // https://www.planetarybiology.com/calculating_habitable_zone.html
        float inner = std::sqrt(luminosity / 1.1f);
        float outer = std::sqrt(luminosity / 0.53f);
        return glm::vec2(inner, outer);
    }

    struct Coefficients {
        float seffSun;
        float a;
        float b;
        float c;
        float d;
    };

    // Coefficients for planets of 1 Earth mass. Received from:
    // https://depts.washington.edu/naivpl/sites/default/files/HZ_coefficients.dat
    constexpr const Coefficients coefficients[] = {
        // Inner boundary - Runaway greenhouse
        {1.10700E+00f, 1.33200E-04f, 1.58000E-08f, -8.30800E-12f, -1.93100E-15f},
        // Outer boundary - Maximum greenhouse
        {3.56000E-01f, 6.17100E-05f, 1.69800E-09f, -3.19800E-12f, -5.57500E-16f}
    };

    const float tstar = teff - 5780.f;
    const float tstar2 = tstar * tstar;

    glm::vec2 distances;
    for (int i = 0; i < 2; ++i) {
        const Coefficients& coeffs = coefficients[i];
        float seff = coeffs.seffSun + (coeffs.a * tstar) + (coeffs.b * tstar2) +
            (coeffs.c * tstar * tstar2) + (coeffs.d * tstar2 * tstar2);

        distances[i] = std::pow(luminosity / seff, 0.5f);
    }

    return distances;
}

} // namespace openspace
