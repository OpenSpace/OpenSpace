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

#include <modules/globebrowsing/src/layerrendersettings.h>

#include <openspace/opengl/gl.h>
#include <cmath>
#include <cstdlib>

namespace {
    using namespace openspace;

    constexpr PropertyInfo SetDefaultInfo = {
        .identifier = "SetDefault",
        .name = "Set default",
        .description = "If this value is triggered it will reset all of these values to "
            "their default values.",
        .visibility = Visibility::User
    };

    constexpr FloatPropertyInfo GammaInfo = {
        .identifier = "Gamma",
        .name = "Gamma",
        .description = "This value is used as an exponent to adjust the color for each "
            "tile.",
        .visibility = Visibility::User,
        .defaults = {
            .value = 1.f,
            .minMax = { 0.f, 5.f }
        }
    };

    constexpr FloatPropertyInfo MultiplierInfo = {
        .identifier = "Multiplier",
        .name = "Multiplier",
        .description = "This value is used as a multiplier to adjust the color applied "
            "after taking the gamma value as an exponent.",
        .visibility = Visibility::User,
        .defaults = {
            .value = 1.f,
            .minMax = { 0.f, 20.f }
        }
    };

    constexpr FloatPropertyInfo OffsetInfo = {
        .identifier = "Offset",
        .name = "Offset",
        .description = "This value is used as an additive modifier to adjust the color "
            "applied after the gamma exponent and the multiplier has been performed.",
        .visibility = Visibility::AdvancedUser,
        .defaults = {
            .value = 0.f,
            .minMax = { -10000.f, 10000.f }
        }
    };
} // namespace

namespace openspace {

LayerRenderSettings::LayerRenderSettings()
    : PropertyOwner({ "Settings" })
    , gamma(GammaInfo)
    , multiplier(MultiplierInfo)
    , offset(OffsetInfo)
    , setDefault(SetDefaultInfo)
{
    addProperty(gamma);
    addProperty(multiplier);
    addProperty(offset);
    addProperty(setDefault);

    setDefault.onChange([this]() {
        gamma = 1.f;
        multiplier = 1.f;
        offset = 0.f;
    });
}

void LayerRenderSettings::onChange(const std::function<void()>& callback) {
    gamma.onChange(callback);
    multiplier.onChange(callback);
    multiplier.onChange(callback);
    offset.onChange(callback);
}

float LayerRenderSettings::performLayerSettings(float value) const {
    return
        ((glm::sign(value) * std::pow(std::abs(value), gamma) * multiplier) + offset);
}

glm::vec4 LayerRenderSettings::performLayerSettings(const glm::vec4& currentValue) const {
    return glm::vec4(
        performLayerSettings(currentValue.r),
        performLayerSettings(currentValue.g),
        performLayerSettings(currentValue.b),
        performLayerSettings(currentValue.a)
    );
}

} // namespace openspace
