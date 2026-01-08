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

#include <openspace/rendering/shadowmapping.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/misc/invariants.h>
#include <ghoul/systemcapabilities/openglcapabilitiescomponent.h>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo CastShadowInfo = {
        "CastShadow",
        "Cast shadow",
        "Enable model to cast shadow on its parent renderable.",
    };

    constexpr openspace::properties::Property::PropertyInfo FrustumSizeInfo = {
        "FrustumSize",
        "Size of the depth-pass view frustum",
        "Sets the width & height (effectively left/right & top/bottom) of the depth-pass "
        "view frustum, z-near & z-far are calculated.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(Shadower)]] Parameters {
        // [[codegen::verbatim(FrustumSizeInfo.description)]]
        std::optional<float> frustumSize;

        // [[codegen::verbatim(CastShadowInfo.description)]]
        std::optional<bool> castShadow;
    };
#include "shadowmapping_codegen.cpp"
} // namespace

namespace openspace::shadowmapping {

documentation::Documentation Shadower::Documentation() {
    return codegen::doc<Parameters>("core_shadower");
}

Shadower::Shadower(const ghoul::Dictionary& dictionary)
    : _castShadow(CastShadowInfo, false)
    , _frustumSize(FrustumSizeInfo, 1.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _frustumSize = p.frustumSize.value_or(_frustumSize);
    _hasFrustumSize = p.frustumSize.has_value();
    _frustumSize.onChange([&]() { _hasFrustumSize = true; });
    _castShadow = p.castShadow.value_or(_castShadow);
}

bool Shadower::isCastingShadow() const {
    return _castShadow;
}

void Shadower::setLightSource(const SceneGraphNode* lightSource) {
    ghoul_assert(lightSource, "No light source provided");
    _lightSource = std::move(lightSource);
}

const SceneGraphNode* Shadower::lightSource() const {
    return _lightSource;
}

void Shadower::setShadowGroup(std::string shadowGroup) {
    _shadowGroup = std::move(shadowGroup);
}

const std::string& Shadower::shadowGroup() const {
    return _shadowGroup;
}

double Shadower::shadowFrustumSize() const {
    return _frustumSize;
}

void Shadowee::addShadower(const Shadower* shadower) {
    ghoul_precondition(shadower, "Shadower must not be nullptr");

    if (std::find(_shadowers.begin(), _shadowers.end(), shadower) == _shadowers.end()) {
        _shadowers.push_back(shadower);
        _isShadowersDirty = true;
    }
}

void Shadowee::removeShadower(const Shadower* shadower) {
    ghoul_precondition(shadower, "Shadower must not be nullptr");

    auto it = std::find(_shadowers.begin(), _shadowers.end(), shadower);
    if (it != _shadowers.end()) {
        _shadowers.erase(it);
    }
}

} // namespace openspace shadowmapping
