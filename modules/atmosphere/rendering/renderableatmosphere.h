/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_MODULE_ATMOSPHERE___RENDERABLEATMOSPHERE___H__
#define __OPENSPACE_MODULE_ATMOSPHERE___RENDERABLEATMOSPHERE___H__

#include <openspace/rendering/renderable.h>

#include <modules/atmosphere/rendering/atmospheredeferredcaster.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/opengl/textureunit.h>

#include <memory>
#include <string>
#include <vector>

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
}

namespace openspace {

class AtmosphereDeferredcaster;

struct TransformData;

// Shadow structure
struct ShadowConfiguration {
    std::pair<std::string, double> source;
    std::pair<std::string, double> caster;
};

struct ShadowRenderingStruct {
    double xu = 0.0;
    double xp = 0.0;
    double rs = 0.0;
    double rc = 0.0;
    glm::dvec3 sourceCasterVec = glm::dvec3(0.0);
    glm::dvec3 casterPositionVec = glm::dvec3(0.0);
    bool isShadowing = false;
};

namespace documentation { struct Documentation; }
namespace planetgeometry { class PlanetGeometry; }

class RenderableAtmosphere : public Renderable {
public:
    RenderableAtmosphere(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;
    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    glm::dmat4 computeModelTransformMatrix(const openspace::TransformData& transformData);
    void updateAtmosphereParameters();

    properties::FloatProperty _atmosphereHeightP;
    properties::FloatProperty _groundAverageReflectanceP;
    properties::FloatProperty _groundRadianceEmittionP;
    properties::FloatProperty _rayleighHeightScaleP;
    properties::FloatProperty _rayleighScatteringCoeffXP;
    properties::FloatProperty _rayleighScatteringCoeffYP;
    properties::FloatProperty _rayleighScatteringCoeffZP;
    properties::BoolProperty  _ozoneEnabledP;
    properties::FloatProperty _ozoneHeightScaleP;
    properties::FloatProperty _ozoneCoeffXP;
    properties::FloatProperty _ozoneCoeffYP;
    properties::FloatProperty _ozoneCoeffZP;
    properties::FloatProperty _mieHeightScaleP;
    properties::FloatProperty _mieScatteringCoeffXP;
    properties::FloatProperty _mieScatteringCoeffYP;
    properties::FloatProperty _mieScatteringCoeffZP;
    properties::FloatProperty _mieScatteringExtinctionPropCoefficientP;
    properties::FloatProperty _mieAsymmetricFactorGP;
    properties::FloatProperty _sunIntensityP;
    properties::BoolProperty  _sunFollowingCameraEnabledP;
    properties::BoolProperty  _hardShadowsEnabledP;

    bool _atmosphereEnabled = false;
    bool _ozoneLayerEnabled = false;
    bool _sunFollowingCameraEnabled = false;
    float _atmosphereRadius = 0.f;
    float _atmospherePlanetRadius = 0.f;
    float _planetAverageGroundReflectance = 0.f;
    float _planetGroundRadianceEmittion = 0.f;
    float _rayleighHeightScale = 0.f;
    float _ozoneHeightScale = 0.f;
    float _mieHeightScale = 0.f;
    float _miePhaseConstant = 0.f;
    float _sunRadianceIntensity = 5.f;
    float _mieScattExtPropCoefProp = 1.f;

    glm::vec3 _mieExtinctionCoeff = glm::vec3(0.0);
    glm::vec3 _rayleighScatteringCoeff = glm::vec3(0.0);
    glm::vec3 _ozoneExtinctionCoeff = glm::vec3(0.0);
    glm::vec3 _mieScatteringCoeff = glm::dvec3(0.0);

    // Atmosphere Debug
    bool _saveCalculationsToTexture = false;
    float _preCalculatedTexturesScale = 1.f;

    std::unique_ptr<AtmosphereDeferredcaster> _deferredcaster;

    bool _shadowEnabled = false;
    bool _hardShadows = false;

    glm::dmat3 _stateMatrix = glm::dmat3(1.0);

    std::vector<ShadowConfiguration> _shadowConfArray;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_ATMOSPHERE___RENDERABLEATMOSPHERE___H__
