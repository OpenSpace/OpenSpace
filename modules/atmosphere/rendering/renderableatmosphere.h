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

#ifndef __OPENSPACE_MODULE_ATMOSPHERE___RENDERABLEATMOSPHERE___H__
#define __OPENSPACE_MODULE_ATMOSPHERE___RENDERABLEATMOSPHERE___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/opengl/textureunit.h>

#include <memory>
#include <string>
#include <vector>

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

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

    properties::FloatProperty _atmosphereHeight;
    properties::FloatProperty _groundAverageReflectance;
    properties::FloatProperty _groundRadianceEmission;
    properties::FloatProperty _rayleighHeightScale;
    properties::Vec3Property _rayleighScatteringCoeff;
    properties::BoolProperty _ozoneEnabled;
    properties::FloatProperty _ozoneHeightScale;
    properties::Vec3Property _ozoneCoeff;
    properties::FloatProperty _mieHeightScale;
    properties::Vec3Property _mieScatteringCoeff;
    properties::FloatProperty _mieScatteringExtinctionPropCoefficient;
    properties::FloatProperty _miePhaseConstant;
    properties::FloatProperty _sunIntensity;
    properties::BoolProperty _sunFollowingCameraEnabled;
    properties::BoolProperty _hardShadowsEnabled;

    float _planetRadius = 0.f;
    float _mieScattExtPropCoefProp = 1.f;

    glm::vec3 _mieExtinctionCoeff = glm::vec3(0.f);

    // Atmosphere Debug
    bool _saveCalculationsToTexture = false;
    float _preCalculatedTexturesScale = 1.f;

    std::unique_ptr<AtmosphereDeferredcaster> _deferredcaster;

    bool _shadowEnabled = false;
    bool _deferredCasterNeedsUpdate = false;
    bool _deferredCasterNeedsCalculation = false;

    std::vector<ShadowConfiguration> _shadowConfArray;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_ATMOSPHERE___RENDERABLEATMOSPHERE___H__
