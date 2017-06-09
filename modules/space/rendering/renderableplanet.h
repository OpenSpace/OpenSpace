/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_MODULE_SPACE___RENDERABLEPLANET___H__
#define __OPENSPACE_MODULE_SPACE___RENDERABLEPLANET___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/opengl/textureunit.h>

#include <memory>
#include <string>
#include <vector>

namespace ghoul {
    namespace opengl {
        class ProgramObject;
        class Texture;
    }
}

namespace openspace {

#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
class AtmosphereDeferredcaster;
#endif

struct TransformData;

namespace planetgeometry {
class PlanetGeometry;
}

namespace documentation { struct Documentation; }

class RenderablePlanet : public Renderable {
public:
    // Shadow structure
    typedef struct {
        std::pair<std::string, float> source;
        std::pair<std::string, float> caster;
    } ShadowConf;

    struct ShadowRenderingStruct {
        float xu, xp;
        float rs, rc;
        glm::vec3 sourceCasterVec;
        glm::vec3 casterPositionVec;
        bool isShadowing;
    };

public:
    RenderablePlanet(const ghoul::Dictionary& dictionary);

    bool initialize() override;
    bool deinitialize() override;
    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& tasks) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

protected:
    void loadTexture();

private: 
    void computeModelTransformMatrix(const openspace::TransformData & transformData, glm::dmat4 * modelTransform);
#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
    void updateAtmosphereParameters();
#endif

private:
    properties::StringProperty _colorTexturePath;
    properties::StringProperty _nightTexturePath;
    properties::StringProperty _heightMapTexturePath;
    properties::FloatProperty _heightExaggeration;
    properties::BoolProperty _performShading;

    std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;

    std::unique_ptr<ghoul::opengl::Texture> _texture;
    std::unique_ptr<ghoul::opengl::Texture> _nightTexture;    
    std::unique_ptr<ghoul::opengl::Texture> _heightMapTexture;
        
    std::unique_ptr<planetgeometry::PlanetGeometry> _geometry;
    

#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
    properties::FloatProperty _atmosphereHeightP;
    properties::FloatProperty _groundAverageReflectanceP;
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
    properties::FloatProperty _mieScatteringCoefficientP;
    properties::FloatProperty _mieScatteringExtinctionPropCoefficientP;
    properties::FloatProperty _mieAsymmetricFactorGP;
    properties::FloatProperty _sunIntensityP;
    properties::FloatProperty _hdrExpositionP;
    properties::FloatProperty _gammaConstantP;

    bool _atmosphereEnabled;
    bool _ozoneLayerEnabled;
    float _atmosphereRadius;
    float _atmospherePlanetRadius;
    float _planetAverageGroundReflectance;
    float _rayleighHeightScale;
    float _ozoneHeightScale;
    float _mieHeightScale;
    float _miePhaseConstant;
    float _sunRadianceIntensity;
    float _hdrConstant;
    float _gammaConstant;

    glm::vec3 _mieExtinctionCoeff;
    glm::vec3 _rayleighScatteringCoeff;
    glm::vec3 _ozoneExtinctionCoeff;
    glm::vec3 _mieScatteringCoeff;

    // Atmosphere Debug
    bool _saveCalculationsToTexture;
    float _preCalculatedTexturesScale;

    std::unique_ptr<AtmosphereDeferredcaster> _deferredcaster;
#endif

    float _alpha;
    float _planetRadius;
    bool _hasNightTexture;
    bool _hasHeightTexture;
    bool _shadowEnabled;
    double _time;

    glm::dmat3 _stateMatrix;

    std::vector< ShadowConf > _shadowConfArray;

    
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___RENDERABLEPLANET___H__
