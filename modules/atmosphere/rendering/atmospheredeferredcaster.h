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

#ifndef __OPENSPACE_MODULE_ATMOSPHERE___ATMOSPHEREDEFERREDCASTER___H__
#define __OPENSPACE_MODULE_ATMOSPHERE___ATMOSPHEREDEFERREDCASTER___H__

#include <openspace/rendering/deferredcaster.h>

#include <modules/atmosphere/rendering/renderableatmosphere.h>
#include <ghoul/glm.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/uniformcache.h>
#include <string>
#include <vector>

namespace ghoul::opengl {
    class Texture;
    class ProgramObject;
} // namespace ghoul::opengl

namespace openspace {

struct RenderData;
struct DeferredcastData;
struct ShadowConfiguration;

struct ShadowRenderingStruct {
    double umbra = 0.0;
    double penumbra = 0.0;
    double radiusSource = 0.0;
    double radiusCaster = 0.0;
    glm::dvec3 sourceCasterVec = glm::dvec3(0.0);
    glm::dvec3 casterPositionVec = glm::dvec3(0.0);
    bool isShadowing = false;
};

class AtmosphereDeferredcaster : public Deferredcaster {
public:
    AtmosphereDeferredcaster(float textureScale,
        std::vector<ShadowConfiguration> shadowConfigArray, bool saveCalculatedTextures);
    ~AtmosphereDeferredcaster() override = default;

    void initialize();
    void deinitialize();
    void preRaycast(const RenderData& data, const DeferredcastData& deferredData,
        ghoul::opengl::ProgramObject& program) override;
    void postRaycast(const RenderData& data, const DeferredcastData& deferredData,
        ghoul::opengl::ProgramObject& program) override;

    std::filesystem::path deferredcastVSPath() const override;
    std::filesystem::path deferredcastFSPath() const override;
    std::filesystem::path helperPath() const override;

    void initializeCachedVariables(ghoul::opengl::ProgramObject& program) override;

    void update(const UpdateData&) override;
    float eclipseShadow(const glm::dvec3& position);

    void calculateAtmosphereParameters();

    void setModelTransform(glm::dmat4 transform);
    void setOpacity(float opacity);

    void setParameters(float atmosphereRadius, float planetRadius,
        float averageGroundReflectance, float groundRadianceEmission,
        float rayleighHeightScale, bool enableOzone, float ozoneHeightScale,
        float mieHeightScale, float miePhaseConstant, float sunRadiance,
        glm::vec3 rayScatteringCoefficients, glm::vec3 ozoneExtinctionCoefficients,
        glm::vec3 mieScatteringCoefficients, glm::vec3 mieExtinctionCoefficients,
        bool sunFollowing, float sunAngularSize, SceneGraphNode* lightSourceNode);

    void setHardShadows(bool enabled);

private:
    void step3DTexture(ghoul::opengl::ProgramObject& prg, int layer) const;

    void calculateTransmittance();
    GLuint calculateDeltaE();
    std::pair<GLuint, GLuint> calculateDeltaS();
    void calculateIrradiance();
    void calculateInscattering(GLuint deltaSRayleigh, GLuint deltaSMie);
    void calculateDeltaJ(int scatteringOrder,
        ghoul::opengl::ProgramObject& program, GLuint deltaJ, GLuint deltaE,
        GLuint deltaSRayleigh, GLuint deltaSMie);
    void calculateDeltaE(int scatteringOrder,
        ghoul::opengl::ProgramObject& program, GLuint deltaE, GLuint deltaSRayleigh,
        GLuint deltaSMie);
    void calculateDeltaS(int scatteringOrder,
        ghoul::opengl::ProgramObject& program, GLuint deltaSRayleigh, GLuint deltaJ);
    void calculateIrradiance(int scatteringOrder,
        ghoul::opengl::ProgramObject& program, GLuint deltaE);
    void calculateInscattering(int scatteringOrder,
        ghoul::opengl::ProgramObject& program, GLuint deltaSRayleigh);


    UniformCache(cullAtmosphere, opacity, Rg, Rt, groundRadianceEmission, HR,
        betaRayleigh, HM, betaMieExtinction, mieG, sunRadiance, ozoneLayerEnabled, HO,
        betaOzoneExtinction, SAMPLES_R, SAMPLES_MU, SAMPLES_MU_S, SAMPLES_NU,
        inverseModelTransformMatrix, modelTransformMatrix,
        projectionToModelTransformMatrix, viewToWorldMatrix, camPosObj, sunDirectionObj,
        hardShadows, transmittanceTexture, irradianceTexture, inscatterTexture,
        sunAngularSize) _uniformCache;

    ghoul::opengl::TextureUnit _transmittanceTableTextureUnit;
    ghoul::opengl::TextureUnit _irradianceTableTextureUnit;
    ghoul::opengl::TextureUnit _inScatteringTableTextureUnit;

    GLuint _transmittanceTableTexture = 0;
    GLuint _irradianceTableTexture = 0;
    GLuint _inScatteringTableTexture = 0;

    // Atmosphere Data
    bool _ozoneEnabled = false;
    bool _sunFollowingCameraEnabled = false;
    float _atmosphereRadius = 0.f;
    float _atmospherePlanetRadius = 0.f;
    float _averageGroundReflectance = 0.f;
    float _groundRadianceEmission = 0.f;
    float _rayleighHeightScale = 0.f;
    float _ozoneHeightScale = 0.f;
    float _mieHeightScale = 0.f;
    float _miePhaseConstant = 0.f;
    float _sunRadianceIntensity = 5.f;
    float _sunAngularSize = 0.3f;
    SceneGraphNode* _lightSourceNode = nullptr;

    glm::vec3 _rayleighScatteringCoeff = glm::vec3(0.f);
    glm::vec3 _ozoneExtinctionCoeff = glm::vec3(0.f);
    glm::vec3 _mieScatteringCoeff = glm::vec3(0.f);
    glm::vec3 _mieExtinctionCoeff = glm::vec3(0.f);

    // Atmosphere Textures Dimmensions
    const glm::ivec2 _transmittanceTableSize;
    const glm::ivec2 _irradianceTableSize;
    const glm::ivec2 _deltaETableSize;
    const int _muSSamples;
    const int _nuSamples;
    const int _muSamples;
    const int _rSamples;
    const glm::ivec3 _textureSize;

    glm::dmat4 _modelTransform;
    float _opacity = 1.f;

    // Eclipse Shadows
    std::vector<ShadowConfiguration> _shadowConfArray;
    std::vector<ShadowRenderingStruct> _shadowDataArrayCache;
    bool _hardShadowsEnabled = false;

    // Atmosphere Debugging
    const bool _saveCalculationTextures = false;

    // Assuming < 1000 shadow casters, the longest uniform name that we are getting is
    // shadowDataArray[999].casterPositionVec
    // which needs to fit into the uniform buffer
    char _uniformNameBuffer[40];
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_ATMOSPHERE___ATMOSPHEREDEFERREDCASTER___H__
