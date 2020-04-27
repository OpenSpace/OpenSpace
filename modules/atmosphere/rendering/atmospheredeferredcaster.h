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

#ifndef __OPENSPACE_MODULE_ATMOSPHERE___ATMOSPHEREDEFERREDCASTER___H__
#define __OPENSPACE_MODULE_ATMOSPHERE___ATMOSPHEREDEFERREDCASTER___H__

#include <openspace/rendering/deferredcaster.h>

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

class AtmosphereDeferredcaster : public Deferredcaster {
public:
    struct AdvancedATMModeData {
        glm::vec3 nRealRayleigh;
        glm::vec3 nComplexRayleigh;
        glm::vec3 nRealMie;
        glm::vec3 nComplexMie;
        glm::vec3 lambdaArray;
        glm::vec3 Kappa;
        glm::vec3 g1;
        glm::vec3 g2;
        glm::vec3 alpha;
        float deltaPolarizability;
        float NRayleigh;
        float NMie;
        float NRayleighAbsMolecule;
        float radiusAbsMoleculeRayleigh;
        float meanRadiusParticleMie;
        float turbidity;
        float jungeExponent;
        bool useOnlyAdvancedMie;
    };

public:
    virtual ~AtmosphereDeferredcaster() = default;

    void initialize();
    void deinitialize();
    void preRaycast(const RenderData& renderData, const DeferredcastData& deferredData,
                    ghoul::opengl::ProgramObject& program) override;
    void postRaycast(const RenderData& renderData, const DeferredcastData& deferredData,
                     ghoul::opengl::ProgramObject& program) override;

    std::string deferredcastPath() const override;
    std::string deferredcastVSPath() const override;
    std::string deferredcastFSPath() const override;
    std::string helperPath() const override;

    void initializeCachedVariables(ghoul::opengl::ProgramObject&) override;

    void update(const UpdateData&) override;

    void preCalculateAtmosphereParam();

    void setModelTransform(const glm::dmat4 &transform);
    void setTime(double time);
    void setAtmosphereRadius(float atmRadius);
    void setPlanetRadius(float planetRadius);
    void setPlanetAverageGroundReflectance(float averageGReflectance);
    void setPlanetGroundRadianceEmittion(float groundRadianceEmittion);
    void setRayleighHeightScale(float rayleighHeightScale);
    void enableOzone(bool enable);
    void enableOxygen(bool enable);
    void setOxygenHeightScale(float ozoneHeightScale);
    void setMieHeightScale(float mieHeightScale);
    void setMiePhaseConstant(float miePhaseConstant);
    void setSunRadianceIntensity(float sunRadiance);
    void setRayleighScatteringCoefficients(glm::vec3& rayScattCoeff);
    void setOxygenAbsCrossSections(glm::vec3 oxygenAbsCrossSections);
    void setOzoneAbsCrossSections(glm::vec3 ozoneAbsCrossSections);
    void setMieScatteringCoefficients(glm::vec3& mieScattCoeff);
    void setMieAbsorptionCoefficients(glm::vec3& mieAbsorbCoeff);
    void setMieExtinctionCoefficients(glm::vec3& mieExtCoeff);
    void setEllipsoidRadii(glm::dvec3& radii);
    void setShadowConfigArray(std::vector<ShadowConfiguration>& shadowConfigArray);
    void setAdvancedModeParameters(const AdvancedATMModeData &advData);
    void setHardShadows(bool enabled);
    void enableSunFollowing(bool enable);
    void enableAdvancedMode(bool enable);

    void setPrecalculationTextureScale(float preCalculatedTexturesScale);
    void enablePrecalculationTexturesSaving();

private:
    void loadComputationPrograms();
    void unloadComputationPrograms();
    void createComputationTextures();
    void deleteComputationTextures();
    void deleteUnusedComputationTextures();
    void executeCalculations();
    void createRenderQuad(GLuint* vao, GLuint* vbo, GLfloat size) const;
    void step3DTexture(std::unique_ptr<ghoul::opengl::ProgramObject>& shaderProg,
        int layer, bool doCalculation = true);
    void checkFrameBufferState(const std::string& codePosition) const;
    void loadAtmosphereDataIntoShaderProgram(
        std::unique_ptr<ghoul::opengl::ProgramObject> & shaderProg
    ) const;
    void renderQuadForCalc(GLuint vao, GLsizei numberOfVertices) const;
    void saveTextureToPPMFile(GLenum color_buffer_attachment, const std::string& fileName,
        int width, int height) const;
    void saveTextureToTxTFile(GLenum color_buffer_attachment, const std::string& fileName,
        int width, int height) const;
    bool isAtmosphereInFrustum(const glm::dmat4& MVMatrix, const glm::dvec3& position,
        double radius) const;

    //JCC: This method is used only for the ATM Paper
    //     Given the view direction and Sun position, it saves the Sky illumination 
    //     covering PI rad on zenith angle
    void saveSkyLuminance() const;

    // Number of planet radii to use as distance threshold for culling
    const double DISTANCE_CULLING_RADII = 5000;

    std::unique_ptr<ghoul::opengl::ProgramObject> _transmittanceProgramObject;
    std::unique_ptr<ghoul::opengl::ProgramObject> _irradianceProgramObject;
    std::unique_ptr<ghoul::opengl::ProgramObject> _irradianceSupTermsProgramObject;
    std::unique_ptr<ghoul::opengl::ProgramObject> _irradianceFinalProgramObject;
    std::unique_ptr<ghoul::opengl::ProgramObject> _inScatteringProgramObject;
    std::unique_ptr<ghoul::opengl::ProgramObject> _inScatteringSupTermsProgramObject;
    std::unique_ptr<ghoul::opengl::ProgramObject> _deltaEProgramObject;
    std::unique_ptr<ghoul::opengl::ProgramObject> _deltaSProgramObject;
    std::unique_ptr<ghoul::opengl::ProgramObject> _deltaSSupTermsProgramObject;
    std::unique_ptr<ghoul::opengl::ProgramObject> _deltaJProgramObject;
    std::unique_ptr<ghoul::opengl::ProgramObject> _atmosphereProgramObject;
    std::unique_ptr<ghoul::opengl::ProgramObject> _deferredAtmosphereProgramObject;

    UniformCache(cullAtmosphere, Rg, Rt,
        groundRadianceEmittion, HR, betaRayleigh, HM,
        betaMieExtinction, mieG, sunRadiance, ozoneLayerEnabled, oxygenAbsLayerEnabled,
        HO, betaOzoneExtinction, SAMPLES_R,
        SAMPLES_MU, SAMPLES_MU_S, SAMPLES_NU, advancedModeEnabled) _uniformCache;
    UniformCache(dInverseModelTransformMatrix, dModelTransformMatrix,
        dSgctProjectionToModelTransformMatrix,
        dSGCTViewToWorldMatrix, dCamPosObj, sunDirectionObj,
        hardShadows, transmittanceTexture, irradianceTexture,
        inscatterTexture, inscatterRayleighTexture, inscatterMieTexture) _uniformCache2;
    UniformCache(useOnlyAdvancedMie, deltaPolarizability, n_real_rayleigh, n_complex_rayleigh,
        n_real_mie, n_complex_mie, lambdaArray, N_rayleigh, N_mie,
        N_rayleigh_abs_molecule, radius_abs_molecule_rayleigh, mean_radius_particle_mie,
        turbidity, jungeExponent, Kappa, g1, g2, alpha) _uniformCacheAdvMode;

    GLuint _transmittanceTableTexture        = 0;
    GLuint _irradianceTableTexture           = 0;
    GLuint _inScatteringTableTexture         = 0;
    GLuint _inScatteringRayleighTableTexture = 0;
    GLuint _inScatteringMieTableTexture      = 0;
    GLuint _deltaETableTexture               = 0;
    GLuint _deltaSRayleighTableTexture       = 0;
    GLuint _deltaSMieTableTexture            = 0;
    GLuint _deltaJTableTexture               = 0;
    GLuint _atmosphereTexture                = 0;

    ghoul::opengl::TextureUnit _transmittanceTableTextureUnit;
    ghoul::opengl::TextureUnit _irradianceTableTextureUnit;
    ghoul::opengl::TextureUnit _inScatteringTableTextureUnit;
    ghoul::opengl::TextureUnit _inScatteringRayleighTableTextureUnit;
    ghoul::opengl::TextureUnit _inScatteringMieTableTextureUnit;

    // Atmosphere Data
    bool _atmosphereCalculated            = false;
    bool _ozoneEnabled                    = true;
    bool _oxygenEnabled                   = true;
    bool _sunFollowingCameraEnabled       = false;
    bool _advancedMode                    = false;
    float _atmosphereRadius               = 0.f;
    float _atmospherePlanetRadius         = 0.f;
    float _planetAverageGroundReflectance = 0.f;
    float _planetGroundRadianceEmittion   = 0.f;
    float _rayleighHeightScale            = 0.f;
    float _oxygenHeightScale              = 0.f;
    float _mieHeightScale                 = 0.f;
    float _miePhaseConstant               = 0.f;
    float _sunRadianceIntensity           = 5.f;

    // Advanced ATM Mode Data
    AdvancedATMModeData _advModeData;

    glm::vec3 _rayleighScatteringCoeff = glm::vec3(0);
    glm::vec3 _oxygenAbsCrossSection   = glm::vec3(0);
    glm::vec3 _ozoneAbsCrossSection    = glm::vec3(0);
    glm::vec3 _mieScatteringCoeff      = glm::vec3(0);
    glm::vec3 _mieAbsorptionCoeff      = glm::vec3(0);
    glm::vec3 _mieExtinctionCoeff      = glm::vec3(0);
    glm::dvec3 _ellipsoidRadii         = glm::dvec3(0);

    // Atmosphere Textures Dimmensions
    int _transmittance_table_width  = 256;
    int _transmittance_table_height = 64;
    int _irradiance_table_width     = 64;
    int _irradiance_table_height    = 16;
    int _delta_e_table_width        = 64;
    int _delta_e_table_height       = 16;
    int _r_samples                  = 32;
    int _mu_samples                 = 128;
    int _mu_s_samples               = 32;
    int _nu_samples                 = 8;

    glm::dmat4 _modelTransform;
    double _time = 0.0;

    // Eclipse Shadows
    std::vector<ShadowConfiguration> _shadowConfArray;
    bool _hardShadowsEnabled = false;

    // Atmosphere Debugging
    float _calculationTextureScale = 1.f;
    bool _saveCalculationTextures  = false;
};

} // openspace

#endif // __OPENSPACE_MODULE_ATMOSPHERE___ATMOSPHEREDEFERREDCASTER___H__
