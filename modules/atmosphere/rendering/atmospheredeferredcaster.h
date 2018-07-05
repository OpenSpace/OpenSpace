/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
    AtmosphereDeferredcaster();
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
    void setOzoneHeightScale(float ozoneHeightScale);
    void setMieHeightScale(float mieHeightScale);
    void setMiePhaseConstant(float miePhaseConstant);
    void setSunRadianceIntensity(float sunRadiance);
    void setRayleighScatteringCoefficients(const glm::vec3& rayScattCoeff);
    void setOzoneExtinctionCoefficients(const glm::vec3& ozoneExtCoeff);
    void setMieScatteringCoefficients(const glm::vec3& mieScattCoeff);
    void setMieExtinctionCoefficients(const glm::vec3& mieExtCoeff);
    void setEllipsoidRadii(const glm::dvec3& radii);
    void setShadowConfigArray(const std::vector<ShadowConfiguration>& shadowConfigArray);
    void setHardShadows(bool enabled);
    void enableSunFollowing(bool enable);

    void setPrecalculationTextureScale(float preCalculatedTexturesScale);
    void enablePrecalculationTexturesSaving();

private:
    void loadComputationPrograms();
    void unloadComputationPrograms();
    void createComputationTextures();
    void deleteComputationTextures();
    void deleteUnusedComputationTextures();
    void executeCalculations(GLuint quadCalcVAO, GLenum drawBuffers[1],
        GLsizei vertexSize);
    void createRenderQuad(GLuint* vao, GLuint* vbo, GLfloat size);
    void step3DTexture(std::unique_ptr<ghoul::opengl::ProgramObject>& shaderProg,
        int layer, bool doCalculation = true);
    void checkFrameBufferState(const std::string& codePosition) const;
    void loadAtmosphereDataIntoShaderProgram(
        std::unique_ptr<ghoul::opengl::ProgramObject> & shaderProg
    );
    void renderQuadForCalc(GLuint vao, GLsizei numberOfVertices);
    void saveTextureToPPMFile(GLenum color_buffer_attachment, const std::string& fileName,
        int width, int height) const;
    bool isAtmosphereInFrustum(const glm::dmat4& MVMatrix, const glm::dvec3& position,
        double radius) const;

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
        betaMieExtinction, mieG, sunRadiance, ozoneLayerEnabled,
        HO, betaOzoneExtinction, SAMPLES_R,
        SAMPLES_MU, SAMPLES_MU_S, SAMPLES_NU) _uniformCache;
    UniformCache(dInverseModelTransformMatrix, dModelTransformMatrix,
        dSgctProjectionToModelTransformMatrix,
        dSGCTViewToWorldMatrix, dCamPosObj, sunDirectionObj,
        hardShadows, transmittanceTexture, irradianceTexture,
        inscatterTexture) _uniformCache2;

    GLuint _transmittanceTableTexture;
    GLuint _irradianceTableTexture;
    GLuint _inScatteringTableTexture;
    GLuint _deltaETableTexture;
    GLuint _deltaSRayleighTableTexture;
    GLuint _deltaSMieTableTexture;
    GLuint _deltaJTableTexture;
    GLuint _atmosphereTexture;

    ghoul::opengl::TextureUnit _transmittanceTableTextureUnit;
    ghoul::opengl::TextureUnit _irradianceTableTextureUnit;
    ghoul::opengl::TextureUnit _inScatteringTableTextureUnit;

    // Atmosphere Data
    bool _atmosphereCalculated;
    bool _ozoneEnabled;
    bool _sunFollowingCameraEnabled;
    float _atmosphereRadius;
    float _atmospherePlanetRadius;
    float _planetAverageGroundReflectance;
    float _planetGroundRadianceEmittion;
    float _rayleighHeightScale;
    float _ozoneHeightScale;
    float _mieHeightScale;
    float _miePhaseConstant;
    float _sunRadianceIntensity;

    glm::vec3 _rayleighScatteringCoeff;
    glm::vec3 _ozoneExtinctionCoeff;
    glm::vec3 _mieScatteringCoeff;
    glm::vec3 _mieExtinctionCoeff;
    glm::dvec3 _ellipsoidRadii;

    // Atmosphere Textures Dimmensions
    int _transmittance_table_width;
    int _transmittance_table_height;
    int _irradiance_table_width;
    int _irradiance_table_height;
    int _delta_e_table_width;
    int _delta_e_table_height;
    int _r_samples;
    int _mu_samples;
    int _mu_s_samples;
    int _nu_samples;

    glm::dmat4 _modelTransform;
    double _time;

    // Eclipse Shadows
    std::vector<ShadowConfiguration> _shadowConfArray;
    bool _hardShadowsEnabled;

    // Atmosphere Debugging
    float _calculationTextureScale;
    bool _saveCalculationTextures;
};

} // openspace

#endif // __OPENSPACE_MODULE_ATMOSPHERE___ATMOSPHEREDEFERREDCASTER___H__
