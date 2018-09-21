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

 /***************************************************************************************
 * Modified part of the code (4D texture mechanism) from Eric Bruneton is used in the
 * following code.
 ****************************************************************************************/

/**
 * Precomputed Atmospheric Scattering
 * Copyright (c) 2008 INRIA
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <modules/atmosphere/rendering/atmospheredeferredcaster.h>

#include <modules/atmosphere/rendering/renderableatmosphere.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/spicemanager.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/renderer.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <glm/gtx/string_cast.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtx/transform.hpp>
#include <glm/gtx/vector_angle.hpp>
#include <glm/gtc/quaternion.hpp>
#include <sstream>
#include <fstream>

#ifdef WIN32
#define _USE_MATH_DEFINES
#endif // WIN32
#include <math.h>


namespace {
    constexpr const char* _loggerCat = "AtmosphereDeferredcaster";

    constexpr const std::array<const char*, 17> UniformNames1 = {
        "cullAtmosphere", "Rg", "Rt", "groundRadianceEmittion", "HR", "betaRayleigh",
        "HM", "betaMieExtinction", "mieG", "sunRadiance", "ozoneLayerEnabled", "HO",
        "betaOzoneExtinction", "SAMPLES_R", "SAMPLES_MU", "SAMPLES_MU_S", "SAMPLES_NU"
    };

    constexpr const std::array<const char*, 10> UniformNames2 = {
        "dInverseModelTransformMatrix", "dModelTransformMatrix",
        "dSgctProjectionToModelTransformMatrix", "dSGCTViewToWorldMatrix", "dCamPosObj",
        "sunDirectionObj", "hardShadows", "transmittanceTexture", "irradianceTexture",
        "inscatterTexture"
    };

    constexpr const char* GlslDeferredcastPath =
        "${MODULES}/atmosphere/shaders/atmosphere_deferred_fs.glsl";
    constexpr const char* GlslDeferredcastFSPath =
        "${MODULES}/atmosphere/shaders/atmosphere_deferred_fs.glsl";
    constexpr const char* GlslDeferredcastVsPath =
        "${MODULES}/atmosphere/shaders/atmosphere_deferred_vs.glsl";

    constexpr const float ATM_EPS = 2.f;
    constexpr const float KM_TO_M = 1000.f;
} // namespace

namespace openspace {

AtmosphereDeferredcaster::AtmosphereDeferredcaster()
    : _transmittanceProgramObject(nullptr)
    , _irradianceProgramObject(nullptr)
    , _irradianceSupTermsProgramObject(nullptr)
    , _irradianceFinalProgramObject(nullptr)
    , _inScatteringProgramObject(nullptr)
    , _inScatteringSupTermsProgramObject(nullptr)
    , _deltaEProgramObject(nullptr)
    , _deltaSProgramObject(nullptr)
    , _deltaSSupTermsProgramObject(nullptr)
    , _deltaJProgramObject(nullptr)
    , _atmosphereProgramObject(nullptr)
    , _transmittanceTableTexture(0)
    , _irradianceTableTexture(0)
    , _inScatteringTableTexture(0)
    , _deltaETableTexture(0)
    , _deltaSRayleighTableTexture(0)
    , _deltaSMieTableTexture(0)
    , _deltaJTableTexture(0)
    , _atmosphereTexture(0)
    , _atmosphereCalculated(false)
    , _ozoneEnabled(false)
    , _sunFollowingCameraEnabled(false)
    , _atmosphereRadius(0.f)
    , _atmospherePlanetRadius(0.f)
    , _planetAverageGroundReflectance(0.f)
    , _planetGroundRadianceEmittion(0.f)
    , _rayleighHeightScale(0.f)
    , _ozoneHeightScale(0.f)
    , _mieHeightScale(0.f)
    , _miePhaseConstant(0.f)
    , _sunRadianceIntensity(50.0f)
    , _rayleighScatteringCoeff(glm::vec3(0.f))
    , _ozoneExtinctionCoeff(glm::vec3(0.f))
    , _mieScatteringCoeff(glm::vec3(0.f))
    , _mieExtinctionCoeff(glm::vec3(0.f))
    , _ellipsoidRadii(glm::dvec3(0.0))
    , _transmittance_table_width(256)
    , _transmittance_table_height(64)
    , _irradiance_table_width(64)
    , _irradiance_table_height(16)
    , _delta_e_table_width(64)
    , _delta_e_table_height(16)
    , _r_samples(32)
    , _mu_samples(128)
    , _mu_s_samples(32)
    , _nu_samples(8)
    , _hardShadowsEnabled(false)
    , _calculationTextureScale(1.0)
    , _saveCalculationTextures(false)
{}

void AtmosphereDeferredcaster::initialize() {
    if (!_atmosphereCalculated) {
        preCalculateAtmosphereParam();
    }
}

void AtmosphereDeferredcaster::deinitialize() {
    _transmittanceProgramObject = nullptr;
    _irradianceProgramObject = nullptr;
    _irradianceSupTermsProgramObject = nullptr;
    _inScatteringProgramObject = nullptr;
    _inScatteringSupTermsProgramObject = nullptr;
    _deltaEProgramObject = nullptr;
    _deltaSProgramObject = nullptr;
    _deltaSSupTermsProgramObject = nullptr;
    _deltaJProgramObject = nullptr;

    glDeleteTextures(1, &_transmittanceTableTexture);
    glDeleteTextures(1, &_irradianceTableTexture);
    glDeleteTextures(1, &_inScatteringTableTexture);
    glDeleteTextures(1, &_deltaETableTexture);
    glDeleteTextures(1, &_deltaSRayleighTableTexture);
    glDeleteTextures(1, &_deltaSMieTableTexture);
    glDeleteTextures(1, &_deltaJTableTexture);
    glDeleteTextures(1, &_atmosphereTexture);
}

void AtmosphereDeferredcaster::preRaycast(const RenderData& renderData,
                                          const DeferredcastData&,
                                          ghoul::opengl::ProgramObject& program)
{
    // Atmosphere Frustum Culling
    glm::dvec3 tPlanetPosWorld = glm::dvec3(
        _modelTransform * glm::dvec4(0.0, 0.0, 0.0, 1.0)
    );

    const double distance = glm::distance(
        tPlanetPosWorld,
        renderData.camera.eyePositionVec3()
    );

    // Radius is in KM
    const double scaledRadius = glm::length(
        glm::dmat3(_modelTransform) * glm::dvec3(1000.0 * _atmosphereRadius, 0.0, 0.0)
    );

    if (distance > scaledRadius * DISTANCE_CULLING_RADII) {
        program.setUniform(_uniformCache.cullAtmosphere, 1);
    }
    else {
        glm::dmat4 MV = glm::dmat4(
            renderData.camera.sgctInternal.projectionMatrix()
        ) * renderData.camera.combinedViewMatrix();

        if (!isAtmosphereInFrustum(
            MV,
            tPlanetPosWorld,
            (_atmosphereRadius + ATM_EPS)*KM_TO_M)
            )
        {
            program.setUniform(_uniformCache.cullAtmosphere, 1);
        }
        else {
            program.setUniform(_uniformCache.cullAtmosphere, 0);
            program.setUniform(_uniformCache.Rg, _atmospherePlanetRadius);
            program.setUniform(_uniformCache.Rt, _atmosphereRadius);
            program.setUniform(
                _uniformCache.groundRadianceEmittion,
                _planetGroundRadianceEmittion
            );
            program.setUniform(_uniformCache.HR, _rayleighHeightScale);
            program.setUniform(_uniformCache.betaRayleigh, _rayleighScatteringCoeff);
            program.setUniform(_uniformCache.HM, _mieHeightScale);
            program.setUniform(_uniformCache.betaMieExtinction, _mieExtinctionCoeff);
            program.setUniform(_uniformCache.mieG, _miePhaseConstant);
            program.setUniform(_uniformCache.sunRadiance, _sunRadianceIntensity);
            program.setUniform(_uniformCache.ozoneLayerEnabled, _ozoneEnabled);
            program.setUniform(_uniformCache.HO, _ozoneHeightScale);
            program.setUniform(_uniformCache.betaOzoneExtinction, _ozoneExtinctionCoeff);
            program.setUniform(_uniformCache.SAMPLES_R, _r_samples);
            program.setUniform(_uniformCache.SAMPLES_MU, _mu_samples);
            program.setUniform(_uniformCache.SAMPLES_MU_S, _mu_s_samples);
            program.setUniform(_uniformCache.SAMPLES_NU, _nu_samples);

            // Object Space
            glm::dmat4 inverseModelMatrix = glm::inverse(_modelTransform);
            program.setUniform(
                _uniformCache2.dInverseModelTransformMatrix,
                inverseModelMatrix
            );
            program.setUniform(_uniformCache2.dModelTransformMatrix, _modelTransform);

            // Eye Space in SGCT to Eye Space in OS (SGCT View to OS Camera Rig)
            glm::dmat4 dSgctEye2OSEye = glm::inverse(
                glm::dmat4(renderData.camera.viewMatrix()));

            glm::dmat4 dSGCTViewToWorldMatrix = glm::inverse(
                renderData.camera.combinedViewMatrix()
            );

            // Eye Space in SGCT to OS World Space
            program.setUniform(_uniformCache2.dSGCTViewToWorldMatrix,
                dSGCTViewToWorldMatrix);

            // SGCT Projection to SGCT Eye Space
            glm::dmat4 dInverseProjection = glm::inverse(
                glm::dmat4(renderData.camera.projectionMatrix()));

            // SGCT Projection to OS Camera Before Rotation
            glm::dmat4 dProjectionToTmpRotTransformMatrix =
                glm::mat4_cast(
                    static_cast<glm::dquat>(renderData.camera.rotationQuaternion())
                ) *
                dSgctEye2OSEye *
                glm::inverse(renderData.camera.viewScaleMatrix()) *
                dInverseProjection;

            // SGCT Projection to World Space
            glm::dmat4 dSgctProjectionToWorldTransformMatrix(
                dProjectionToTmpRotTransformMatrix
            );
            double* mSource = glm::value_ptr(dSgctProjectionToWorldTransformMatrix);

            mSource[12] += renderData.camera.eyePositionVec3().x;
            mSource[13] += renderData.camera.eyePositionVec3().y;
            mSource[14] += renderData.camera.eyePositionVec3().z;
            mSource[15] = 1.0;


            // SGCT Projection to Object Space
            glm::dmat4 inverseWholeMatrixPipeline = inverseModelMatrix *
                dSgctProjectionToWorldTransformMatrix;
            program.setUniform(_uniformCache2.dSgctProjectionToModelTransformMatrix,
                inverseWholeMatrixPipeline);

            glm::dvec4 camPosObjCoords = inverseModelMatrix *
                                     glm::dvec4(renderData.camera.eyePositionVec3(), 1.0);
            program.setUniform(_uniformCache2.dCamPosObj, camPosObjCoords);

            double lt;
            glm::dvec3 sunPosWorld = SpiceManager::ref().targetPosition(
                "SUN",
                "SUN",
                "GALACTIC",
                {},
                _time,
                lt
            );
            glm::dvec4 sunPosObj = glm::dvec4(0.0);

            // Sun following camera position
            if (_sunFollowingCameraEnabled) {
                sunPosObj = inverseModelMatrix * glm::dvec4(
                    renderData.camera.eyePositionVec3(),
                    1.0
                );
            }
            else {
                sunPosObj = inverseModelMatrix *
                    glm::dvec4(sunPosWorld - renderData.modelTransform.translation, 1.0);
            }

            // Sun Position in Object Space
            program.setUniform(
                _uniformCache2.sunDirectionObj,
                glm::normalize(glm::dvec3(sunPosObj))
            );

            // Shadow calculations..
            if (!_shadowConfArray.empty()) {
                std::vector<ShadowRenderingStruct> shadowDataArray;
                shadowDataArray.reserve(_shadowConfArray.size());

                for (const ShadowConfiguration & shadowConf : _shadowConfArray) {
                    // TO REMEMBER: all distances and lengths in world coordinates are in
                    // meters!!! We need to move this to view space...
                    // Getting source and caster:
                    glm::dvec3 sourcePos = SpiceManager::ref().targetPosition(
                        shadowConf.source.first,
                        "SUN", "GALACTIC",
                        {},
                        _time,
                        lt
                    );
                    sourcePos *= KM_TO_M; // converting to meters
                    glm::dvec3 casterPos = SpiceManager::ref().targetPosition(
                        shadowConf.caster.first,
                        "SUN", "GALACTIC",
                        {},
                        _time,
                        lt
                    );
                    casterPos *= KM_TO_M; // converting to meters

                    // First we determine if the caster is shadowing the current planet
                    // (all calculations in World Coordinates):
                    glm::dvec3 planetCasterVec = casterPos - renderData.position.dvec3();
                    glm::dvec3 sourceCasterVec = casterPos - sourcePos;
                    double sc_length = glm::length(sourceCasterVec);
                    glm::dvec3 planetCaster_proj = (
                        glm::dot(planetCasterVec, sourceCasterVec) /
                        (sc_length*sc_length)) * sourceCasterVec;
                    double d_test = glm::length(planetCasterVec - planetCaster_proj);
                    double xp_test = shadowConf.caster.second *
                        sc_length / (shadowConf.source.second + shadowConf.caster.second);
                    double rp_test = shadowConf.caster.second *
                        (glm::length(planetCaster_proj) + xp_test) / xp_test;

                    double casterDistSun = glm::length(casterPos - sunPosWorld);
                    double planetDistSun = glm::length(
                        renderData.position.dvec3() - sunPosWorld
                    );

                    ShadowRenderingStruct shadowData;
                    shadowData.isShadowing = false;

                    if (((d_test - rp_test) < (_atmospherePlanetRadius * KM_TO_M)) &&
                    //if (((d_test - rp_test) < (_atmosphereRadius * KM_TO_M)) &&
                        (casterDistSun < planetDistSun)) {
                        // The current caster is shadowing the current planet
                        shadowData.isShadowing = true;
                        shadowData.rs = shadowConf.source.second;
                        shadowData.rc = shadowConf.caster.second;
                        shadowData.sourceCasterVec = glm::normalize(sourceCasterVec);
                        shadowData.xp = xp_test;
                        shadowData.xu = shadowData.rc * sc_length /
                                        (shadowData.rs - shadowData.rc);
                        shadowData.casterPositionVec = casterPos;
                    }
                    shadowDataArray.push_back(shadowData);
                }

                const std::string uniformVarName("shadowDataArray[");
                unsigned int counter = 0;
                for (const ShadowRenderingStruct & sd : shadowDataArray) {
                    std::stringstream ss;
                    ss << uniformVarName << counter << "].isShadowing";
                    program.setUniform(ss.str(), sd.isShadowing);
                    if (sd.isShadowing) {
                        ss.str(std::string());
                        ss << uniformVarName << counter << "].xp";
                        program.setUniform(ss.str(), sd.xp);
                        ss.str(std::string());
                        ss << uniformVarName << counter << "].xu";
                        program.setUniform(ss.str(), sd.xu);
                        // ss.str(std::string());
                        // ss << uniformVarName << counter << "].rs";
                        // program.setUniform(ss.str(), sd.rs);
                        ss.str(std::string());
                        ss << uniformVarName << counter << "].rc";
                        program.setUniform(ss.str(), sd.rc);
                        ss.str(std::string());
                        ss << uniformVarName << counter << "].sourceCasterVec";
                        program.setUniform(ss.str(), sd.sourceCasterVec);
                        ss.str(std::string());
                        ss << uniformVarName << counter << "].casterPositionVec";
                        program.setUniform(ss.str(), sd.casterPositionVec);
                    }
                    counter++;
                }
                program.setUniform(_uniformCache2.hardShadows, _hardShadowsEnabled);
            }
        }
    }
    _transmittanceTableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
    program.setUniform(
        _uniformCache2.transmittanceTexture,
        _transmittanceTableTextureUnit
    );

    _irradianceTableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D, _irradianceTableTexture);
    program.setUniform(_uniformCache2.irradianceTexture, _irradianceTableTextureUnit);

    _inScatteringTableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_3D, _inScatteringTableTexture);
    program.setUniform(_uniformCache2.inscatterTexture, _inScatteringTableTextureUnit);
}

void AtmosphereDeferredcaster::postRaycast(const RenderData&,
                                           const DeferredcastData&,
                                           ghoul::opengl::ProgramObject&)
{
    // Deactivate the texture units
    _transmittanceTableTextureUnit.deactivate();
    _irradianceTableTextureUnit.deactivate();
    _inScatteringTableTextureUnit.deactivate();
}

std::string AtmosphereDeferredcaster::deferredcastPath() const {
    return GlslDeferredcastPath;
}

std::string AtmosphereDeferredcaster::deferredcastFSPath() const {
    return GlslDeferredcastFSPath;
}

std::string AtmosphereDeferredcaster::deferredcastVSPath() const {
    return GlslDeferredcastVsPath;
}

std::string AtmosphereDeferredcaster::helperPath() const {
    return ""; // no helper file
}

void AtmosphereDeferredcaster::initializeCachedVariables(
                                                    ghoul::opengl::ProgramObject& program)
{
    ghoul::opengl::updateUniformLocations(program, _uniformCache, UniformNames1);
    ghoul::opengl::updateUniformLocations(program, _uniformCache2, UniformNames2);
}

void AtmosphereDeferredcaster::update(const UpdateData&) {}

void AtmosphereDeferredcaster::setModelTransform(const glm::dmat4& transform) {
    _modelTransform = transform;
}

void AtmosphereDeferredcaster::setTime(double time) {
    _time = time;
}

void AtmosphereDeferredcaster::setAtmosphereRadius(float atmRadius) {
    _atmosphereRadius = atmRadius;
}

void AtmosphereDeferredcaster::setPlanetRadius(float planetRadius) {
    _atmospherePlanetRadius = planetRadius;
}

void AtmosphereDeferredcaster::setPlanetAverageGroundReflectance(
                                                                float averageGReflectance)
{
    _planetAverageGroundReflectance = averageGReflectance;
}

void AtmosphereDeferredcaster::setPlanetGroundRadianceEmittion(
                                                             float groundRadianceEmittion)
{
    _planetGroundRadianceEmittion = groundRadianceEmittion;
}

void AtmosphereDeferredcaster::setRayleighHeightScale(float rayleighHeightScale) {
    _rayleighHeightScale = rayleighHeightScale;
}

void AtmosphereDeferredcaster::enableOzone(bool enable) {
    _ozoneEnabled = enable;
}

void AtmosphereDeferredcaster::setOzoneHeightScale(float ozoneHeightScale) {
    _ozoneHeightScale = ozoneHeightScale;
}

void AtmosphereDeferredcaster::setMieHeightScale(float mieHeightScale) {
    _mieHeightScale = mieHeightScale;
}

void AtmosphereDeferredcaster::setMiePhaseConstant(float miePhaseConstant) {
    _miePhaseConstant = miePhaseConstant;
}

void AtmosphereDeferredcaster::setSunRadianceIntensity(float sunRadiance) {
    _sunRadianceIntensity = sunRadiance;
}

void AtmosphereDeferredcaster::setRayleighScatteringCoefficients(
                                                           const glm::vec3& rayScattCoeff)
{
    _rayleighScatteringCoeff = std::move(rayScattCoeff);
}

void AtmosphereDeferredcaster::setOzoneExtinctionCoefficients(
                                                           const glm::vec3& ozoneExtCoeff)
{
    _ozoneExtinctionCoeff = std::move(ozoneExtCoeff);
}

void AtmosphereDeferredcaster::setMieScatteringCoefficients(
                                                           const glm::vec3& mieScattCoeff)
{
    _mieScatteringCoeff = std::move(mieScattCoeff);
}

void AtmosphereDeferredcaster::setMieExtinctionCoefficients(const glm::vec3& mieExtCoeff)
{
    _mieExtinctionCoeff = std::move(mieExtCoeff);
}

void AtmosphereDeferredcaster::setEllipsoidRadii(const glm::dvec3& radii) {
    _ellipsoidRadii = std::move(radii);
}

void AtmosphereDeferredcaster::setHardShadows(bool enabled) {
    _hardShadowsEnabled = enabled;
}

void AtmosphereDeferredcaster::setShadowConfigArray(
                                const std::vector<ShadowConfiguration>& shadowConfigArray)
{
    _shadowConfArray = std::move(shadowConfigArray);
}

void AtmosphereDeferredcaster::enableSunFollowing(bool enable) {
    _sunFollowingCameraEnabled = enable;
}

void AtmosphereDeferredcaster::setPrecalculationTextureScale(
                                                         float preCalculatedTexturesScale)
{
    _calculationTextureScale = preCalculatedTexturesScale;
    _transmittance_table_width *= static_cast<unsigned int>(_calculationTextureScale);
    _transmittance_table_height *= static_cast<unsigned int>(_calculationTextureScale);
    _irradiance_table_width *= static_cast<unsigned int>(_calculationTextureScale);
    _irradiance_table_height *= static_cast<unsigned int>(_calculationTextureScale);
    _delta_e_table_width *= static_cast<unsigned int>(_calculationTextureScale);
    _delta_e_table_height *= static_cast<unsigned int>(_calculationTextureScale);
    _r_samples *= static_cast<unsigned int>(_calculationTextureScale);
    _mu_samples *= static_cast<unsigned int>(_calculationTextureScale);
    _mu_s_samples *= static_cast<unsigned int>(_calculationTextureScale);
    _nu_samples *= static_cast<unsigned int>(_calculationTextureScale);
}

void AtmosphereDeferredcaster::enablePrecalculationTexturesSaving() {
    _saveCalculationTextures = true;
}

void AtmosphereDeferredcaster::loadComputationPrograms() {
    //============== Transmittance T =================
    if (!_transmittanceProgramObject) {
        _transmittanceProgramObject = ghoul::opengl::ProgramObject::Build(
            "transmittanceCalcProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/transmittance_calc_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/transmittance_calc_fs.glsl")
        );
    }
    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    _transmittanceProgramObject->setIgnoreSubroutineUniformLocationError(
        IgnoreError::Yes
    );
    _transmittanceProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    //============== Irradiance E =================
    if (!_irradianceProgramObject) {
        _irradianceProgramObject = ghoul::opengl::ProgramObject::Build(
            "irradianceCalcProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/irradiance_calc_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/irradiance_calc_fs.glsl"));
    }
    _irradianceProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _irradianceProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    if (!_irradianceSupTermsProgramObject) {
        _irradianceSupTermsProgramObject = ghoul::opengl::ProgramObject::Build(
            "irradianceSupTermsCalcProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/irradiance_sup_calc_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/irradiance_sup_calc_fs.glsl"));
    }
    _irradianceSupTermsProgramObject->setIgnoreSubroutineUniformLocationError(
        IgnoreError::Yes
    );
    _irradianceSupTermsProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    //============== InScattering S =================
    if (!_inScatteringProgramObject) {
        _inScatteringProgramObject = ghoul::opengl::ProgramObject::Build(
            "inScatteringCalcProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/inScattering_calc_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/inScattering_calc_fs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/inScattering_calc_gs.glsl"));
    }
    _inScatteringProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _inScatteringProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    if (!_inScatteringSupTermsProgramObject) {
        _inScatteringSupTermsProgramObject = ghoul::opengl::ProgramObject::Build(
            "inScatteringSupTermsCalcProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/inScattering_sup_calc_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/inScattering_sup_calc_fs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/inScattering_sup_calc_gs.glsl"));
    }
    _inScatteringSupTermsProgramObject->setIgnoreSubroutineUniformLocationError(
        IgnoreError::Yes
    );
    _inScatteringSupTermsProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    //============== Delta E =================
    if (!_deltaEProgramObject) {
        _deltaEProgramObject = ghoul::opengl::ProgramObject::Build(
            "deltaECalcProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/deltaE_calc_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/deltaE_calc_fs.glsl"));
    }
    _deltaEProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _deltaEProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    //============== Irradiance finel E =================
    if (!_irradianceFinalProgramObject) {
        _irradianceFinalProgramObject = ghoul::opengl::ProgramObject::Build(
            "irradianceEFinalProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/irradiance_final_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/irradiance_final_fs.glsl"));
    }
    _irradianceFinalProgramObject->setIgnoreSubroutineUniformLocationError(
        IgnoreError::Yes
    );
    _irradianceFinalProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    //============== Delta S =================
    if (!_deltaSProgramObject) {
        _deltaSProgramObject = ghoul::opengl::ProgramObject::Build(
            "deltaSCalcProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/deltaS_calc_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/deltaS_calc_fs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/deltaS_calc_gs.glsl"));
    }
    _deltaSProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _deltaSProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    if (!_deltaSSupTermsProgramObject) {
        _deltaSSupTermsProgramObject = ghoul::opengl::ProgramObject::Build(
            "deltaSSUPTermsCalcProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/deltaS_sup_calc_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/deltaS_sup_calc_fs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/deltaS_sup_calc_gs.glsl"));
    }
    _deltaSSupTermsProgramObject->setIgnoreSubroutineUniformLocationError(
        IgnoreError::Yes
    );
    _deltaSSupTermsProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    //============== Delta J (Radiance Scattered) =================
    if (!_deltaJProgramObject) {
        _deltaJProgramObject = ghoul::opengl::ProgramObject::Build(
            "deltaJCalcProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/deltaJ_calc_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/deltaJ_calc_fs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/deltaJ_calc_gs.glsl"));
    }
    _deltaJProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _deltaJProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);
}

void AtmosphereDeferredcaster::unloadComputationPrograms() {
    _transmittanceProgramObject = nullptr;
    _irradianceProgramObject = nullptr;
    _irradianceSupTermsProgramObject = nullptr;
    _inScatteringProgramObject = nullptr;
    _inScatteringSupTermsProgramObject = nullptr;
    _deltaEProgramObject = nullptr;
    _irradianceFinalProgramObject = nullptr;
    _deltaSProgramObject = nullptr;
    _deltaSSupTermsProgramObject = nullptr;
    _deltaJProgramObject = nullptr;
}

void AtmosphereDeferredcaster::createComputationTextures() {
    if (!_atmosphereCalculated) {
        //============== Transmittance =================
        ghoul::opengl::TextureUnit transmittanceTableTextureUnit;
        transmittanceTableTextureUnit.activate();
        glGenTextures(1, &_transmittanceTableTexture);
        glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        // Stopped using a buffer object for GL_PIXEL_UNPACK_BUFFER
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB32F, _transmittance_table_width,
            _transmittance_table_height, 0, GL_RGB, GL_FLOAT, nullptr);

        //============== Irradiance =================
        ghoul::opengl::TextureUnit irradianceTableTextureUnit;
        irradianceTableTextureUnit.activate();
        glGenTextures(1, &_irradianceTableTexture);
        glBindTexture(GL_TEXTURE_2D, _irradianceTableTexture);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB32F, _irradiance_table_width,
            _irradiance_table_height, 0, GL_RGB, GL_FLOAT, nullptr);

        //============== InScattering =================
        ghoul::opengl::TextureUnit inScatteringTableTextureUnit;
        inScatteringTableTextureUnit.activate();
        glGenTextures(1, &_inScatteringTableTexture);
        glBindTexture(GL_TEXTURE_3D, _inScatteringTableTexture);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
        glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA32F, _mu_s_samples * _nu_samples,
            _mu_samples, _r_samples, 0, GL_RGB, GL_FLOAT, nullptr);
    }

    //============== Delta E =================
    ghoul::opengl::TextureUnit deltaETableTextureUnit;
    deltaETableTextureUnit.activate();
    glGenTextures(1, &_deltaETableTexture);
    glBindTexture(GL_TEXTURE_2D, _deltaETableTexture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB32F, _delta_e_table_width,
        _delta_e_table_height, 0, GL_RGB, GL_FLOAT, nullptr);

    //============== Delta S =================
    ghoul::opengl::TextureUnit deltaSRayleighTableTextureUnit;
    deltaSRayleighTableTextureUnit.activate();
    glGenTextures(1, &_deltaSRayleighTableTexture);
    glBindTexture(GL_TEXTURE_3D, _deltaSRayleighTableTexture);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage3D(GL_TEXTURE_3D, 0, GL_RGB32F, _mu_s_samples * _nu_samples,
        _mu_samples, _r_samples, 0, GL_RGB, GL_FLOAT, nullptr);

    ghoul::opengl::TextureUnit deltaSMieTableTextureUnit;
    deltaSMieTableTextureUnit.activate();
    glGenTextures(1, &_deltaSMieTableTexture);
    glBindTexture(GL_TEXTURE_3D, _deltaSMieTableTexture);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage3D(GL_TEXTURE_3D, 0, GL_RGB32F, _mu_s_samples * _nu_samples,
        _mu_samples, _r_samples, 0, GL_RGB, GL_FLOAT, nullptr);

    //============== Delta J (Radiance Scattered) =================
    ghoul::opengl::TextureUnit deltaJTableTextureUnit;
    deltaJTableTextureUnit.activate();
    glGenTextures(1, &_deltaJTableTexture);
    glBindTexture(GL_TEXTURE_3D, _deltaJTableTexture);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage3D(GL_TEXTURE_3D, 0, GL_RGB32F, _mu_s_samples * _nu_samples,
        _mu_samples, _r_samples, 0, GL_RGB, GL_FLOAT, nullptr);

}

void AtmosphereDeferredcaster::deleteComputationTextures() {
    // Cleaning up
    glDeleteTextures(1, &_transmittanceTableTexture);
    glDeleteTextures(1, &_irradianceTableTexture);
    glDeleteTextures(1, &_inScatteringTableTexture);
    glDeleteTextures(1, &_deltaETableTexture);
    glDeleteTextures(1, &_deltaSRayleighTableTexture);
    glDeleteTextures(1, &_deltaSMieTableTexture);
    glDeleteTextures(1, &_deltaJTableTexture);
}

void AtmosphereDeferredcaster::deleteUnusedComputationTextures() {
    glDeleteTextures(1, &_deltaETableTexture);
    glDeleteTextures(1, &_deltaSRayleighTableTexture);
    glDeleteTextures(1, &_deltaSMieTableTexture);
    glDeleteTextures(1, &_deltaJTableTexture);
}

void AtmosphereDeferredcaster::executeCalculations(GLuint quadCalcVAO,
                                                   GLenum drawBuffers[1],
                                                   GLsizei vertexSize)
{
    ghoul::opengl::TextureUnit transmittanceTableTextureUnit;
    ghoul::opengl::TextureUnit irradianceTableTextureUnit;
    ghoul::opengl::TextureUnit inScatteringTableTextureUnit;
    ghoul::opengl::TextureUnit deltaETableTextureUnit;
    ghoul::opengl::TextureUnit deltaSRayleighTableTextureUnit;
    ghoul::opengl::TextureUnit deltaSMieTableTextureUnit;
    ghoul::opengl::TextureUnit deltaJTableTextureUnit;

    // Saving current OpenGL state
    GLboolean blendEnabled = glIsEnabled(GL_BLEND);
    GLenum blendEquationRGB;
    GLenum blendEquationAlpha;
    GLenum blendDestAlpha;
    GLenum blendDestRGB;
    GLenum blendSrcAlpha;
    GLenum blendSrcRGB;

    if (blendEnabled) {
        glDisable(GL_BLEND);
    }
    glGetIntegerv(GL_BLEND_EQUATION_RGB, &blendEquationRGB);
    glGetIntegerv(GL_BLEND_EQUATION_ALPHA, &blendEquationAlpha);
    glGetIntegerv(GL_BLEND_DST_ALPHA, &blendDestAlpha);
    glGetIntegerv(GL_BLEND_DST_RGB, &blendDestRGB);
    glGetIntegerv(GL_BLEND_SRC_ALPHA, &blendSrcAlpha);
    glGetIntegerv(GL_BLEND_SRC_RGB, &blendSrcRGB);

    // ===========================================================
    // See Precomputed Atmosphere Scattering from Bruneton et al. paper, algorithm 4.1:
    // ===========================================================
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        _transmittanceTableTexture,
        0
    );
    checkFrameBufferState("_transmittanceTableTexture");
    glViewport(0, 0, _transmittance_table_width, _transmittance_table_height);
    _transmittanceProgramObject->activate();
    loadAtmosphereDataIntoShaderProgram(_transmittanceProgramObject);
    //glClear(GL_COLOR_BUFFER_BIT);
    static const float black[] = { 0.0f, 0.0f, 0.0f, 0.0f };
    glClearBufferfv(GL_COLOR, 0, black);
    renderQuadForCalc(quadCalcVAO, vertexSize);
    if (_saveCalculationTextures) {
        saveTextureToPPMFile(
            GL_COLOR_ATTACHMENT0,
            std::string("transmittance_texture.ppm"),
            _transmittance_table_width,
            _transmittance_table_height
        );
    }
    _transmittanceProgramObject->deactivate();

    // line 2 in algorithm 4.1
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _deltaETableTexture, 0);
    checkFrameBufferState("_deltaETableTexture");
    glViewport(0, 0, _delta_e_table_width, _delta_e_table_height);
    _irradianceProgramObject->activate();
    transmittanceTableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
    _irradianceProgramObject->setUniform(
        "transmittanceTexture",
        transmittanceTableTextureUnit
    );
    loadAtmosphereDataIntoShaderProgram(_irradianceProgramObject);
    glClear(GL_COLOR_BUFFER_BIT);
    renderQuadForCalc(quadCalcVAO, vertexSize);
    if (_saveCalculationTextures) {
        saveTextureToPPMFile(
            GL_COLOR_ATTACHMENT0,
            std::string("deltaE_table_texture.ppm"),
            _delta_e_table_width,
            _delta_e_table_height
        );
    }
    _irradianceProgramObject->deactivate();

    // line 3 in algorithm 4.1
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        _deltaSRayleighTableTexture,
        0
    );
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT1,
        _deltaSMieTableTexture,
        0
    );
    GLenum colorBuffers[2] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
    glDrawBuffers(2, colorBuffers);
    checkFrameBufferState("_deltaSRay and _deltaSMie TableTexture");
    glViewport(0, 0, _mu_s_samples * _nu_samples, _mu_samples);
    _inScatteringProgramObject->activate();
    transmittanceTableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
    _inScatteringProgramObject->setUniform(
        "transmittanceTexture",
        transmittanceTableTextureUnit
    );
    loadAtmosphereDataIntoShaderProgram(_inScatteringProgramObject);
    glClear(GL_COLOR_BUFFER_BIT);
    for (int layer = 0; layer < _r_samples; ++layer) {
        step3DTexture(_inScatteringProgramObject, layer);
        renderQuadForCalc(quadCalcVAO, vertexSize);
    }
    if (_saveCalculationTextures) {
        saveTextureToPPMFile(
            GL_COLOR_ATTACHMENT0,
            std::string("deltaS_rayleigh_texture.ppm"),
            _mu_s_samples * _nu_samples,
            _mu_samples
        );
        saveTextureToPPMFile(
            GL_COLOR_ATTACHMENT1,
            std::string("deltaS_mie_texture.ppm"),
            _mu_s_samples * _nu_samples,
            _mu_samples
        );
    }
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, 0, 0);
    glDrawBuffers(1, drawBuffers);

    _inScatteringProgramObject->deactivate();

    // line 4 in algorithm 4.1
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        _irradianceTableTexture,
        0
    );
    checkFrameBufferState("_irradianceTableTexture");
    glDrawBuffer(GL_COLOR_ATTACHMENT0);

    glViewport(0, 0, _delta_e_table_width, _delta_e_table_height);
    _deltaEProgramObject->activate();
    //_deltaEProgramObject->setUniform("line", 4);
    deltaETableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D, _deltaETableTexture);
    _deltaEProgramObject->setUniform("deltaETexture", deltaETableTextureUnit);
    loadAtmosphereDataIntoShaderProgram(_deltaEProgramObject);
    glClear(GL_COLOR_BUFFER_BIT);
    renderQuadForCalc(quadCalcVAO, vertexSize);
    if (_saveCalculationTextures) {
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, std::string("irradiance_texture.ppm"),
            _delta_e_table_width, _delta_e_table_height);
    }
    _deltaEProgramObject->deactivate();

    // line 5 in algorithm 4.1
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        _inScatteringTableTexture,
        0
    );
    checkFrameBufferState("_inScatteringTableTexture");
    glViewport(0, 0, _mu_s_samples * _nu_samples, _mu_samples);
    _deltaSProgramObject->activate();
    deltaSRayleighTableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_3D, _deltaSRayleighTableTexture);
    deltaSMieTableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_3D, _deltaSMieTableTexture);
    _deltaSProgramObject->setUniform("deltaSRTexture", deltaSRayleighTableTextureUnit);
    _deltaSProgramObject->setUniform("deltaSMTexture", deltaSMieTableTextureUnit);
    loadAtmosphereDataIntoShaderProgram(_deltaSProgramObject);
    glClear(GL_COLOR_BUFFER_BIT);
    for (int layer = 0; layer < _r_samples; ++layer) {
        step3DTexture(_deltaSProgramObject, layer, false);
        renderQuadForCalc(quadCalcVAO, vertexSize);
    }
    if (_saveCalculationTextures) {
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, std::string("S_texture.ppm"),
            _mu_s_samples * _nu_samples, _mu_samples);
    }
    _deltaSProgramObject->deactivate();

    // loop in line 6 in algorithm 4.1
    for (int scatteringOrder = 2; scatteringOrder <= 4; ++scatteringOrder) {
        // line 7 in algorithm 4.1
        glFramebufferTexture(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            _deltaJTableTexture,
            0
        );
        checkFrameBufferState("_deltaJTableTexture");
        glViewport(0, 0, _mu_s_samples * _nu_samples, _mu_samples);
        _deltaJProgramObject->activate();
        if (scatteringOrder == 2) {
            _deltaJProgramObject->setUniform("firstIteraction", 1);
        }
        else {
            _deltaJProgramObject->setUniform("firstIteraction", 0);
        }
        transmittanceTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
        _deltaJProgramObject->setUniform(
            "transmittanceTexture",
            transmittanceTableTextureUnit
        );
        deltaETableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_2D, _deltaETableTexture);
        _deltaJProgramObject->setUniform("deltaETexture", deltaETableTextureUnit);
        deltaSRayleighTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_3D, _deltaSRayleighTableTexture);
        _deltaJProgramObject->setUniform(
            "deltaSRTexture",
            deltaSRayleighTableTextureUnit
        );
        deltaSMieTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_3D, _deltaSMieTableTexture);
        _deltaJProgramObject->setUniform("deltaSMTexture", deltaSMieTableTextureUnit);
        loadAtmosphereDataIntoShaderProgram(_deltaJProgramObject);
        for (int layer = 0; layer < _r_samples; ++layer) {
            step3DTexture(_deltaJProgramObject, layer);
            renderQuadForCalc(quadCalcVAO, vertexSize);
        }
        if (_saveCalculationTextures) {
            saveTextureToPPMFile(GL_COLOR_ATTACHMENT0,
                fmt::format("deltaJ_texture-scattering_order-{}.ppm", scatteringOrder),
                _mu_s_samples * _nu_samples, _mu_samples);
        }
        _deltaJProgramObject->deactivate();

        // line 8 in algorithm 4.1
        glFramebufferTexture(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            _deltaETableTexture,
            0
        );
        checkFrameBufferState("_deltaETableTexture");
        glViewport(0, 0, _delta_e_table_width, _delta_e_table_height);
        _irradianceSupTermsProgramObject->activate();
        if (scatteringOrder == 2) {
            _irradianceSupTermsProgramObject->setUniform("firstIteraction", 1);
        }
        else {
            _irradianceSupTermsProgramObject->setUniform("firstIteraction", 0);
        }
        transmittanceTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
        _irradianceSupTermsProgramObject->setUniform(
            "transmittanceTexture",
            transmittanceTableTextureUnit
        );
        deltaSRayleighTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_3D, _deltaSRayleighTableTexture);
        _irradianceSupTermsProgramObject->setUniform(
            "deltaSRTexture",
            deltaSRayleighTableTextureUnit
        );
        deltaSMieTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_3D, _deltaSMieTableTexture);
        _irradianceSupTermsProgramObject->setUniform(
            "deltaSMTexture",
            deltaSMieTableTextureUnit
        );
        loadAtmosphereDataIntoShaderProgram(_irradianceSupTermsProgramObject);
        renderQuadForCalc(quadCalcVAO, vertexSize);
        if (_saveCalculationTextures) {
            saveTextureToPPMFile(GL_COLOR_ATTACHMENT0,
                fmt::format("deltaE_texture-scattering_order-{}.ppm", scatteringOrder),
                _delta_e_table_width, _delta_e_table_height);
        }
        _irradianceSupTermsProgramObject->deactivate();

        // line 9 in algorithm 4.1
        glFramebufferTexture(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            _deltaSRayleighTableTexture,
            0
        );
        checkFrameBufferState("_deltaSRayleighTableTexture");
        glViewport(0, 0, _mu_s_samples * _nu_samples, _mu_samples);
        _inScatteringSupTermsProgramObject->activate();
        transmittanceTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
        _inScatteringSupTermsProgramObject->setUniform(
            "transmittanceTexture",
            transmittanceTableTextureUnit
        );
        deltaJTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_3D, _deltaJTableTexture);
        _inScatteringSupTermsProgramObject->setUniform(
            "deltaJTexture",
            deltaJTableTextureUnit
        );
        loadAtmosphereDataIntoShaderProgram(_inScatteringSupTermsProgramObject);
        for (int layer = 0; layer < _r_samples; ++layer) {
            step3DTexture(_inScatteringSupTermsProgramObject, layer);
            renderQuadForCalc(quadCalcVAO, vertexSize);
        }
        if (_saveCalculationTextures) {
            saveTextureToPPMFile(GL_COLOR_ATTACHMENT0,
                fmt::format("deltaS_texture-scattering_order-{}.ppm",
                    scatteringOrder),
                _mu_s_samples * _nu_samples,
                _mu_samples
            );
        }
        _inScatteringSupTermsProgramObject->deactivate();

        glEnable(GL_BLEND);
        glBlendEquationSeparate(GL_FUNC_ADD, GL_FUNC_ADD);
        glBlendFuncSeparate(GL_ONE, GL_ONE, GL_ONE, GL_ONE);

        // line 10 in algorithm 4.1
        glFramebufferTexture(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            _irradianceTableTexture,
            0
        );
        checkFrameBufferState("_irradianceTableTexture");
        glViewport(0, 0, _delta_e_table_width, _delta_e_table_height);
        _irradianceFinalProgramObject->activate();
        deltaETableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_2D, _deltaETableTexture);
        _irradianceFinalProgramObject->setUniform(
            "deltaETexture",
            deltaETableTextureUnit
        );
        loadAtmosphereDataIntoShaderProgram(_irradianceFinalProgramObject);
        renderQuadForCalc(quadCalcVAO, vertexSize);
        if (_saveCalculationTextures) {
            saveTextureToPPMFile(GL_COLOR_ATTACHMENT0,
                fmt::format("irradianceTable_order-{}.ppm",
                    scatteringOrder),
                _delta_e_table_width, _delta_e_table_height);
        }
        _irradianceFinalProgramObject->deactivate();

        // line 11 in algorithm 4.1
        glFramebufferTexture(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            _inScatteringTableTexture,
            0
        );
        checkFrameBufferState("_inScatteringTableTexture");
        glViewport(0, 0, _mu_s_samples * _nu_samples, _mu_samples);
        _deltaSSupTermsProgramObject->activate();
        deltaSRayleighTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_3D, _deltaSRayleighTableTexture);
        _deltaSSupTermsProgramObject->setUniform(
            "deltaSTexture",
            deltaSRayleighTableTextureUnit
        );
        loadAtmosphereDataIntoShaderProgram(_deltaSSupTermsProgramObject);
        for (int layer = 0; layer < _r_samples; ++layer) {
            step3DTexture(_deltaSSupTermsProgramObject, layer, false);
            renderQuadForCalc(quadCalcVAO, vertexSize);
        }
        if (_saveCalculationTextures) {
            saveTextureToPPMFile(GL_COLOR_ATTACHMENT0,
                fmt::format("inscatteringTable_order-{}.ppm",
                    scatteringOrder),
                _mu_s_samples * _nu_samples, _mu_samples);
        }
        _deltaSSupTermsProgramObject->deactivate();

        glDisable(GL_BLEND);
    }

    // Restores OpenGL blending state
    if (blendEnabled)
        glEnable(GL_BLEND);

    glBlendEquationSeparate(blendEquationRGB, blendEquationAlpha);
    glBlendFuncSeparate(blendSrcRGB, blendDestRGB, blendSrcAlpha, blendDestAlpha);
}

void AtmosphereDeferredcaster::preCalculateAtmosphereParam() {
    //==========================================================
    //========= Load Shader Programs for Calculations ==========
    //==========================================================
    loadComputationPrograms();

    //==========================================================
    //============ Create Textures for Calculations ============
    //==========================================================
    createComputationTextures();

    // Saves current FBO first
    GLint defaultFBO;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);

    GLint m_viewport[4];
    glGetIntegerv(GL_VIEWPORT, m_viewport);

    // Creates the FBO for the calculations
    GLuint calcFBO;
    glGenFramebuffers(1, &calcFBO);
    glBindFramebuffer(GL_FRAMEBUFFER, calcFBO);
    GLenum drawBuffers[1] = { GL_COLOR_ATTACHMENT0 };
    glDrawBuffers(1, drawBuffers);

    // Prepare for rendering/calculations
    GLuint quadCalcVAO;
    GLuint quadCalcVBO;
    createRenderQuad(&quadCalcVAO, &quadCalcVBO, 1.0f);

    // Starting Calculations...
    LDEBUG("Starting precalculations for scattering effects...");

    //==========================================================
    //=================== Execute Calculations =================
    //==========================================================
    executeCalculations(quadCalcVAO, drawBuffers, 6);

    deleteUnusedComputationTextures();

    // Restores system state
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    glViewport(
        m_viewport[0],
        m_viewport[1],
        m_viewport[2],
        m_viewport[3]
    );
    glDeleteBuffers(1, &quadCalcVBO);
    glDeleteVertexArrays(1, &quadCalcVAO);
    glDeleteFramebuffers(1, &calcFBO);

    LDEBUG("Ended precalculations for Atmosphere effects...");
}

void AtmosphereDeferredcaster::createRenderQuad(GLuint* vao, GLuint* vbo, GLfloat size) {
    glGenVertexArrays(1, vao);
    glGenBuffers(1, vbo);
    glBindVertexArray(*vao);
    glBindBuffer(GL_ARRAY_BUFFER, *vbo);

    const GLfloat vertex_data[] = {
        //      x      y     z     w
        -size, -size, 0.0f, 1.0f,
        size,    size, 0.0f, 1.0f,
        -size,  size, 0.0f, 1.0f,
        -size, -size, 0.0f, 1.0f,
        size, -size, 0.0f, 1.0f,
        size,    size, 0.0f, 1.0f
    };

    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glVertexAttribPointer(
        0,
        4,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 4,
        nullptr
    );
    glEnableVertexAttribArray(0);

    glBindVertexArray(0);
}

void AtmosphereDeferredcaster::loadAtmosphereDataIntoShaderProgram(
                                std::unique_ptr<ghoul::opengl::ProgramObject>& shaderProg)
{
    shaderProg->setUniform("Rg", _atmospherePlanetRadius);
    shaderProg->setUniform("Rt", _atmosphereRadius);
    shaderProg->setUniform("AverageGroundReflectance", _planetAverageGroundReflectance);
    shaderProg->setUniform("groundRadianceEmittion", _planetGroundRadianceEmittion);
    shaderProg->setUniform("HR", _rayleighHeightScale);
    shaderProg->setUniform("betaRayleigh", _rayleighScatteringCoeff);
    shaderProg->setUniform("HM", _mieHeightScale);
    shaderProg->setUniform("betaMieScattering", _mieScatteringCoeff);
    shaderProg->setUniform("betaMieExtinction", _mieExtinctionCoeff);
    shaderProg->setUniform("mieG", _miePhaseConstant);
    shaderProg->setUniform("sunRadiance", _sunRadianceIntensity);
    shaderProg->setUniform("TRANSMITTANCE_W", _transmittance_table_width);
    shaderProg->setUniform("TRANSMITTANCE_H", _transmittance_table_height);
    shaderProg->setUniform("SKY_W", _irradiance_table_width);
    shaderProg->setUniform("SKY_H", _irradiance_table_height);
    shaderProg->setUniform("OTHER_TEXTURES_W", _delta_e_table_width);
    shaderProg->setUniform("OTHER_TEXTURES_H", _delta_e_table_height);
    shaderProg->setUniform("SAMPLES_R", _r_samples);
    shaderProg->setUniform("SAMPLES_MU", _mu_samples);
    shaderProg->setUniform("SAMPLES_MU_S", _mu_s_samples);
    shaderProg->setUniform("SAMPLES_NU", _nu_samples);
    shaderProg->setUniform("ozoneLayerEnabled", _ozoneEnabled);
    shaderProg->setUniform("HO", _ozoneHeightScale);
    shaderProg->setUniform("betaOzoneExtinction", _ozoneExtinctionCoeff);
}

void AtmosphereDeferredcaster::checkFrameBufferState(
                                                    const std::string& codePosition) const
{
    if (glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Framework not built. " + codePosition);
        GLenum fbErr = glCheckFramebufferStatus(GL_FRAMEBUFFER);
        switch (fbErr) {
            case GL_FRAMEBUFFER_UNDEFINED:
                LERROR("Indefined framebuffer.");
                break;
            case GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT:
                LERROR("Incomplete, missing attachement.");
                break;
            case GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT:
                LERROR("Framebuffer doesn't have at least one image attached to it.");
                break;
            case GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER:
                LERROR(
                    "Returned if the value of GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE is "
                    "GL_NONE for any color attachment point(s) named by GL_DRAW_BUFFERi."
                );
                break;
            case GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER:
                LERROR(
                    "Returned if GL_READ_BUFFER is not GL_NONE and the value of "
                    "GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE is GL_NONE for the color "
                    "attachment point named by GL_READ_BUFFER.");
                break;
            case GL_FRAMEBUFFER_UNSUPPORTED:
                LERROR(
                    "Returned if the combination of internal formats of the attached "
                    "images violates an implementation - dependent set of restrictions."
                );
                break;
            case GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE:
                LERROR(
                    "Returned if the value of GL_RENDERBUFFE_r_samples is not the same "
                    "for all attached renderbuffers; if the value of GL_TEXTURE_SAMPLES "
                    "is the not same for all attached textures; or , if the attached "
                    "images are a mix of renderbuffers and textures, the value of "
                    "GL_RENDERBUFFE_r_samples does not match the value of "
                    "GL_TEXTURE_SAMPLES."
                );
                LERROR(
                    "Returned if the value of GL_TEXTURE_FIXED_SAMPLE_LOCATIONS is not "
                    "the same for all attached textures; or , if the attached images are "
                    "a mix of renderbuffers and textures, the value of "
                    "GL_TEXTURE_FIXED_SAMPLE_LOCATIONS is not GL_TRUE for all attached "
                    "textures."
                );
                break;
            case GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS:
                LERROR(
                    "Returned if any framebuffer attachment is layered, and any "
                    "populated attachment is not layered, or if all populated color "
                    "attachments are not from textures of the same target."
                );
                break;
            default:
                LDEBUG("No error found checking framebuffer: " + codePosition);
                break;
            }
    }
}

void AtmosphereDeferredcaster::renderQuadForCalc(GLuint vao, GLsizei numberOfVertices) {
    glBindVertexArray(vao);
    glDrawArrays(GL_TRIANGLES, 0, numberOfVertices);
    glBindVertexArray(0);
}

void AtmosphereDeferredcaster::step3DTexture(
                                std::unique_ptr<ghoul::opengl::ProgramObject>& shaderProg,
                                                                                int layer,
                                                                       bool doCalculation)
{
    // See OpenGL redbook 8th Edition page 556 for Layered Rendering
    if (doCalculation) {
        float earth2 = _atmospherePlanetRadius * _atmospherePlanetRadius;
        float atm2 = _atmosphereRadius * _atmosphereRadius;
        float diff = atm2 - earth2;
        float ri = static_cast<float>(layer) / static_cast<float>(_r_samples - 1);
        float ri_2 = ri * ri;
        float epsilon =
            (layer == 0) ?
            0.01f :
            (layer == (_r_samples - 1)) ? -0.001f : 0.0f;
        float r = sqrtf(earth2 + ri_2 * diff) + epsilon;
        float dminG = r - _atmospherePlanetRadius;
        float dminT = _atmosphereRadius - r;
        float dh = sqrtf(r * r - earth2);
        float dH = dh + sqrtf(diff);

        shaderProg->setUniform("r", r);
        shaderProg->setUniform("dhdH", dminT, dH, dminG, dh);
    }

    shaderProg->setUniform("layer", static_cast<int>(layer));
}

void AtmosphereDeferredcaster::saveTextureToPPMFile(GLenum color_buffer_attachment,
                                                    const std::string& fileName,
                                                    int width, int height) const
{
    std::fstream ppmFile;

    ppmFile.open(fileName.c_str(), std::fstream::out);
    if (ppmFile.is_open()) {
        unsigned char * pixels = new unsigned char[width*height * 3];
        for (int t = 0; t < width*height * 3; ++t)
            pixels[t] = 255;

        if (color_buffer_attachment != GL_DEPTH_ATTACHMENT) {
            glReadBuffer(color_buffer_attachment);
            glReadPixels(0, 0, width, height, GL_RGB, GL_UNSIGNED_BYTE, pixels);

        }
        else {
            glReadPixels(
                0,
                0,
                width,
                height,
                GL_DEPTH_COMPONENT,
                GL_UNSIGNED_BYTE,
                pixels
            );
        }

        ppmFile << "P3" << std::endl;
        ppmFile << width << " " << height << std::endl;
        ppmFile << "255" << std::endl;

        std::cout << "\n\nFILE\n\n";
        int k = 0;
        for (int i = 0; i < width; i++) {
            for (int j = 0; j < height; j++) {
                ppmFile << static_cast<unsigned int>(pixels[k]) << " "
                    << static_cast<unsigned int>(pixels[k + 1]) << " "
                    << static_cast<unsigned int>(pixels[k + 2]) << " ";
                k += 3;
            }
            ppmFile << std::endl;
        }
        delete[] pixels;

        ppmFile.close();
    }
}

bool AtmosphereDeferredcaster::isAtmosphereInFrustum(const glm::dmat4& MVMatrix,
                                                     const glm::dvec3& position,
                                                     double radius) const
{

    // Frustum Planes
    //glm::dvec3 col1(MVMatrix[0], MVMatrix[4], MVMatrix[8]);
    //glm::dvec3 col2(MVMatrix[1], MVMatrix[5], MVMatrix[9]);
    //glm::dvec3 col3(MVMatrix[2], MVMatrix[6], MVMatrix[10]);
    //glm::dvec3 col4(MVMatrix[3], MVMatrix[7], MVMatrix[11]);

    glm::dvec3 col1(MVMatrix[0][0], MVMatrix[1][0], MVMatrix[2][0]);
    glm::dvec3 col2(MVMatrix[0][1], MVMatrix[1][1], MVMatrix[2][1]);
    glm::dvec3 col3(MVMatrix[0][2], MVMatrix[1][2], MVMatrix[2][2]);
    glm::dvec3 col4(MVMatrix[0][3], MVMatrix[1][3], MVMatrix[2][3]);

    glm::dvec3 leftNormal = col4 + col1;
    glm::dvec3 rightNormal = col4 - col1;
    glm::dvec3 bottomNormal = col4 + col2;
    glm::dvec3 topNormal = col4 - col2;
    glm::dvec3 nearNormal = col3 + col4;
    glm::dvec3 farNormal = col4 - col3;

    // Plane Distances
    //double leftDistance = MVMatrix[15] + MVMatrix[12];
    //double rightDistance = MVMatrix[15] - MVMatrix[12];
    //double bottomDistance = MVMatrix[15] + MVMatrix[13];
    //double topDistance = MVMatrix[15] - MVMatrix[13];
    //double nearDistance = MVMatrix[15] + MVMatrix[14];
    //double farDistance = MVMatrix[15] - MVMatrix[14];

    double leftDistance = MVMatrix[3][3] + MVMatrix[3][0];
    double rightDistance = MVMatrix[3][3] - MVMatrix[3][0];
    double bottomDistance = MVMatrix[3][3] + MVMatrix[3][1];
    double topDistance = MVMatrix[3][3] - MVMatrix[3][1];
    double nearDistance = MVMatrix[3][3] + MVMatrix[3][2];
    double farDistance = MVMatrix[3][3] - MVMatrix[3][2];

    // Normalize Planes
    double invMag = 1.0 / glm::length(leftNormal);
    leftNormal *= invMag;
    leftDistance *= invMag;

    invMag = 1.0 / glm::length(rightNormal);
    rightNormal *= invMag;
    rightDistance *= invMag;

    invMag = 1.0 / glm::length(bottomNormal);
    bottomNormal *= invMag;
    bottomDistance *= invMag;

    invMag = 1.0 / glm::length(topNormal);
    topNormal *= invMag;
    topDistance *= invMag;

    invMag = 1.0 / glm::length(nearNormal);
    nearNormal *= invMag;
    nearDistance *= invMag;

    invMag = 1.0 / glm::length(farNormal);
    farNormal *= invMag;
    farDistance *= invMag;

    if ((glm::dot(leftNormal, position) + leftDistance) < -radius) {
        return false;
    }
    else if ((glm::dot(rightNormal, position) + rightDistance) < -radius) {
        return false;
    }
    else if ((glm::dot(bottomNormal, position) + bottomDistance) < -radius) {
        return false;
    }
    else if ((glm::dot(topNormal, position) + topDistance) < -radius) {
        return false;
    }
    else if ((glm::dot(nearNormal, position) + nearDistance) < -radius) {
        return false;
    }
    // The far plane testing is disabled because the atm has no depth.
    /*else if ((glm::dot(farNormal, position) + farDistance) < -radius) {
    return false;
    }*/

    return true;
}

} // namespace openspace
