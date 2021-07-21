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

 /***************************************************************************************
 * Modified part of the code (4D texture mechanism) from Eric Bruneton is used in the
 * following code.
 ****************************************************************************************/

/**
 * Precomputed Atmospheric Scattering
 * Copyright (c) 2008 INRIA
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are
 * permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice, this list of
 *    conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list
 *    of conditions and the following disclaimer in the documentation and/or other
 *    materials provided with the distribution.
 * 3. Neither the name of the copyright holders nor the names of its contributors may be
 *    used to endorse or promote products derived from this software without specific
 *    prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
 * THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <modules/atmosphere/rendering/atmospheredeferredcaster.h>

#include <openspace/engine/globals.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <cmath>
#include <fstream>

namespace {
    constexpr const char* _loggerCat = "AtmosphereDeferredcaster";

    constexpr const std::array<const char*, 27> UniformNames = {
        "cullAtmosphere", "Rg", "Rt", "groundRadianceEmission", "HR", "betaRayleigh",
        "HM", "betaMieExtinction", "mieG", "sunRadiance", "ozoneLayerEnabled", "HO",
        "betaOzoneExtinction", "SAMPLES_R", "SAMPLES_MU", "SAMPLES_MU_S", "SAMPLES_NU",
        "inverseModelTransformMatrix", "modelTransformMatrix",
        "projectionToModelTransformMatrix", "viewToWorldMatrix", "camPosObj",
        "sunDirectionObj", "hardShadows", "transmittanceTexture", "irradianceTexture",
        "inscatterTexture"
    };

    constexpr const float ATM_EPS = 2000.f;
    constexpr const float KM_TO_M = 1000.f;

    void createRenderQuad(GLuint* vao, GLuint* vbo) {
        glGenVertexArrays(1, vao);
        glBindVertexArray(*vao);
        glGenBuffers(1, vbo);
        glBindBuffer(GL_ARRAY_BUFFER, *vbo);

        const GLfloat VertexData[] = {
            //  x      y    z
            -1.f, -1.f,
             1.f,  1.f,
            -1.f,  1.f,
            -1.f, -1.f,
             1.f, -1.f,
             1.f,  1.f,
        };

        glBufferData(GL_ARRAY_BUFFER, sizeof(VertexData), VertexData, GL_STATIC_DRAW);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(GLfloat), nullptr);
        glBindVertexArray(0);
    }

    void saveTextureFile(GLenum colorBufferAttachment,
                         const std::filesystem::path& fileName, const glm::ivec2& size)
    {
        std::fstream ppmFile;
        ppmFile.open(fileName, std::fstream::out);
        if (!ppmFile.is_open()) {
            return;
        }

        std::vector<unsigned char> px(
            size.x * size.y * 3,
            static_cast<unsigned char>(255)
        );

        if (colorBufferAttachment != GL_DEPTH_ATTACHMENT) {
            glReadBuffer(colorBufferAttachment);
            glReadPixels(0, 0, size.x, size.y, GL_RGB, GL_UNSIGNED_BYTE, px.data());
        }
        else {
            glReadPixels(
                0,
                0,
                size.x,
                size.y,
                GL_DEPTH_COMPONENT,
                GL_UNSIGNED_BYTE,
                px.data()
            );
        }

        ppmFile << "P3" << '\n' << size.x << " " << size.y << '\n' << "255" << '\n';

        int k = 0;
        for (int i = 0; i < size.x; i++) {
            for (int j = 0; j < size.y; j++) {
                ppmFile << static_cast<unsigned int>(px[k]) << ' '
                    << static_cast<unsigned int>(px[k + 1]) << ' '
                    << static_cast<unsigned int>(px[k + 2]) << ' ';
                k += 3;
            }
            ppmFile << '\n';
        }
    }

    bool isAtmosphereInFrustum(const glm::dmat4& MVMatrix, const glm::dvec3& position,
                               double radius)
    {
        // Frustum Planes
        glm::dvec3 col1 = glm::dvec3(MVMatrix[0][0], MVMatrix[1][0], MVMatrix[2][0]);
        glm::dvec3 col2 = glm::dvec3(MVMatrix[0][1], MVMatrix[1][1], MVMatrix[2][1]);
        glm::dvec3 col3 = glm::dvec3(MVMatrix[0][2], MVMatrix[1][2], MVMatrix[2][2]);
        glm::dvec3 col4 = glm::dvec3(MVMatrix[0][3], MVMatrix[1][3], MVMatrix[2][3]);

        glm::dvec3 leftNormal = col4 + col1;
        glm::dvec3 rightNormal = col4 - col1;
        glm::dvec3 bottomNormal = col4 + col2;
        glm::dvec3 topNormal = col4 - col2;
        glm::dvec3 nearNormal = col3 + col4;
        glm::dvec3 farNormal = col4 - col3;

        // Plane Distances
        double leftDistance = MVMatrix[3][3] + MVMatrix[3][0];
        double rightDistance = MVMatrix[3][3] - MVMatrix[3][0];
        double bottomDistance = MVMatrix[3][3] + MVMatrix[3][1];
        double topDistance = MVMatrix[3][3] - MVMatrix[3][1];
        double nearDistance = MVMatrix[3][3] + MVMatrix[3][2];

        // Normalize Planes
        const double invLeftMag = 1.0 / glm::length(leftNormal);
        leftNormal *= invLeftMag;
        leftDistance *= invLeftMag;

        const double invRightMag = 1.0 / glm::length(rightNormal);
        rightNormal *= invRightMag;
        rightDistance *= invRightMag;

        const double invBottomMag = 1.0 / glm::length(bottomNormal);
        bottomNormal *= invBottomMag;
        bottomDistance *= invBottomMag;

        const double invTopMag = 1.0 / glm::length(topNormal);
        topNormal *= invTopMag;
        topDistance *= invTopMag;

        const double invNearMag = 1.0 / glm::length(nearNormal);
        nearNormal *= invNearMag;
        nearDistance *= invNearMag;

        const double invFarMag = 1.0 / glm::length(farNormal);
        farNormal *= invFarMag;

        if (((glm::dot(leftNormal, position) + leftDistance) < -radius) ||
            ((glm::dot(rightNormal, position) + rightDistance) < -radius) ||
            ((glm::dot(bottomNormal, position) + bottomDistance) < -radius) ||
            ((glm::dot(topNormal, position) + topDistance) < -radius) ||
            ((glm::dot(nearNormal, position) + nearDistance) < -radius))
            // The far plane testing is disabled because the atm has no depth.
        {
            return false;
        }
        return true;
    }

    void renderQuadForCalc(GLuint vao, GLsizei numberOfVertices) {
        glBindVertexArray(vao);
        glDrawArrays(GL_TRIANGLES, 0, numberOfVertices);
        glBindVertexArray(0);
    }
} // namespace

namespace openspace {

void AtmosphereDeferredcaster::initialize() {
    ZoneScoped

    if (!_atmosphereCalculated) {
        preCalculateAtmosphereParam();
    }

    std::memset(_uniformNameBuffer, 0, sizeof(_uniformNameBuffer));
    std::strcpy(_uniformNameBuffer, "shadowDataArray[");
}

void AtmosphereDeferredcaster::deinitialize() {
    ZoneScoped

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
}

void AtmosphereDeferredcaster::preRaycast(const RenderData& renderData,
                                          const DeferredcastData&,
                                          ghoul::opengl::ProgramObject& program)
{
    ZoneScoped

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
        glm::dmat3(_modelTransform) * glm::dvec3(KM_TO_M * _atmosphereRadius, 0.0, 0.0)
    );

    // Number of planet radii to use as distance threshold for culling
    const double DISTANCE_CULLING_RADII = 5000;
    if (distance > scaledRadius * DISTANCE_CULLING_RADII) {
        program.setUniform(_uniformCache.cullAtmosphere, 1);
    }
    else {
        glm::dmat4 MV = glm::dmat4(renderData.camera.sgctInternal.projectionMatrix()) *
                        renderData.camera.combinedViewMatrix();

        const double totalAtmosphere = (scaledRadius + ATM_EPS);
        if (!isAtmosphereInFrustum(MV, tPlanetPosWorld, totalAtmosphere)) {
            program.setUniform(_uniformCache.cullAtmosphere, 1);
        }
        else {
            program.setUniform(_uniformCache.cullAtmosphere, 0);
            program.setUniform(_uniformCache.Rg, _atmospherePlanetRadius);
            program.setUniform(_uniformCache.Rt, _atmosphereRadius);
            program.setUniform(
                _uniformCache.groundRadianceEmission,
                _planetGroundRadianceEmission
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
                _uniformCache.inverseModelTransformMatrix, inverseModelMatrix
            );
            program.setUniform(_uniformCache.modelTransformMatrix, _modelTransform);

            glm::dmat4 viewToWorldMatrix = glm::inverse(
                renderData.camera.combinedViewMatrix()
            );

            // Eye Space to World Space
            program.setUniform(_uniformCache.viewToWorldMatrix, viewToWorldMatrix);

            // Projection to Eye Space
            glm::dmat4 dInverseProjection = glm::inverse(
                glm::dmat4(renderData.camera.projectionMatrix())
            );

            glm::dmat4 inverseWholeMatrixPipeline =
                inverseModelMatrix * viewToWorldMatrix * dInverseProjection;

            program.setUniform(
                _uniformCache.projectionToModelTransformMatrix,
                inverseWholeMatrixPipeline
            );

            glm::dvec4 camPosObjCoords =
                inverseModelMatrix * glm::dvec4(renderData.camera.eyePositionVec3(), 1.0);
            program.setUniform(_uniformCache.camPosObj, glm::dvec3(camPosObjCoords));

            SceneGraphNode* node = sceneGraph()->sceneGraphNode("Sun");
            glm::dvec3 sunPosWorld = node ? node->worldPosition() : glm::dvec3(0.0);

            glm::dvec4 sunPosObj;
            // Sun following camera position
            if (_sunFollowingCameraEnabled) {
                sunPosObj = inverseModelMatrix * glm::dvec4(
                    renderData.camera.eyePositionVec3(),
                    1.0
                );
            }
            else {
                sunPosObj = inverseModelMatrix *
                    glm::dvec4(
                        (sunPosWorld - renderData.modelTransform.translation) * 1000.0,
                        1.0
                    );
            }

            // Sun Position in Object Space
            program.setUniform(
                _uniformCache.sunDirectionObj,
                glm::normalize(glm::dvec3(sunPosObj))
            );

            ghoul::opengl::updateUniformLocations(program, _uniformCache, UniformNames);

            // Shadow calculations..
            if (!_shadowConfArray.empty()) {
                ZoneScopedN("Shadow Configuration")

                _shadowDataArrayCache.clear();

                for (const ShadowConfiguration& shadowConf : _shadowConfArray) {
                    // TO REMEMBER: all distances and lengths in world coordinates are in
                    // meters!!! We need to move this to view space...
                    // Getting source and caster:
                    double lt;
                    glm::dvec3 sourcePos = SpiceManager::ref().targetPosition(
                        shadowConf.source.first,
                        "SSB",
                        "GALACTIC",
                        {},
                        _time,
                        lt
                    );
                    sourcePos *= KM_TO_M; // converting to meters
                    glm::dvec3 casterPos = SpiceManager::ref().targetPosition(
                        shadowConf.caster.first,
                        "SSB",
                        "GALACTIC",
                        {},
                        _time,
                        lt
                    );
                    casterPos *= KM_TO_M; // converting to meters

                    SceneGraphNode* sourceNode = sceneGraphNode(shadowConf.source.first);
                    SceneGraphNode* casterNode = sceneGraphNode(shadowConf.caster.first);

                    if ((sourceNode == nullptr) || (casterNode == nullptr)) {
                        LERRORC(
                            "AtmosphereDeferredcaster",
                            "Invalid scenegraph node for the shadow's caster or shadow's "
                            "receiver"
                        );
                        return;
                    }

                    const double sourceRadiusScale = std::max(
                        glm::compMax(sourceNode->scale()),
                        1.0
                    );

                    const double casterRadiusScale = std::max(
                        glm::compMax(casterNode->scale()),
                        1.0
                    );

                    // First we determine if the caster is shadowing the current planet
                    // (all calculations in World Coordinates):
                    glm::dvec3 planetCasterVec =
                        casterPos - renderData.modelTransform.translation;
                    glm::dvec3 sourceCasterVec = casterPos - sourcePos;
                    double sc_length = glm::length(sourceCasterVec);
                    glm::dvec3 planetCaster_proj = (
                        glm::dot(planetCasterVec, sourceCasterVec) /
                        (sc_length*sc_length)) * sourceCasterVec;
                    double d_test = glm::length(planetCasterVec - planetCaster_proj);
                    double xp_test = shadowConf.caster.second * casterRadiusScale *
                        sc_length /
                        (shadowConf.source.second * sourceRadiusScale +
                         shadowConf.caster.second * casterRadiusScale);
                    double rp_test = shadowConf.caster.second * casterRadiusScale *
                        (glm::length(planetCaster_proj) + xp_test) / xp_test;

                    double casterDistSun = glm::length(casterPos - sunPosWorld);
                    double planetDistSun = glm::length(
                        renderData.modelTransform.translation - sunPosWorld
                    );

                    ShadowRenderingStruct shadowData;
                    shadowData.isShadowing = false;

                    if (((d_test - rp_test) < (_atmospherePlanetRadius * KM_TO_M)) &&
                        (casterDistSun < planetDistSun))
                    {
                        // The current caster is shadowing the current planet
                        shadowData.isShadowing = true;
                        shadowData.rs = shadowConf.source.second * sourceRadiusScale;
                        shadowData.rc = shadowConf.caster.second * casterRadiusScale;
                        shadowData.sourceCasterVec = glm::normalize(sourceCasterVec);
                        shadowData.xp = xp_test;
                        shadowData.xu =
                            shadowData.rc * sc_length / (shadowData.rs - shadowData.rc);
                        shadowData.casterPositionVec = casterPos;
                    }
                    _shadowDataArrayCache.push_back(shadowData);
                }

                // _uniformNameBuffer[0..15] = "shadowDataArray["
                unsigned int counter = 0;
                for (const ShadowRenderingStruct& sd : _shadowDataArrayCache) {
                    // Add the counter
                    char* bf = fmt::format_to(_uniformNameBuffer + 16, "{}", counter);

                    std::strcpy(bf, "].isShadowing\0");
                    program.setUniform(_uniformNameBuffer, sd.isShadowing);

                    if (sd.isShadowing) {
                        std::strcpy(bf, "].xp\0");
                        program.setUniform(_uniformNameBuffer, sd.xp);
                        std::strcpy(bf, "].xu\0");
                        program.setUniform(_uniformNameBuffer, sd.xu);
                        std::strcpy(bf, "].rc\0");
                        program.setUniform(_uniformNameBuffer, sd.rc);
                        std::strcpy(bf, "].sourceCasterVec\0");
                        program.setUniform(_uniformNameBuffer, sd.sourceCasterVec);
                        std::strcpy(bf, "].casterPositionVec\0");
                        program.setUniform(_uniformNameBuffer, sd.casterPositionVec);
                    }
                    counter++;
                }
                program.setUniform(_uniformCache.hardShadows, _hardShadowsEnabled);
            }
        }
    }
    _transmittanceTableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
    program.setUniform(
        _uniformCache.transmittanceTexture,
        _transmittanceTableTextureUnit
    );

    _irradianceTableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D, _irradianceTableTexture);
    program.setUniform(_uniformCache.irradianceTexture, _irradianceTableTextureUnit);

    _inScatteringTableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_3D, _inScatteringTableTexture);
    program.setUniform(_uniformCache.inscatterTexture, _inScatteringTableTextureUnit);
}

void AtmosphereDeferredcaster::postRaycast(const RenderData&, const DeferredcastData&,
                                           ghoul::opengl::ProgramObject&)
{
    ZoneScoped

    // Deactivate the texture units
    _transmittanceTableTextureUnit.deactivate();
    _irradianceTableTextureUnit.deactivate();
    _inScatteringTableTextureUnit.deactivate();
}

std::filesystem::path AtmosphereDeferredcaster::deferredcastPath() const {
    return absPath("${MODULE_ATMOSPHERE}/shaders/atmosphere_deferred_fs.glsl");
}

std::filesystem::path AtmosphereDeferredcaster::deferredcastFSPath() const {
    return absPath("${MODULE_ATMOSPHERE}/shaders/atmosphere_deferred_fs.glsl");
}

std::filesystem::path AtmosphereDeferredcaster::deferredcastVSPath() const {
    return absPath("${MODULE_ATMOSPHERE}/shaders/atmosphere_deferred_vs.glsl");
}

std::filesystem::path AtmosphereDeferredcaster::helperPath() const {
    return ""; // no helper file
}

void AtmosphereDeferredcaster::initializeCachedVariables(
                                                    ghoul::opengl::ProgramObject& program)
{
    ghoul::opengl::updateUniformLocations(program, _uniformCache, UniformNames);
}

void AtmosphereDeferredcaster::update(const UpdateData&) {}

void AtmosphereDeferredcaster::setModelTransform(glm::dmat4 transform) {
    _modelTransform = std::move(transform);
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

void AtmosphereDeferredcaster::setPlanetGroundRadianceEmission(
                                                             float groundRadianceEmission)
{
    _planetGroundRadianceEmission = groundRadianceEmission;
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

void AtmosphereDeferredcaster::setRayleighScatteringCoefficients(glm::vec3 rayScattCoeff)
{
    _rayleighScatteringCoeff = std::move(rayScattCoeff);
}

void AtmosphereDeferredcaster::setOzoneExtinctionCoefficients(glm::vec3 ozoneExtCoeff) {
    _ozoneExtinctionCoeff = std::move(ozoneExtCoeff);
}

void AtmosphereDeferredcaster::setMieScatteringCoefficients(glm::vec3 mieScattCoeff) {
    _mieScatteringCoeff = std::move(mieScattCoeff);
}

void AtmosphereDeferredcaster::setMieExtinctionCoefficients(glm::vec3 mieExtCoeff) {
    _mieExtinctionCoeff = std::move(mieExtCoeff);
}

void AtmosphereDeferredcaster::setEllipsoidRadii(glm::dvec3 radii) {
    _ellipsoidRadii = std::move(radii);
}

void AtmosphereDeferredcaster::setHardShadows(bool enabled) {
    _hardShadowsEnabled = enabled;
}

void AtmosphereDeferredcaster::setShadowConfigArray(
                                       std::vector<ShadowConfiguration> shadowConfigArray)
{
    _shadowConfArray = std::move(shadowConfigArray);

    _shadowDataArrayCache.clear();
    _shadowDataArrayCache.reserve(_shadowConfArray.size());
}

void AtmosphereDeferredcaster::enableSunFollowing(bool enable) {
    _sunFollowingCameraEnabled = enable;
}

void AtmosphereDeferredcaster::setPrecalculationTextureScale(
                                                         float preCalculatedTexturesScale)
{
    _transmittanceTableSize *= static_cast<unsigned int>(preCalculatedTexturesScale);
    _irradianceTableSize *= static_cast<unsigned int>(preCalculatedTexturesScale);
    _deltaETableSize *= static_cast<unsigned int>(preCalculatedTexturesScale);
    _r_samples *= static_cast<unsigned int>(preCalculatedTexturesScale);
    _mu_samples *= static_cast<unsigned int>(preCalculatedTexturesScale);
    _mu_s_samples *= static_cast<unsigned int>(preCalculatedTexturesScale);
    _nu_samples *= static_cast<unsigned int>(preCalculatedTexturesScale);
}

void AtmosphereDeferredcaster::enablePrecalculationTexturesSaving() {
    _saveCalculationTextures = true;
}

void AtmosphereDeferredcaster::loadComputationPrograms() {
    // Transmittance T
    if (!_transmittanceProgramObject) {
        _transmittanceProgramObject = ghoul::opengl::ProgramObject::Build(
            "transmittanceCalcProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/transmittance_calc_fs.glsl")
        );
    }
    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;

    // Irradiance E
    if (!_irradianceProgramObject) {
        _irradianceProgramObject = ghoul::opengl::ProgramObject::Build(
            "irradianceCalcProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/irradiance_calc_fs.glsl")
        );
    }

    if (!_irradianceSupTermsProgramObject) {
        _irradianceSupTermsProgramObject = ghoul::opengl::ProgramObject::Build(
            "irradianceSupTermsCalcProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/irradiance_sup_calc_fs.glsl")
        );
    }

    // InScattering S
    if (!_inScatteringProgramObject) {
        _inScatteringProgramObject = ghoul::opengl::ProgramObject::Build(
            "inScatteringCalcProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/inScattering_calc_fs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/calculation_gs.glsl")
        );
    }

    if (!_inScatteringSupTermsProgramObject) {
        _inScatteringSupTermsProgramObject = ghoul::opengl::ProgramObject::Build(
            "inScatteringSupTermsCalcProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/inScattering_sup_calc_fs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/calculation_gs.glsl")
        );
    }

    // Delta E
    if (!_deltaEProgramObject) {
        _deltaEProgramObject = ghoul::opengl::ProgramObject::Build(
            "deltaECalcProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/deltaE_calc_fs.glsl")
        );
    }

    // Irradiance finel E
    if (!_irradianceFinalProgramObject) {
        _irradianceFinalProgramObject = ghoul::opengl::ProgramObject::Build(
            "irradianceEFinalProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/irradiance_final_fs.glsl")
        );
    }

    // Delta S
    if (!_deltaSProgramObject) {
        _deltaSProgramObject = ghoul::opengl::ProgramObject::Build(
            "deltaSCalcProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/deltaS_calc_fs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/calculation_gs.glsl")
        );
    }

    if (!_deltaSSupTermsProgramObject) {
        _deltaSSupTermsProgramObject = ghoul::opengl::ProgramObject::Build(
            "deltaSSUPTermsCalcProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/deltaS_sup_calc_fs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/calculation_gs.glsl")
        );
    }

    // Delta J (Radiance Scattered)
    if (!_deltaJProgramObject) {
        _deltaJProgramObject = ghoul::opengl::ProgramObject::Build(
            "deltaJCalcProgram",
            absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/deltaJ_calc_fs.glsl"),
            absPath("${MODULE_ATMOSPHERE}/shaders/calculation_gs.glsl")
        );
    }
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
        // Transmittance
        glGenTextures(1, &_transmittanceTableTexture);
        glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        // Stopped using a buffer object for GL_PIXEL_UNPACK_BUFFER
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
        glTexImage2D(
            GL_TEXTURE_2D,
            0,
            GL_RGB32F,
            _transmittanceTableSize.x,
            _transmittanceTableSize.y,
            0,
            GL_RGB,
            GL_FLOAT,
            nullptr
        );
        if (glbinding::Binding::ObjectLabel.isResolved()) {
            glObjectLabel(
                GL_TEXTURE,
                _transmittanceTableTexture,
                -1,
                "Transmittance Table"
            );
        }

        // Irradiance
        glGenTextures(1, &_irradianceTableTexture);
        glBindTexture(GL_TEXTURE_2D, _irradianceTableTexture);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
        glTexImage2D(
            GL_TEXTURE_2D,
            0,
            GL_RGB32F,
            _irradianceTableSize.x,
            _irradianceTableSize.y,
            0,
            GL_RGB,
            GL_FLOAT,
            nullptr
        );
        if (glbinding::Binding::ObjectLabel.isResolved()) {
            glObjectLabel(
                GL_TEXTURE,
                _irradianceTableTexture,
                -1,
                "Irradiance Table"
            );
        }

        //
        // InScattering
        glGenTextures(1, &_inScatteringTableTexture);
        glBindTexture(GL_TEXTURE_3D, _inScatteringTableTexture);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
        glTexImage3D(
            GL_TEXTURE_3D,
            0,
            GL_RGBA32F,
            _mu_s_samples * _nu_samples,
            _mu_samples,
            _r_samples,
            0,
            GL_RGB,
            GL_FLOAT,
            nullptr
        );
        if (glbinding::Binding::ObjectLabel.isResolved()) {
            glObjectLabel(
                GL_TEXTURE,
                _inScatteringTableTexture,
                -1,
                "InScattering Table"
            );
        }
    }

    // Delta E
    glGenTextures(1, &_deltaETableTexture);
    glBindTexture(GL_TEXTURE_2D, _deltaETableTexture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGB32F,
        _deltaETableSize.x,
        _deltaETableSize.y,
        0,
        GL_RGB,
        GL_FLOAT,
        nullptr
    );
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(GL_TEXTURE, _deltaETableTexture, -1, "DeltaE Table");
    }

    // Delta S
    glGenTextures(1, &_deltaSRayleighTableTexture);
    glBindTexture(GL_TEXTURE_3D, _deltaSRayleighTableTexture);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage3D(
        GL_TEXTURE_3D,
        0,
        GL_RGB32F,
        _mu_s_samples * _nu_samples,
        _mu_samples,
        _r_samples,
        0,
        GL_RGB,
        GL_FLOAT,
        nullptr
    );
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(
            GL_TEXTURE,
            _deltaSRayleighTableTexture,
            -1,
            "DeltaS Rayleigh Table"
        );
    }

    glGenTextures(1, &_deltaSMieTableTexture);
    glBindTexture(GL_TEXTURE_3D, _deltaSMieTableTexture);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage3D(
        GL_TEXTURE_3D,
        0,
        GL_RGB32F,
        _mu_s_samples * _nu_samples,
        _mu_samples,
        _r_samples,
        0,
        GL_RGB,
        GL_FLOAT,
        nullptr
    );
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(
            GL_TEXTURE,
            _deltaSMieTableTexture,
            -1,
            "DeltaS Mie Table"
        );
    }

    // Delta J (Radiance Scattered)
    glGenTextures(1, &_deltaJTableTexture);
    glBindTexture(GL_TEXTURE_3D, _deltaJTableTexture);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage3D(
        GL_TEXTURE_3D,
        0,
        GL_RGB32F,
        _mu_s_samples * _nu_samples,
        _mu_samples,
        _r_samples,
        0,
        GL_RGB,
        GL_FLOAT,
        nullptr
    );
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(GL_TEXTURE, _deltaJTableTexture, -1, "DeltaJ Table");
    }
}

void AtmosphereDeferredcaster::deleteComputationTextures() {
    glDeleteTextures(1, &_transmittanceTableTexture);
    _transmittanceTableTexture = 0;
    glDeleteTextures(1, &_irradianceTableTexture);
    _irradianceTableTexture = 0;
    glDeleteTextures(1, &_inScatteringTableTexture);
    _inScatteringTableTexture = 0;
    glDeleteTextures(1, &_deltaETableTexture);
    _deltaETableTexture = 0;
    glDeleteTextures(1, &_deltaSRayleighTableTexture);
    _deltaSRayleighTableTexture = 0;
    glDeleteTextures(1, &_deltaSMieTableTexture);
    _deltaSMieTableTexture = 0;
    glDeleteTextures(1, &_deltaJTableTexture);
    _deltaJTableTexture = 0;
}

void AtmosphereDeferredcaster::deleteUnusedComputationTextures() {
    glDeleteTextures(1, &_deltaETableTexture);
    _deltaETableTexture = 0;
    glDeleteTextures(1, &_deltaSRayleighTableTexture);
    _deltaSRayleighTableTexture = 0;
    glDeleteTextures(1, &_deltaSMieTableTexture);
    _deltaSMieTableTexture = 0;
    glDeleteTextures(1, &_deltaJTableTexture);
    _deltaJTableTexture = 0;
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

    glDisable(GL_BLEND);

    // See Precomputed Atmosphere Scattering from Bruneton et al. paper, algorithm 4.1:
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        _transmittanceTableTexture,
        0
    );
    glViewport(0, 0, _transmittanceTableSize.x, _transmittanceTableSize.y);
    _transmittanceProgramObject->activate();
    _transmittanceProgramObject->setUniform("Rg", _atmospherePlanetRadius);
    _transmittanceProgramObject->setUniform("Rt", _atmosphereRadius);
    _transmittanceProgramObject->setUniform("HR", _rayleighHeightScale);
    _transmittanceProgramObject->setUniform("betaRayleigh", _rayleighScatteringCoeff);
    _transmittanceProgramObject->setUniform("HM", _mieHeightScale);
    _transmittanceProgramObject->setUniform("betaMieExtinction", _mieExtinctionCoeff);
    _transmittanceProgramObject->setUniform("TRANSMITTANCE", _transmittanceTableSize);
    _transmittanceProgramObject->setUniform("ozoneLayerEnabled", _ozoneEnabled);
    _transmittanceProgramObject->setUniform("HO", _ozoneHeightScale);
    _transmittanceProgramObject->setUniform("betaOzoneExtinction", _ozoneExtinctionCoeff);

    static const float Black[] = { 0.f, 0.f, 0.f, 0.f };
    glClearBufferfv(GL_COLOR, 0, Black);
    renderQuadForCalc(quadCalcVAO, vertexSize);
    if (_saveCalculationTextures) {
        saveTextureFile(
            GL_COLOR_ATTACHMENT0,
            "transmittance_texture.ppm",
            _transmittanceTableSize
        );
    }
    _transmittanceProgramObject->deactivate();

    // line 2 in algorithm 4.1
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _deltaETableTexture, 0);
    glViewport(0, 0, _deltaETableSize.x, _deltaETableSize.y);
    _irradianceProgramObject->activate();
    transmittanceTableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
    _irradianceProgramObject->setUniform(
        "transmittanceTexture",
        transmittanceTableTextureUnit
    );
    _irradianceProgramObject->setUniform("Rg", _atmospherePlanetRadius);
    _irradianceProgramObject->setUniform("Rt", _atmosphereRadius);
    _irradianceProgramObject->setUniform("OTHER_TEXTURES", _deltaETableSize);
    glClear(GL_COLOR_BUFFER_BIT);
    renderQuadForCalc(quadCalcVAO, vertexSize);
    if (_saveCalculationTextures) {
        saveTextureFile(
            GL_COLOR_ATTACHMENT0,
            "deltaE_table_texture.ppm",
            _deltaETableSize
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
    glViewport(0, 0, _mu_s_samples * _nu_samples, _mu_samples);
    _inScatteringProgramObject->activate();
    transmittanceTableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
    _inScatteringProgramObject->setUniform(
        "transmittanceTexture",
        transmittanceTableTextureUnit
    );
    _inScatteringProgramObject->setUniform("Rg", _atmospherePlanetRadius);
    _inScatteringProgramObject->setUniform("Rt", _atmosphereRadius);
    _inScatteringProgramObject->setUniform("HR", _rayleighHeightScale);
    _inScatteringProgramObject->setUniform("betaRayleigh", _rayleighScatteringCoeff);
    _inScatteringProgramObject->setUniform("HM", _mieHeightScale);
    _inScatteringProgramObject->setUniform("betaMieScattering", _mieScatteringCoeff);
    _inScatteringProgramObject->setUniform("SAMPLES_MU", _mu_samples);
    _inScatteringProgramObject->setUniform("SAMPLES_MU_S", _mu_s_samples);
    _inScatteringProgramObject->setUniform("SAMPLES_NU", _nu_samples);
    _inScatteringProgramObject->setUniform("ozoneLayerEnabled", _ozoneEnabled);
    _inScatteringProgramObject->setUniform("HO", _ozoneHeightScale);
    glClear(GL_COLOR_BUFFER_BIT);
    for (int layer = 0; layer < _r_samples; ++layer) {
        step3DTexture(*_inScatteringProgramObject, layer, true);
        renderQuadForCalc(quadCalcVAO, vertexSize);
    }
    if (_saveCalculationTextures) {
        saveTextureFile(
            GL_COLOR_ATTACHMENT0,
            "deltaS_rayleigh_texture.ppm",
            glm::ivec2(_mu_s_samples * _nu_samples, _mu_samples)
        );
        saveTextureFile(
            GL_COLOR_ATTACHMENT1,
            "deltaS_mie_texture.ppm",
            glm::ivec2(_mu_s_samples * _nu_samples, _mu_samples)
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
    glDrawBuffer(GL_COLOR_ATTACHMENT0);

    glViewport(0, 0, _deltaETableSize.x, _deltaETableSize.y);
    _deltaEProgramObject->activate();
    glClear(GL_COLOR_BUFFER_BIT);
    renderQuadForCalc(quadCalcVAO, vertexSize);
    if (_saveCalculationTextures) {
        saveTextureFile(
            GL_COLOR_ATTACHMENT0,
            "irradiance_texture.ppm",
            _deltaETableSize
        );
    }
    _deltaEProgramObject->deactivate();

    // line 5 in algorithm 4.1
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        _inScatteringTableTexture,
        0
    );
    glViewport(0, 0, _mu_s_samples * _nu_samples, _mu_samples);
    _deltaSProgramObject->activate();
    deltaSRayleighTableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_3D, _deltaSRayleighTableTexture);
    deltaSMieTableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_3D, _deltaSMieTableTexture);
    _deltaSProgramObject->setUniform("deltaSRTexture", deltaSRayleighTableTextureUnit);
    _deltaSProgramObject->setUniform("deltaSMTexture", deltaSMieTableTextureUnit);
    _deltaSProgramObject->setUniform("SAMPLES_R", _r_samples);
    _deltaSProgramObject->setUniform("SAMPLES_MU", _mu_samples);
    _deltaSProgramObject->setUniform("SAMPLES_MU_S", _mu_s_samples);
    _deltaSProgramObject->setUniform("SAMPLES_NU", _nu_samples);
    glClear(GL_COLOR_BUFFER_BIT);
    for (int layer = 0; layer < _r_samples; ++layer) {
        step3DTexture(*_deltaSProgramObject, layer, false);
        renderQuadForCalc(quadCalcVAO, vertexSize);
    }
    if (_saveCalculationTextures) {
        saveTextureFile(
            GL_COLOR_ATTACHMENT0,
            "S_texture.ppm",
            glm::ivec2(_mu_s_samples * _nu_samples, _mu_samples)
        );
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
        glViewport(0, 0, _mu_s_samples * _nu_samples, _mu_samples);
        _deltaJProgramObject->activate();
        if (scatteringOrder == 2) {
            _deltaJProgramObject->setUniform("firstIteration", 1);
        }
        else {
            _deltaJProgramObject->setUniform("firstIteration", 0);
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
        _deltaJProgramObject->setUniform("Rg", _atmospherePlanetRadius);
        _deltaJProgramObject->setUniform("Rt", _atmosphereRadius);
        _deltaJProgramObject->setUniform("AverageGroundReflectance", _planetAverageGroundReflectance);
        _deltaJProgramObject->setUniform("HR", _rayleighHeightScale);
        _deltaJProgramObject->setUniform("betaRayleigh", _rayleighScatteringCoeff);
        _deltaJProgramObject->setUniform("HM", _mieHeightScale);
        _deltaJProgramObject->setUniform("betaMieScattering", _mieScatteringCoeff);
        _deltaJProgramObject->setUniform("mieG", _miePhaseConstant);
        _deltaJProgramObject->setUniform("SAMPLES_R", _r_samples);
        _deltaJProgramObject->setUniform("SAMPLES_MU", _mu_samples);
        _deltaJProgramObject->setUniform("SAMPLES_MU_S", _mu_s_samples);
        _deltaJProgramObject->setUniform("SAMPLES_NU", _nu_samples);
        for (int layer = 0; layer < _r_samples; ++layer) {
            step3DTexture(*_deltaJProgramObject, layer, true);
            renderQuadForCalc(quadCalcVAO, vertexSize);
        }
        if (_saveCalculationTextures) {
            saveTextureFile(
                GL_COLOR_ATTACHMENT0,
                fmt::format("deltaJ_texture-scattering_order-{}.ppm", scatteringOrder),
                glm::ivec2(_mu_s_samples * _nu_samples, _mu_samples)
            );
        }
        _deltaJProgramObject->deactivate();

        // line 8 in algorithm 4.1
        glFramebufferTexture(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            _deltaETableTexture,
            0
        );
        glViewport(0, 0, _deltaETableSize.x, _deltaETableSize.y);
        _irradianceSupTermsProgramObject->activate();
        if (scatteringOrder == 2) {
            _irradianceSupTermsProgramObject->setUniform("firstIteration", 1);
        }
        else {
            _irradianceSupTermsProgramObject->setUniform("firstIteration", 0);
        }
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
        _irradianceSupTermsProgramObject->setUniform("Rg", _atmospherePlanetRadius);
        _irradianceSupTermsProgramObject->setUniform("Rt", _atmosphereRadius);
        _irradianceSupTermsProgramObject->setUniform("mieG", _miePhaseConstant);
        _irradianceSupTermsProgramObject->setUniform("SKY", _irradianceTableSize);
        _irradianceSupTermsProgramObject->setUniform("SAMPLES_R", _r_samples);
        _irradianceSupTermsProgramObject->setUniform("SAMPLES_MU", _mu_samples);
        _irradianceSupTermsProgramObject->setUniform("SAMPLES_MU_S", _mu_s_samples);
        _irradianceSupTermsProgramObject->setUniform("SAMPLES_NU", _nu_samples);
        renderQuadForCalc(quadCalcVAO, vertexSize);
        if (_saveCalculationTextures) {
            saveTextureFile(
                GL_COLOR_ATTACHMENT0,
                fmt::format("deltaE_texture-scattering_order-{}.ppm", scatteringOrder),
                _deltaETableSize
            );
        }
        _irradianceSupTermsProgramObject->deactivate();

        // line 9 in algorithm 4.1
        glFramebufferTexture(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            _deltaSRayleighTableTexture,
            0
        );
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
        _inScatteringSupTermsProgramObject->setUniform("Rg", _atmospherePlanetRadius);
        _inScatteringSupTermsProgramObject->setUniform("Rt", _atmosphereRadius);
        _inScatteringSupTermsProgramObject->setUniform("SAMPLES_R", _r_samples);
        _inScatteringSupTermsProgramObject->setUniform("SAMPLES_MU", _mu_samples);
        _inScatteringSupTermsProgramObject->setUniform("SAMPLES_MU_S", _mu_s_samples);
        _inScatteringSupTermsProgramObject->setUniform("SAMPLES_NU", _nu_samples);
        for (int layer = 0; layer < _r_samples; ++layer) {
            step3DTexture(*_inScatteringSupTermsProgramObject, layer, true);
            renderQuadForCalc(quadCalcVAO, vertexSize);
        }
        if (_saveCalculationTextures) {
            saveTextureFile(
                GL_COLOR_ATTACHMENT0,
                fmt::format("deltaS_texture-scattering_order-{}.ppm", scatteringOrder),
                glm::ivec2(_mu_s_samples * _nu_samples, _mu_samples)
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
        glViewport(0, 0, _deltaETableSize.x, _deltaETableSize.y);
        _irradianceFinalProgramObject->activate();
        deltaETableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_2D, _deltaETableTexture);
        _irradianceFinalProgramObject->setUniform(
            "deltaETexture",
            deltaETableTextureUnit
        );
        renderQuadForCalc(quadCalcVAO, vertexSize);
        if (_saveCalculationTextures) {
            saveTextureFile(
                GL_COLOR_ATTACHMENT0,
                fmt::format("irradianceTable_order-{}.ppm", scatteringOrder),
                _deltaETableSize
            );
        }
        _irradianceFinalProgramObject->deactivate();

        // line 11 in algorithm 4.1
        glFramebufferTexture(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            _inScatteringTableTexture,
            0
        );
        glViewport(0, 0, _mu_s_samples * _nu_samples, _mu_samples);
        _deltaSSupTermsProgramObject->activate();
        deltaSRayleighTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_3D, _deltaSRayleighTableTexture);
        _deltaSSupTermsProgramObject->setUniform(
            "deltaSTexture",
            deltaSRayleighTableTextureUnit
        );
        _deltaSSupTermsProgramObject->setUniform("SAMPLES_R", _r_samples);
        _deltaSSupTermsProgramObject->setUniform("SAMPLES_MU", _mu_samples);
        _deltaSSupTermsProgramObject->setUniform("SAMPLES_MU_S", _mu_s_samples);
        _deltaSSupTermsProgramObject->setUniform("SAMPLES_NU", _nu_samples);
        for (int layer = 0; layer < _r_samples; ++layer) {
            step3DTexture(*_deltaSSupTermsProgramObject, layer, false);
            renderQuadForCalc(quadCalcVAO, vertexSize);
        }
        if (_saveCalculationTextures) {
            saveTextureFile(GL_COLOR_ATTACHMENT0,
                fmt::format("inscatteringTable_order-{}.ppm", scatteringOrder),
                glm::ivec2(_mu_s_samples * _nu_samples, _mu_samples)
            );
        }
        _deltaSSupTermsProgramObject->deactivate();

        glDisable(GL_BLEND);
    }

    // Restores OpenGL blending state
    global::renderEngine->openglStateCache().resetBlendState();
}

void AtmosphereDeferredcaster::preCalculateAtmosphereParam() {
    // Load Shader Programs for Calculations
    loadComputationPrograms();

    // Create Textures for Calculations
    createComputationTextures();

    // Saves current FBO first
    GLint defaultFBO;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);

    GLint viewport[4];
    global::renderEngine->openglStateCache().viewport(viewport);

    // Creates the FBO for the calculations
    GLuint calcFBO;
    glGenFramebuffers(1, &calcFBO);
    glBindFramebuffer(GL_FRAMEBUFFER, calcFBO);
    GLenum drawBuffers[1] = { GL_COLOR_ATTACHMENT0 };
    glDrawBuffers(1, drawBuffers);

    // Prepare for rendering/calculations
    GLuint quadCalcVAO;
    GLuint quadCalcVBO;
    createRenderQuad(&quadCalcVAO, &quadCalcVBO);

    // Starting Calculations
    LDEBUG("Starting precalculations for scattering effects");

    // Execute Calculations
    executeCalculations(quadCalcVAO, drawBuffers, 6);

    deleteUnusedComputationTextures();

    // Restores system state
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    global::renderEngine->openglStateCache().setViewportState(viewport);
    glDeleteBuffers(1, &quadCalcVBO);
    glDeleteVertexArrays(1, &quadCalcVAO);
    glDeleteFramebuffers(1, &calcFBO);

    LDEBUG("Ended precalculations for Atmosphere effects");
}

void AtmosphereDeferredcaster::step3DTexture(ghoul::opengl::ProgramObject& shaderProg,
                                             int layer, bool doCalculation)
{
    // See OpenGL redbook 8th Edition page 556 for Layered Rendering
    if (doCalculation) {
        const float earth2 = _atmospherePlanetRadius * _atmospherePlanetRadius;
        const float diff = _atmosphereRadius * _atmosphereRadius - earth2;
        const float ri = static_cast<float>(layer) / static_cast<float>(_r_samples - 1);
        const float eps = [&]() {
            if (layer == 0) {
                return 0.01f;
            }
            else {
                if (layer == (_r_samples - 1)) {
                    return -0.001f;
                }
                else {
                    return 0.f;
                }
            }
        }();
        const float r = std::sqrt(earth2 + ri * ri * diff) + eps;
        const float dminG = r - _atmospherePlanetRadius;
        const float dminT = _atmosphereRadius - r;
        const float dh = std::sqrt(r * r - earth2);
        const float dH = dh + std::sqrt(diff);

        shaderProg.setUniform("r", r);
        shaderProg.setUniform("dhdH", dminT, dH, dminG, dh);
    }

    shaderProg.setUniform("layer", layer);
}

} // namespace openspace
