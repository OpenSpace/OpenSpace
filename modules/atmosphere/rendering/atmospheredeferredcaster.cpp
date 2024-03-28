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
    constexpr std::string_view _loggerCat = "AtmosphereDeferredcaster";

    constexpr std::array<const char*, 29> UniformNames = {
        "cullAtmosphere", "opacity", "Rg", "Rt", "groundRadianceEmission", "HR",
        "betaRayleigh", "HM", "betaMieExtinction", "mieG", "sunRadiance",
        "ozoneLayerEnabled", "HO", "betaOzoneExtinction", "SAMPLES_R", "SAMPLES_MU",
        "SAMPLES_MU_S", "SAMPLES_NU", "inverseModelTransformMatrix",
        "modelTransformMatrix", "projectionToModelTransformMatrix", "viewToWorldMatrix",
        "camPosObj", "sunDirectionObj", "hardShadows", "transmittanceTexture",
        "irradianceTexture", "inscatterTexture", "sunAngularSize"
    };

    constexpr float ATM_EPS = 2000.f;
    constexpr float KM_TO_M = 1000.f;

    template <GLenum colorBufferAttachment = GL_COLOR_ATTACHMENT0>
    void saveTextureFile(const std::filesystem::path& fileName, const glm::ivec2& size) {
        std::ofstream ppmFile(fileName);
        if (!ppmFile.is_open()) {
            return;
        }

        std::vector<unsigned char> px(
            size.x * size.y * 3,
            static_cast<unsigned char>(255)
        );

        glReadBuffer(colorBufferAttachment);
        glReadPixels(0, 0, size.x, size.y, GL_RGB, GL_UNSIGNED_BYTE, px.data());

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

    bool isAtmosphereInFrustum(const glm::dmat4& mv, const glm::dvec3& position,
                               double radius)
    {
        // Frustum Planes
        const glm::dvec3 col1 = glm::dvec3(mv[0][0], mv[1][0], mv[2][0]);
        const glm::dvec3 col2 = glm::dvec3(mv[0][1], mv[1][1], mv[2][1]);
        const glm::dvec3 col3 = glm::dvec3(mv[0][2], mv[1][2], mv[2][2]);
        const glm::dvec3 col4 = glm::dvec3(mv[0][3], mv[1][3], mv[2][3]);

        glm::dvec3 leftNormal = col4 + col1;
        glm::dvec3 rightNormal = col4 - col1;
        glm::dvec3 bottomNormal = col4 + col2;
        glm::dvec3 topNormal = col4 - col2;
        glm::dvec3 nearNormal = col3 + col4;
        glm::dvec3 farNormal = col4 - col3;

        // Plane Distances
        double leftDistance = mv[3][3] + mv[3][0];
        double rightDistance = mv[3][3] - mv[3][0];
        double bottomDistance = mv[3][3] + mv[3][1];
        double topDistance = mv[3][3] - mv[3][1];
        double nearDistance = mv[3][3] + mv[3][2];

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

        const bool outsideFrustum =
            (((glm::dot(leftNormal, position) + leftDistance) < -radius) ||
            ((glm::dot(rightNormal, position) + rightDistance) < -radius) ||
            ((glm::dot(bottomNormal, position) + bottomDistance) < -radius) ||
            ((glm::dot(topNormal, position) + topDistance) < -radius) ||
            ((glm::dot(nearNormal, position) + nearDistance) < -radius));

        return !outsideFrustum;
    }

    GLuint createTexture(const glm::ivec2& size, std::string_view name) {
        GLuint t = 0;
        glGenTextures(1, &t);
        glBindTexture(GL_TEXTURE_2D, t);
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
            size.x,
            size.y,
            0,
            GL_RGB,
            GL_FLOAT,
            nullptr
        );
        if (glbinding::Binding::ObjectLabel.isResolved()) {
            glObjectLabel(GL_TEXTURE, t, static_cast<GLsizei>(name.size()), name.data());
        }
        return t;
    }

    GLuint createTexture(const glm::ivec3& size, std::string_view name, int components) {
        ghoul_assert(components == 3 || components == 4, "Only 3-4 components supported");

        GLuint t = 0;
        glGenTextures(1, &t);
        glBindTexture(GL_TEXTURE_3D, t);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
        // Stopped using a buffer object for GL_PIXEL_UNPACK_BUFFER
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
        glTexImage3D(
            GL_TEXTURE_3D,
            0,
            (components == 3) ? GL_RGB32F : GL_RGBA32F,
            size.x,
            size.y,
            size.z,
            0,
            GL_RGB,
            GL_FLOAT,
            nullptr
        );
        if (glbinding::Binding::ObjectLabel.isResolved()) {
            glObjectLabel(GL_TEXTURE, t, static_cast<GLsizei>(name.size()), name.data());
        }
        return t;
    }
} // namespace

namespace openspace {

AtmosphereDeferredcaster::AtmosphereDeferredcaster(float textureScale,
                                       std::vector<ShadowConfiguration> shadowConfigArray,
                                                              bool saveCalculatedTextures)
    : _transmittanceTableSize(glm::ivec2(256 * textureScale, 64 * textureScale))
    , _irradianceTableSize(glm::ivec2(64 * textureScale, 16 * textureScale))
    , _deltaETableSize(glm::ivec2(64 * textureScale, 16 * textureScale))
    , _muSSamples(static_cast<int>(32 * textureScale))
    , _nuSamples(static_cast<int>(8 * textureScale))
    , _muSamples(static_cast<int>(128 * textureScale))
    , _rSamples(static_cast<int>(32 * textureScale))
    , _textureSize(_muSSamples * _nuSamples, _muSamples, _rSamples)
    , _shadowConfArray(std::move(shadowConfigArray))
    , _saveCalculationTextures(saveCalculatedTextures)
{
    std::memset(_uniformNameBuffer, '\0', sizeof(_uniformNameBuffer));
    std::strcpy(_uniformNameBuffer, "shadowDataArray[");
    _shadowDataArrayCache.reserve(_shadowConfArray.size());
}

void AtmosphereDeferredcaster::initialize() {
    ZoneScoped;

    _transmittanceTableTexture = createTexture(_transmittanceTableSize, "Transmittance");
    _irradianceTableTexture = createTexture(_irradianceTableSize, "Irradiance");
    _inScatteringTableTexture = createTexture(_textureSize, "InScattering", 4);
    calculateAtmosphereParameters();
}

void AtmosphereDeferredcaster::deinitialize() {
    ZoneScoped;

    glDeleteTextures(1, &_transmittanceTableTexture);
    glDeleteTextures(1, &_irradianceTableTexture);
    glDeleteTextures(1, &_inScatteringTableTexture);
}

void AtmosphereDeferredcaster::update(const UpdateData&) {}

float AtmosphereDeferredcaster::eclipseShadow(const glm::dvec3& position) {
    // This code is copied from the atmosphere deferred fragment shader
    // It is used to calculate the eclipse shadow
    if (_shadowDataArrayCache.empty() || !_shadowDataArrayCache.front().isShadowing) {
        return 1.f;
    }

    const ShadowRenderingStruct& shadow = _shadowDataArrayCache.front();
    const glm::dvec3 positionToCaster = shadow.casterPositionVec - position;
    const glm::dvec3 sourceToCaster = shadow.sourceCasterVec; // Normalized
    const glm::dvec3 casterShadow =
        dot(positionToCaster, sourceToCaster) * sourceToCaster;
    const glm::dvec3 positionToShadow = positionToCaster - casterShadow;

    const float distanceToShadow = static_cast<float>(length(positionToShadow));
    const double shadowLength = length(casterShadow);

    const float radiusPenumbra = static_cast<float>(
        shadow.radiusCaster * (shadowLength + shadow.penumbra) / shadow.penumbra
    );
    const float radiusUmbra = static_cast<float>(
        shadow.radiusCaster * (shadow.umbra - shadowLength) / shadow.umbra
    );

    // Is the position in the umbra - the fully shaded part
    if (distanceToShadow < radiusUmbra) {
        if (_hardShadowsEnabled) {
            return 0.5f;
        }
        else {
            // Smooth the shadow with the butterworth function
            const float s = radiusUmbra / (radiusUmbra + std::pow(distanceToShadow, 4.f));
            return std::sqrt(s);
        }
    }
    else if (distanceToShadow < radiusPenumbra) { // In penumbra - partially shaded part
        return _hardShadowsEnabled ? 0.5f : distanceToShadow / radiusPenumbra;
    }
    else {
        return 1.f;
    }
}

void AtmosphereDeferredcaster::preRaycast(const RenderData& data, const DeferredcastData&,
                                          ghoul::opengl::ProgramObject& program)
{
    ZoneScoped;
    TracyGpuZone("Atmosphere preRaycast");


    // Atmosphere Frustum Culling
    const glm::dvec3 tPlanetPos = glm::dvec3(
        _modelTransform * glm::dvec4(0.0, 0.0, 0.0, 1.0)
    );
    const double distance = glm::distance(tPlanetPos, data.camera.eyePositionVec3());

    // Radius is in KM
    const double scaledRadius = glm::length(
        glm::dmat3(_modelTransform) * glm::dvec3(KM_TO_M * _atmosphereRadius, 0.0, 0.0)
    );

    // Number of planet radii to use as distance threshold for culling
    program.setUniform(_uniformCache.cullAtmosphere, 1);

    constexpr double DistanceCullingRadii = 5000;
    const glm::dmat4 MV = glm::dmat4(data.camera.sgctInternal.projectionMatrix()) *
        data.camera.combinedViewMatrix();
    if (distance <= scaledRadius * DistanceCullingRadii &&
        isAtmosphereInFrustum(MV, tPlanetPos, scaledRadius + ATM_EPS))
    {
        program.setUniform(_uniformCache.cullAtmosphere, 0);
        program.setUniform(_uniformCache.opacity, _opacity);
        program.setUniform(_uniformCache.Rg, _atmospherePlanetRadius);
        program.setUniform(_uniformCache.Rt, _atmosphereRadius);
        program.setUniform(_uniformCache.groundRadianceEmission, _groundRadianceEmission);
        program.setUniform(_uniformCache.HR, _rayleighHeightScale);
        program.setUniform(_uniformCache.betaRayleigh, _rayleighScatteringCoeff);
        program.setUniform(_uniformCache.HM, _mieHeightScale);
        program.setUniform(_uniformCache.betaMieExtinction, _mieExtinctionCoeff);
        program.setUniform(_uniformCache.mieG, _miePhaseConstant);
        program.setUniform(_uniformCache.sunRadiance, _sunRadianceIntensity);
        program.setUniform(_uniformCache.ozoneLayerEnabled, _ozoneEnabled);
        program.setUniform(_uniformCache.HO, _ozoneHeightScale);
        program.setUniform(_uniformCache.betaOzoneExtinction, _ozoneExtinctionCoeff);
        program.setUniform(_uniformCache.SAMPLES_R, _rSamples);
        program.setUniform(_uniformCache.SAMPLES_MU, _muSamples);
        program.setUniform(_uniformCache.SAMPLES_MU_S, _muSSamples);
        program.setUniform(_uniformCache.SAMPLES_NU, _nuSamples);
        // We expose the value as degrees, but the shader wants radians
        program.setUniform(_uniformCache.sunAngularSize, glm::radians(_sunAngularSize));

        // Object Space
        const glm::dmat4 invModelMatrix = glm::inverse(_modelTransform);
        program.setUniform(_uniformCache.inverseModelTransformMatrix, invModelMatrix);
        program.setUniform(_uniformCache.modelTransformMatrix, _modelTransform);

        const glm::dmat4 viewToWorld = glm::inverse(data.camera.combinedViewMatrix());

        // Eye Space to World Space
        program.setUniform(_uniformCache.viewToWorldMatrix, viewToWorld);

        // Projection to Eye Space
        const glm::dmat4 dInvProj = glm::inverse(
            glm::dmat4(data.camera.projectionMatrix())
        );

        const glm::dmat4 invWholePipeline = invModelMatrix * viewToWorld * dInvProj;

        program.setUniform(_uniformCache.projectionToModelTransform, invWholePipeline);

        const glm::dvec4 camPosObjCoords =
            invModelMatrix * glm::dvec4(data.camera.eyePositionVec3(), 1.0);
        program.setUniform(_uniformCache.camPosObj, glm::dvec3(camPosObjCoords));

        // For the lighting we use the provided node, or the Sun
        SceneGraphNode* node =
            _lightSourceNode ? _lightSourceNode : sceneGraph()->sceneGraphNode("Sun");
        const glm::dvec3 sunPosWorld = node ? node->worldPosition() : glm::dvec3(0.0);

        glm::dvec3 sunPosObj;
        // Sun following camera position
        if (_sunFollowingCameraEnabled) {
            sunPosObj = invModelMatrix * glm::dvec4(data.camera.eyePositionVec3(), 1.0);
        }
        else {
            sunPosObj = invModelMatrix *
                glm::dvec4((sunPosWorld - data.modelTransform.translation) * 1000.0, 1.0);
        }

        // Sun Position in Object Space
        program.setUniform(_uniformCache.sunDirectionObj, glm::normalize(sunPosObj));

        // Shadow calculations..
        _shadowDataArrayCache.clear();
        for (ShadowConfiguration& shadowConf : _shadowConfArray) {
            // TO REMEMBER: all distances and lengths in world coordinates are in
            // meters!!! We need to move this to view space...
            double lt = 0.0;
            glm::dvec3 sourcePos = SpiceManager::ref().targetPosition(
                shadowConf.source.first,
                "SSB",
                "GALACTIC",
                {},
                data.time.j2000Seconds(),
                lt
            );
            sourcePos *= KM_TO_M; // converting to meters
            glm::dvec3 casterPos = SpiceManager::ref().targetPosition(
                shadowConf.caster.first,
                "SSB",
                "GALACTIC",
                {},
                data.time.j2000Seconds(),
                lt
            );
            casterPos *= KM_TO_M; // converting to meters

            SceneGraphNode* sourceNode = sceneGraphNode(shadowConf.source.first);
            if (!sourceNode) {
                if (!shadowConf.printedSourceError) {
                    LERROR("Invalid scenegraph node for the shadow's receiver");
                    shadowConf.printedSourceError = true;
                }
                return;
            }
            SceneGraphNode* casterNode = sceneGraphNode(shadowConf.caster.first);
            if (!casterNode) {
                if (!shadowConf.printedCasterError) {
                    LERROR("Invalid scenegraph node for the shadow's caster");
                    shadowConf.printedCasterError = true;
                }
                return;
            }

            const double sourceScale = std::max(glm::compMax(sourceNode->scale()), 1.0);
            const double casterScale = std::max(glm::compMax(casterNode->scale()), 1.0);
            const double actualSourceRadius = shadowConf.source.second * sourceScale;
            const double actualCasterRadius = shadowConf.caster.second * casterScale;
            // First we determine if the caster is shadowing the current planet
            // (all calculations in World Coordinates):
            const glm::dvec3 planetCasterVec =
                casterPos - data.modelTransform.translation;
            const glm::dvec3 sourceCasterVec = casterPos - sourcePos;
            const double scLength = glm::length(sourceCasterVec);
            const glm::dvec3 planetCasterProj =
                (glm::dot(planetCasterVec, sourceCasterVec) / (scLength * scLength)) *
                sourceCasterVec;
            const double dTest = glm::length(planetCasterVec - planetCasterProj);
            const double xpTest = actualCasterRadius * scLength /
                (actualSourceRadius + actualCasterRadius);
            const double rpTest = actualCasterRadius *
                (glm::length(planetCasterProj) + xpTest) / xpTest;

            const double casterDistSun = glm::length(casterPos - sunPosWorld);
            const double planetDistSun = glm::length(
                data.modelTransform.translation - sunPosWorld
            );

            ShadowRenderingStruct shadow;
            shadow.isShadowing = false;

            if (((dTest - rpTest) < (_atmospherePlanetRadius * KM_TO_M)) &&
                (casterDistSun < planetDistSun))
            {
                // The current caster is shadowing the current planet
                shadow.isShadowing = true;
                shadow.radiusSource = actualSourceRadius;
                shadow.radiusCaster = actualCasterRadius;
                shadow.sourceCasterVec = glm::normalize(sourceCasterVec);
                shadow.penumbra = xpTest;
                shadow.umbra =
                    shadow.radiusCaster * scLength /
                    (shadow.radiusSource - shadow.radiusCaster);
                shadow.casterPositionVec = casterPos;
            }
            _shadowDataArrayCache.push_back(shadow);
        }

        // _uniformNameBuffer[0..15] = "shadowDataArray["
        unsigned int counter = 0;
        for (const ShadowRenderingStruct& sd : _shadowDataArrayCache) {
            // Add the counter
            char* bf = std::format_to(_uniformNameBuffer + 16, "{}", counter);

            std::strcpy(bf, "].isShadowing\0");
            program.setUniform(_uniformNameBuffer, sd.isShadowing);

            if (sd.isShadowing) {
                std::strcpy(bf, "].xp\0");
                program.setUniform(_uniformNameBuffer, sd.penumbra);
                std::strcpy(bf, "].xu\0");
                program.setUniform(_uniformNameBuffer, sd.umbra);
                std::strcpy(bf, "].rc\0");
                program.setUniform(_uniformNameBuffer, sd.radiusCaster);
                std::strcpy(bf, "].sourceCasterVec\0");
                program.setUniform(_uniformNameBuffer, sd.sourceCasterVec);
                std::strcpy(bf, "].casterPositionVec\0");
                program.setUniform(_uniformNameBuffer, sd.casterPositionVec);
            }
            counter++;
        }
        program.setUniform(_uniformCache.hardShadows, _hardShadowsEnabled);
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
    ZoneScoped;

    TracyGpuZone("Atmosphere postRaycast");

    // Deactivate the texture units
    _transmittanceTableTextureUnit.deactivate();
    _irradianceTableTextureUnit.deactivate();
    _inScatteringTableTextureUnit.deactivate();
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

void AtmosphereDeferredcaster::setModelTransform(glm::dmat4 transform) {
    _modelTransform = std::move(transform);
}

void AtmosphereDeferredcaster::setOpacity(float opacity) {
    _opacity = opacity;
}

void AtmosphereDeferredcaster::setParameters(float atmosphereRadius, float planetRadius,
                                             float averageGroundReflectance,
                                             float groundRadianceEmission,
                                             float rayleighHeightScale, bool enableOzone,
                                             float ozoneHeightScale, float mieHeightScale,
                                             float miePhaseConstant, float sunRadiance,
                                             glm::vec3 rayScatteringCoefficients,
                                             glm::vec3 ozoneExtinctionCoefficients,
                                             glm::vec3 mieScatteringCoefficients,
                                             glm::vec3 mieExtinctionCoefficients,
                                             bool sunFollowing, float sunAngularSize,
                                             SceneGraphNode* lightSourceNode)
{
    _atmosphereRadius = atmosphereRadius;
    _atmospherePlanetRadius = planetRadius;
    _averageGroundReflectance = averageGroundReflectance;
    _groundRadianceEmission = groundRadianceEmission;
    _rayleighHeightScale = rayleighHeightScale;
    _ozoneEnabled = enableOzone;
    _ozoneHeightScale = ozoneHeightScale;
    _mieHeightScale = mieHeightScale;
    _miePhaseConstant = miePhaseConstant;
    _sunRadianceIntensity = sunRadiance;
    _rayleighScatteringCoeff = std::move(rayScatteringCoefficients);
    _ozoneExtinctionCoeff = std::move(ozoneExtinctionCoefficients);
    _mieScatteringCoeff = std::move(mieScatteringCoefficients);
    _mieExtinctionCoeff = std::move(mieExtinctionCoefficients);
    _sunFollowingCameraEnabled = sunFollowing;
    _sunAngularSize = sunAngularSize;
    // The light source may be nullptr which we interpret to mean a position of (0,0,0)
    _lightSourceNode = lightSourceNode;
}

void AtmosphereDeferredcaster::setHardShadows(bool enabled) {
    _hardShadowsEnabled = enabled;
}

void AtmosphereDeferredcaster::calculateTransmittance() {
    ZoneScoped;

    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        _transmittanceTableTexture,
        0
    );
    glViewport(0, 0, _transmittanceTableSize.x, _transmittanceTableSize.y);
    using ProgramObject = ghoul::opengl::ProgramObject;
    std::unique_ptr<ProgramObject> program = ProgramObject::Build(
        "Transmittance Program",
        absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
        absPath("${MODULE_ATMOSPHERE}/shaders/transmittance_calc_fs.glsl")
    );
    program->activate();
    program->setUniform("Rg", _atmospherePlanetRadius);
    program->setUniform("Rt", _atmosphereRadius);
    program->setUniform("HR", _rayleighHeightScale);
    program->setUniform("betaRayleigh", _rayleighScatteringCoeff);
    program->setUniform("HM", _mieHeightScale);
    program->setUniform("betaMieExtinction", _mieExtinctionCoeff);
    program->setUniform("TRANSMITTANCE", _transmittanceTableSize);
    program->setUniform("ozoneLayerEnabled", _ozoneEnabled);
    program->setUniform("HO", _ozoneHeightScale);
    program->setUniform("betaOzoneExtinction", _ozoneExtinctionCoeff);

    constexpr glm::vec4 Black = glm::vec4(0.f, 0.f, 0.f, 0.f);
    glClearBufferfv(GL_COLOR, 0, glm::value_ptr(Black));
    glDrawArrays(GL_TRIANGLES, 0, 6);
    if (_saveCalculationTextures) {
        saveTextureFile("transmittance_texture.ppm", _transmittanceTableSize);
    }
    program->deactivate();
}

GLuint AtmosphereDeferredcaster::calculateDeltaE() {
    ZoneScoped;

    const GLuint deltaE = createTexture(_deltaETableSize, "DeltaE");
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, deltaE, 0);
    glViewport(0, 0, _deltaETableSize.x, _deltaETableSize.y);
    using ProgramObject = ghoul::opengl::ProgramObject;
    std::unique_ptr<ProgramObject> program = ProgramObject::Build(
        "Irradiance Program",
        absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
        absPath("${MODULE_ATMOSPHERE}/shaders/irradiance_calc_fs.glsl")
    );
    program->activate();
    ghoul::opengl::TextureUnit unit;
    unit.activate();
    glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
    program->setUniform("transmittanceTexture", unit);
    program->setUniform("Rg", _atmospherePlanetRadius);
    program->setUniform("Rt", _atmosphereRadius);
    program->setUniform("OTHER_TEXTURES", _deltaETableSize);
    glClear(GL_COLOR_BUFFER_BIT);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    if (_saveCalculationTextures) {
        saveTextureFile("deltaE_table_texture.ppm", _deltaETableSize);
    }
    program->deactivate();
    return deltaE;
}

std::pair<GLuint, GLuint> AtmosphereDeferredcaster::calculateDeltaS() {
    ZoneScoped;

    const GLuint deltaSRayleigh = createTexture(_textureSize, "DeltaS Rayleigh", 3);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, deltaSRayleigh, 0);
    const GLuint deltaSMie = createTexture(_textureSize, "DeltaS Mie", 3);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, deltaSMie, 0);
    std::array<GLenum, 2> colorBuffers = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
    glDrawBuffers(2, colorBuffers.data());
    glViewport(0, 0, _textureSize.x, _textureSize.y);
    using ProgramObject = ghoul::opengl::ProgramObject;
    std::unique_ptr<ProgramObject> program = ProgramObject::Build(
        "InScattering Program",
        absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
        absPath("${MODULE_ATMOSPHERE}/shaders/inScattering_calc_fs.glsl"),
        absPath("${MODULE_ATMOSPHERE}/shaders/calculation_gs.glsl")
    );
    program->activate();
    ghoul::opengl::TextureUnit unit;
    unit.activate();
    glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
    program->setUniform("transmittanceTexture", unit);
    program->setUniform("Rg", _atmospherePlanetRadius);
    program->setUniform("Rt", _atmosphereRadius);
    program->setUniform("HR", _rayleighHeightScale);
    program->setUniform("betaRayleigh", _rayleighScatteringCoeff);
    program->setUniform("HM", _mieHeightScale);
    program->setUniform("betaMieScattering", _mieScatteringCoeff);
    program->setUniform("SAMPLES_MU_S", _muSSamples);
    program->setUniform("SAMPLES_NU", _nuSamples);
    program->setUniform("SAMPLES_MU", _muSamples);
    program->setUniform("ozoneLayerEnabled", _ozoneEnabled);
    program->setUniform("HO", _ozoneHeightScale);
    glClear(GL_COLOR_BUFFER_BIT);
    for (int layer = 0; layer < _rSamples; ++layer) {
        program->setUniform("layer", layer);
        step3DTexture(*program, layer);
        glDrawArrays(GL_TRIANGLES, 0, 6);
    }
    if (_saveCalculationTextures) {
        saveTextureFile("deltaS_rayleigh_texture.ppm", glm::ivec2(_textureSize));
        saveTextureFile<GL_COLOR_ATTACHMENT1>(
            "deltaS_mie_texture.ppm",
            glm::ivec2(_textureSize)
        );
    }
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, 0, 0);
    const std::array<GLenum, 1> drawBuffers = { GL_COLOR_ATTACHMENT0 };
    glDrawBuffers(1, drawBuffers.data());

    program->deactivate();
    return { deltaSRayleigh, deltaSMie };
}

void AtmosphereDeferredcaster::calculateIrradiance() {
    ZoneScoped;

    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        _irradianceTableTexture,
        0
    );
    glDrawBuffer(GL_COLOR_ATTACHMENT0);

    glViewport(0, 0, _deltaETableSize.x, _deltaETableSize.y);
    using ProgramObject = ghoul::opengl::ProgramObject;
    std::unique_ptr<ProgramObject> program = ProgramObject::Build(
        "DeltaE Program",
        absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
        absPath("${MODULE_ATMOSPHERE}/shaders/deltaE_calc_fs.glsl")
    );
    program->activate();
    glClear(GL_COLOR_BUFFER_BIT);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    if (_saveCalculationTextures) {
        saveTextureFile("irradiance_texture.ppm", _deltaETableSize);
    }
    program->deactivate();
}

void AtmosphereDeferredcaster::calculateInscattering(GLuint deltaSRayleigh,
                                                     GLuint deltaSMie)
{
    ZoneScoped;

    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        _inScatteringTableTexture,
        0
    );
    glViewport(0, 0, _textureSize.x, _textureSize.y);
    using ProgramObject = ghoul::opengl::ProgramObject;
    std::unique_ptr<ProgramObject> program = ProgramObject::Build(
        "deltaSCalcProgram",
        absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
        absPath("${MODULE_ATMOSPHERE}/shaders/deltaS_calc_fs.glsl"),
        absPath("${MODULE_ATMOSPHERE}/shaders/calculation_gs.glsl")
    );
    program->activate();

    ghoul::opengl::TextureUnit deltaSRayleighUnit;
    deltaSRayleighUnit.activate();
    glBindTexture(GL_TEXTURE_3D, deltaSRayleigh);
    program->setUniform("deltaSRTexture", deltaSRayleighUnit);

    ghoul::opengl::TextureUnit deltaSMieUnit;
    deltaSMieUnit.activate();
    glBindTexture(GL_TEXTURE_3D, deltaSMie);
    program->setUniform("deltaSMTexture", deltaSMieUnit);

    program->setUniform("SAMPLES_MU_S", _muSSamples);
    program->setUniform("SAMPLES_NU", _nuSamples);
    program->setUniform("SAMPLES_MU", _muSamples);
    program->setUniform("SAMPLES_R", _rSamples);
    glClear(GL_COLOR_BUFFER_BIT);
    for (int layer = 0; layer < _rSamples; ++layer) {
        program->setUniform("layer", layer);
        glDrawArrays(GL_TRIANGLES, 0, 6);
    }
    if (_saveCalculationTextures) {
        saveTextureFile("S_texture.ppm", glm::ivec2(_textureSize));
    }
    program->deactivate();
}

void AtmosphereDeferredcaster::calculateDeltaJ(int scatteringOrder,
                                               ghoul::opengl::ProgramObject& program,
                                               GLuint deltaJ, GLuint deltaE,
                                               GLuint deltaSRayleigh, GLuint deltaSMie)
{
    ZoneScoped;

    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, deltaJ, 0);
    glViewport(0, 0, _textureSize.x, _textureSize.y);
    program.activate();

    ghoul::opengl::TextureUnit transmittanceUnit;
    transmittanceUnit.activate();
    glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
    program.setUniform("transmittanceTexture", transmittanceUnit);

    ghoul::opengl::TextureUnit deltaEUnit;
    deltaEUnit.activate();
    glBindTexture(GL_TEXTURE_2D, deltaE);
    program.setUniform("deltaETexture", deltaEUnit);

    ghoul::opengl::TextureUnit deltaSRayleighUnit;
    deltaSRayleighUnit.activate();
    glBindTexture(GL_TEXTURE_3D, deltaSRayleigh);
    program.setUniform("deltaSRTexture", deltaSRayleighUnit);

    ghoul::opengl::TextureUnit deltaSMieUnit;
    deltaSMieUnit.activate();
    glBindTexture(GL_TEXTURE_3D, deltaSMie);
    program.setUniform("deltaSMTexture", deltaSMieUnit);

    program.setUniform("firstIteration", (scatteringOrder == 2) ? 1 : 0);
    program.setUniform("Rg", _atmospherePlanetRadius);
    program.setUniform("Rt", _atmosphereRadius);
    program.setUniform("AverageGroundReflectance", _averageGroundReflectance);
    program.setUniform("HR", _rayleighHeightScale);
    program.setUniform("betaRayleigh", _rayleighScatteringCoeff);
    program.setUniform("HM", _mieHeightScale);
    program.setUniform("betaMieScattering", _mieScatteringCoeff);
    program.setUniform("mieG", _miePhaseConstant);
    program.setUniform("SAMPLES_MU_S", _muSSamples);
    program.setUniform("SAMPLES_NU", _nuSamples);
    program.setUniform("SAMPLES_MU", _muSamples);
    program.setUniform("SAMPLES_R", _rSamples);
    for (int layer = 0; layer < _rSamples; ++layer) {
        program.setUniform("layer", layer);
        step3DTexture(program, layer);
        glDrawArrays(GL_TRIANGLES, 0, 6);
    }
    if (_saveCalculationTextures) {
        saveTextureFile(
            std::format("deltaJ_texture-scattering_order-{}.ppm", scatteringOrder),
            glm::ivec2(_textureSize)
        );
    }
    program.deactivate();
}

void AtmosphereDeferredcaster::calculateDeltaE(int scatteringOrder,
                                               ghoul::opengl::ProgramObject& program,
                                               GLuint deltaE, GLuint deltaSRayleigh,
                                               GLuint deltaSMie)
{
    ZoneScoped;

    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, deltaE, 0);
    glViewport(0, 0, _deltaETableSize.x, _deltaETableSize.y);
    program.activate();

    ghoul::opengl::TextureUnit deltaSRayleighUnit;
    deltaSRayleighUnit.activate();
    glBindTexture(GL_TEXTURE_3D, deltaSRayleigh);
    program.setUniform("deltaSRTexture", deltaSRayleighUnit);

    ghoul::opengl::TextureUnit deltaSMieUnit;
    deltaSMieUnit.activate();
    glBindTexture(GL_TEXTURE_3D, deltaSMie);
    program.setUniform("deltaSMTexture", deltaSMieUnit);

    program.setUniform("firstIteration", (scatteringOrder == 2) ? 1 : 0);
    program.setUniform("Rg", _atmospherePlanetRadius);
    program.setUniform("Rt", _atmosphereRadius);
    program.setUniform("mieG", _miePhaseConstant);
    program.setUniform("SKY", _irradianceTableSize);
    program.setUniform("SAMPLES_MU_S", _muSSamples);
    program.setUniform("SAMPLES_NU", _nuSamples);
    program.setUniform("SAMPLES_MU", _muSamples);
    program.setUniform("SAMPLES_R", _rSamples);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    if (_saveCalculationTextures) {
        saveTextureFile(
            std::format("deltaE_texture-scattering_order-{}.ppm", scatteringOrder),
            _deltaETableSize
        );
    }
    program.deactivate();
}

void AtmosphereDeferredcaster::calculateDeltaS(int scatteringOrder,
                                               ghoul::opengl::ProgramObject& program,
                                               GLuint deltaSRayleigh, GLuint deltaJ)
{
    ZoneScoped;

    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, deltaSRayleigh, 0);
    glViewport(0, 0, _textureSize.x, _textureSize.y);
    program.activate();

    ghoul::opengl::TextureUnit transmittanceUnit;
    transmittanceUnit.activate();
    glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
    program.setUniform("transmittanceTexture", transmittanceUnit);

    ghoul::opengl::TextureUnit deltaJUnit;
    deltaJUnit.activate();
    glBindTexture(GL_TEXTURE_3D, deltaJ);
    program.setUniform("deltaJTexture", deltaJUnit);

    program.setUniform("Rg", _atmospherePlanetRadius);
    program.setUniform("Rt", _atmosphereRadius);
    program.setUniform("SAMPLES_MU_S", _muSSamples);
    program.setUniform("SAMPLES_NU", _nuSamples);
    program.setUniform("SAMPLES_MU", _muSamples);
    program.setUniform("SAMPLES_R", _rSamples);
    for (int layer = 0; layer < _rSamples; ++layer) {
        program.setUniform("layer", layer);
        step3DTexture(program, layer);
        glDrawArrays(GL_TRIANGLES, 0, 6);
    }
    if (_saveCalculationTextures) {
        saveTextureFile(
            std::format("deltaS_texture-scattering_order-{}.ppm", scatteringOrder),
            glm::ivec2(_textureSize)
        );
    }
    program.deactivate();
}

void AtmosphereDeferredcaster::calculateIrradiance(int scatteringOrder,
                                                   ghoul::opengl::ProgramObject& program,
                                                   GLuint deltaE)
{
    ZoneScoped;

    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        _irradianceTableTexture,
        0
    );
    glViewport(0, 0, _deltaETableSize.x, _deltaETableSize.y);
    program.activate();

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    glBindTexture(GL_TEXTURE_2D, deltaE);
    program.setUniform("deltaETexture", unit);
    program.setUniform("OTHER_TEXTURES", _deltaETableSize);

    glDrawArrays(GL_TRIANGLES, 0, 6);
    if (_saveCalculationTextures) {
        saveTextureFile(
            std::format("irradianceTable_order-{}.ppm", scatteringOrder),
            _deltaETableSize
        );
    }
    program.deactivate();
}

void AtmosphereDeferredcaster::calculateInscattering(int scatteringOrder,
                                                    ghoul::opengl::ProgramObject& program,
                                                                    GLuint deltaSRayleigh)

{
    ZoneScoped;

    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        _inScatteringTableTexture,
        0
    );
    glViewport(0, 0, _textureSize.x, _textureSize.y);
    program.activate();

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    glBindTexture(GL_TEXTURE_3D, deltaSRayleigh);
    program.setUniform("deltaSTexture", unit);
    program.setUniform("SAMPLES_MU_S", _muSSamples);
    program.setUniform("SAMPLES_NU", _nuSamples);
    program.setUniform("SAMPLES_MU", _muSamples);
    program.setUniform("SAMPLES_R", _rSamples);
    for (int layer = 0; layer < _rSamples; ++layer) {
        program.setUniform("layer", layer);
        glDrawArrays(GL_TRIANGLES, 0, 6);
    }
    if (_saveCalculationTextures) {
        saveTextureFile(
            std::format("inscatteringTable_order-{}.ppm", scatteringOrder),
            glm::ivec2(_textureSize)
        );
    }
    program.deactivate();
}

void AtmosphereDeferredcaster::calculateAtmosphereParameters() {
    ZoneScoped;

    using ProgramObject = ghoul::opengl::ProgramObject;
    std::unique_ptr<ProgramObject> deltaJProgram = ProgramObject::Build(
        "DeltaJ Program",
        absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
        absPath("${MODULE_ATMOSPHERE}/shaders/deltaJ_calc_fs.glsl"),
        absPath("${MODULE_ATMOSPHERE}/shaders/calculation_gs.glsl")
    );
    std::unique_ptr<ProgramObject> irradianceSupTermsProgram = ProgramObject::Build(
        "IrradianceSupTerms Program",
        absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
        absPath("${MODULE_ATMOSPHERE}/shaders/irradiance_sup_calc_fs.glsl")
    );
    std::unique_ptr<ProgramObject> inScatteringSupTermsProgram = ProgramObject::Build(
        "InScatteringSupTerms Program",
        absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
        absPath("${MODULE_ATMOSPHERE}/shaders/inScattering_sup_calc_fs.glsl"),
        absPath("${MODULE_ATMOSPHERE}/shaders/calculation_gs.glsl")
    );
    std::unique_ptr<ProgramObject> irradianceFinalProgram = ProgramObject::Build(
        "IrradianceEFinal Program",
        absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
        absPath("${MODULE_ATMOSPHERE}/shaders/irradiance_final_fs.glsl")
    );
    std::unique_ptr<ProgramObject> deltaSSupTermsProgram = ProgramObject::Build(
        "DeltaSSUPTerms Program",
        absPath("${MODULE_ATMOSPHERE}/shaders/calculation_vs.glsl"),
        absPath("${MODULE_ATMOSPHERE}/shaders/deltaS_sup_calc_fs.glsl"),
        absPath("${MODULE_ATMOSPHERE}/shaders/calculation_gs.glsl")
    );


    // Saves current FBO first
    GLint defaultFBO = 0;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);

    std::array<GLint, 4> viewport;
    global::renderEngine->openglStateCache().viewport(viewport.data());

    // Creates the FBO for the calculations
    GLuint calcFBO = 0;
    glGenFramebuffers(1, &calcFBO);
    glBindFramebuffer(GL_FRAMEBUFFER, calcFBO);
    std::array<GLenum, 1> drawBuffers = { GL_COLOR_ATTACHMENT0 };
    glDrawBuffers(1, drawBuffers.data());

    // Prepare for rendering/calculations
    GLuint quadVao = 0;
    glGenVertexArrays(1, &quadVao);
    glBindVertexArray(quadVao);
    GLuint quadVbo = 0;
    glGenBuffers(1, &quadVbo);
    glBindBuffer(GL_ARRAY_BUFFER, quadVbo);

    constexpr std::array<GLfloat, 12> VertexData = {
        // x     y
        -1.f, -1.f,
         1.f,  1.f,
        -1.f,  1.f,
        -1.f, -1.f,
         1.f, -1.f,
         1.f,  1.f,
    };

    glBufferData(GL_ARRAY_BUFFER, sizeof(VertexData), VertexData.data(), GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(GLfloat), nullptr);

    // Execute Calculations
    LDEBUG("Starting precalculations for scattering effects");
    glDisable(GL_BLEND);

    // See Precomputed Atmosphere Scattering from Bruneton et al. paper, algorithm 4.1:
    calculateTransmittance();

    // line 2 in algorithm 4.1
    const GLuint deltaETable = calculateDeltaE();

    // line 3 in algorithm 4.1
    auto [deltaSRayleighTable, deltaSMieTable] = calculateDeltaS();

    // line 4 in algorithm 4.1
    calculateIrradiance();

    // line 5 in algorithm 4.1
    calculateInscattering(deltaSRayleighTable, deltaSMieTable);

    const GLuint deltaJTable = createTexture(_textureSize, "DeltaJ", 3);

    // loop in line 6 in algorithm 4.1
    for (int scatteringOrder = 2; scatteringOrder <= 4; ++scatteringOrder) {
        // line 7 in algorithm 4.1
        calculateDeltaJ(
            scatteringOrder,
            *deltaJProgram,
            deltaJTable,
            deltaETable,
            deltaSRayleighTable,
            deltaSMieTable
        );

        // line 8 in algorithm 4.1
        calculateDeltaE(
            scatteringOrder,
            *irradianceSupTermsProgram,
            deltaETable,
            deltaSRayleighTable,
            deltaSMieTable
        );

        // line 9 in algorithm 4.1
        calculateDeltaS(
            scatteringOrder,
            *inScatteringSupTermsProgram,
            deltaSRayleighTable,
            deltaJTable
        );

        glEnable(GL_BLEND);
        glBlendEquationSeparate(GL_FUNC_ADD, GL_FUNC_ADD);
        glBlendFuncSeparate(GL_ONE, GL_ONE, GL_ONE, GL_ONE);

        // line 10 in algorithm 4.1
        calculateIrradiance(
            scatteringOrder,
            *irradianceFinalProgram,
            deltaETable
        );

        // line 11 in algorithm 4.1
        calculateInscattering(
            scatteringOrder,
            *deltaSSupTermsProgram,
            deltaSRayleighTable
        );

        glDisable(GL_BLEND);
    }

    // Restores OpenGL blending state
    global::renderEngine->openglStateCache().resetBlendState();

    glDeleteTextures(1, &deltaETable);
    glDeleteTextures(1, &deltaSRayleighTable);
    glDeleteTextures(1, &deltaSMieTable);
    glDeleteTextures(1, &deltaJTable);

    // Restores system state
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    global::renderEngine->openglStateCache().setViewportState(viewport.data());
    glDeleteBuffers(1, &quadVbo);
    glDeleteVertexArrays(1, &quadVao);
    glDeleteFramebuffers(1, &calcFBO);
    glBindVertexArray(0);

    LDEBUG("Ended precalculations for Atmosphere effects");
}

void AtmosphereDeferredcaster::step3DTexture(ghoul::opengl::ProgramObject& prg,
                                             int layer) const
{
    // See OpenGL redbook 8th Edition page 556 for Layered Rendering
    const float planet2 = _atmospherePlanetRadius * _atmospherePlanetRadius;
    const float diff = _atmosphereRadius * _atmosphereRadius - planet2;
    const float ri = static_cast<float>(layer) / static_cast<float>(_rSamples - 1);
    float eps = 0.01f;
    if (layer > 0) {
        if (layer == (_rSamples - 1)) {
            eps = -0.001f;
        }
        else {
            eps = 0.f;
        }
    }
    const float r = std::sqrt(planet2 + ri * ri * diff) + eps;
    const float dminG = r - _atmospherePlanetRadius;
    const float dminT = _atmosphereRadius - r;
    const float dh = std::sqrt(r * r - planet2);
    const float dH = dh + std::sqrt(diff);

    prg.setUniform("r", r);
    prg.setUniform("dhdH", dminT, dH, dminG, dh);
}

} // namespace openspace
