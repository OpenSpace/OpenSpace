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

#include <modules/globebrowsing/src/shadowcomponent.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/src/renderableglobe.h>
#include <openspace/camera/camera.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <glm/gtc/matrix_transform.hpp>
#include <fstream>
#include <cstdlib>
#include <locale>

namespace {
    constexpr std::string_view _loggerCat = "ShadowComponent";

    // Texture coords in [0, 1], while clip coords in [-1, 1]
    const glm::dmat4 ToTextureCoordsMatrix = glm::dmat4(
        glm::dvec4(0.5, 0.0, 0.0, 0.0),
        glm::dvec4(0.0, 0.5, 0.0, 0.0),
        glm::dvec4(0.0, 0.0, 1.0, 0.0),
        glm::dvec4(0.5, 0.5, 0.0, 1.0)
    );

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "Enable/Disable Shadows",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SaveDepthTextureInfo = {
        "SaveDepthTextureInfo",
        "Save Depth Texture",
        "Debug",
        // @VISIBILITY(3.67)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DistanceFractionInfo = {
        "DistanceFraction",
        "Distance Fraction",
        "Distance fraction of original distance from light source to the globe to be "
        "considered as the new light source distance",
        // @VISIBILITY(3.67)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DepthMapSizeInfo = {
        "DepthMapSize",
        "Depth Map Size",
        "The depth map size in pixels. You must entry the width and height values",
        // @VISIBILITY(3.67)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr std::array<GLfloat, 4> ShadowBorder = { 1.f, 1.f, 1.f, 1.f };

    void checkFrameBufferState(const std::string& codePosition) {
        if (glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
            LERROR("Framework not built. " + codePosition);
            const GLenum fbErr = glCheckFramebufferStatus(GL_FRAMEBUFFER);
            switch (fbErr) {
                case GL_FRAMEBUFFER_UNDEFINED:
                    LERROR("Indefined framebuffer");
                    break;
                case GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT:
                    LERROR("Incomplete, missing attachement");
                    break;
                case GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT:
                    LERROR("Framebuffer doesn't have at least one image attached to it");
                    break;
                case GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER:
                    LERROR(
                        "Returned if the value of GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE "
                        "is GL_NONE for any color attachment point(s) named by "
                        "GL_DRAW_BUFFERi"
                    );
                    break;
                case GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER:
                    LERROR(
                        "Returned if GL_READ_BUFFER is not GL_NONE and the value of "
                        "GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE is GL_NONE for the color "
                        "attachment point named by GL_READ_BUFFER");
                    break;
                case GL_FRAMEBUFFER_UNSUPPORTED:
                    LERROR(
                        "Returned if the combination of internal formats of the attached "
                        "images violates an implementation - dependent set of "
                        "restrictions"
                    );
                    break;
                case GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE:
                    LERROR(
                        "Returned if the value of GL_RENDERBUFFE_r_samples is not the "
                        "same for all attached renderbuffers; if the value of "
                        "GL_TEXTURE_SAMPLES is the not same for all attached textures; "
                        "or , if the attached images are a mix of renderbuffers and "
                        "textures, the value of GL_RENDERBUFFE_r_samples does not match "
                        "the value of GL_TEXTURE_SAMPLES"
                    );
                    LERROR(
                        "Returned if the value of GL_TEXTURE_FIXED_SAMPLE_LOCATIONS is "
                        "not the same for all attached textures; or , if the attached "
                        "images are a mix of renderbuffers and textures, the value of "
                        "GL_TEXTURE_FIXED_SAMPLE_LOCATIONS is not GL_TRUE for all "
                        "attached textures"
                    );
                    break;
                case GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS:
                    LERROR(
                        "Returned if any framebuffer attachment is layered, and any "
                        "populated attachment is not layered, or if all populated color "
                        "attachments are not from textures of the same target"
                    );
                    break;
                default:
                    LDEBUG("No error found checking framebuffer: " + codePosition);
                    break;
            }
        }
    }

    struct [[codegen::Dictionary(ShadowComponent)]] Parameters {
        // [[codegen::verbatim(DistanceFractionInfo.description)]]
        std::optional<int> distanceFraction;

        // [[codegen::verbatim(DepthMapSizeInfo.description)]]
        std::optional<glm::ivec2> depthMapSize [[codegen::greater({ 1280, 720 })]];
    };
#include "shadowcomponent_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation ShadowComponent::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_shadows_component");
}

ShadowComponent::ShadowComponent(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "ShadowsComponent" })
    , _saveDepthTexture(SaveDepthTextureInfo)
    , _distanceFraction(DistanceFractionInfo, 20, 1, 10000)
    , _enabled(EnabledInfo, true)
{
    using ghoul::filesystem::File;

    // @TODO (abock, 2021-03-25)  This is not really a nice solution as this key name is
    // coded into the RenderableGlobe. Instead, the parent should unpack the dictionary
    // and pass the unpacked dictionary in here;  Or maybe we don't want a dictionary at
    // this state anyway?
    if (!dictionary.hasValue<ghoul::Dictionary>("Shadows")) {
        return;
    }
    const ghoul::Dictionary d = dictionary.value<ghoul::Dictionary>("Shadows");

    const Parameters p = codegen::bake<Parameters>(d);

    addProperty(_enabled);

    _distanceFraction = p.distanceFraction.value_or(_distanceFraction);
    addProperty(_distanceFraction);

    _saveDepthTexture.onChange([this]() { _executeDepthTextureSave = true; });

    if (p.depthMapSize.has_value()) {
        _shadowDepthTextureWidth = p.depthMapSize->x;
        _shadowDepthTextureHeight = p.depthMapSize->y;
        _dynamicDepthTextureRes = false;
    }
    else {
        const glm::ivec2 renderingRes= global::renderEngine->renderingResolution();
        _shadowDepthTextureWidth = renderingRes.x * 2;
        _shadowDepthTextureHeight = renderingRes.y * 2;
        _dynamicDepthTextureRes = true;
    }

    addProperty(_saveDepthTexture);
}

void ShadowComponent::initialize() {
    buildDDepthTexture();
}

bool ShadowComponent::isReady() const {
    return true;
}

void ShadowComponent::initializeGL() {
    ZoneScoped;

    createDepthTexture();
    createShadowFBO();
}

void ShadowComponent::deinitializeGL() {
    glDeleteTextures(1, &_shadowDepthTexture);
    glDeleteTextures(1, &_positionInLightSpaceTexture);
    glDeleteTextures(1, &_dDepthTexture);
    glDeleteFramebuffers(1, &_shadowFBO);
}

RenderData ShadowComponent::begin(const RenderData& data) {
    const glm::ivec2 renderingResolution = global::renderEngine->renderingResolution();
    if (_dynamicDepthTextureRes && ((_shadowDepthTextureWidth != renderingResolution.x) ||
        (_shadowDepthTextureHeight != renderingResolution.y)))
    {
        _shadowDepthTextureWidth = renderingResolution.x * 2;
        _shadowDepthTextureHeight = renderingResolution.y * 2;
        updateDepthTexture();
    }

    // ===========================================
    // Builds light's ModelViewProjectionMatrix:
    // ===========================================

    const glm::dvec3 diffVector =
        glm::dvec3(_sunPosition) - data.modelTransform.translation;
    const double originalLightDistance = glm::length(diffVector);
    const glm::dvec3 lightDirection = glm::normalize(diffVector);

    // Percentage of the original light source distance (to avoid artifacts)
    //double multiplier = originalLightDistance *
    //    (static_cast<double>(_distanceFraction)/1.0E5);

    const double multiplier = originalLightDistance *
        (static_cast<double>(_distanceFraction) / 1E17);

    // New light source position
    //glm::dvec3 lightPosition = data.modelTransform.translation +
    //    (lightDirection * multiplier);
    const glm::dvec3 lightPosition = data.modelTransform.translation +
        (diffVector * multiplier);

    //// Light Position
    //glm::dvec3 lightPosition = glm::dvec3(_sunPosition);

    //=============== Manually Created Camera Matrix ===================
    //==================================================================
    // camera Z
    const glm::dvec3 cameraZ = lightDirection;

    // camera X
    const glm::dvec3 upVector = glm::dvec3(0.0, 1.0, 0.0);
    const glm::dvec3 cameraX = glm::normalize(glm::cross(upVector, cameraZ));

    // camera Y
    const glm::dvec3 cameraY = glm::cross(cameraZ, cameraX);

    // init 4x4 matrix
    glm::dmat4 cameraRotationMatrix(1.0);

    double* matrix = glm::value_ptr(cameraRotationMatrix);
    matrix[0] = cameraX.x;
    matrix[4] = cameraX.y;
    matrix[8] = cameraX.z;
    matrix[1] = cameraY.x;
    matrix[5] = cameraY.y;
    matrix[9] = cameraY.z;
    matrix[2] = cameraZ.x;
    matrix[6] = cameraZ.y;
    matrix[10] = cameraZ.z;

    // set translation part
    // We aren't setting the position here because it is set in
    // the camera->setPosition()
    //matrix[12] = -glm::dot(cameraX, lightPosition);
    //matrix[13] = -glm::dot(cameraY, lightPosition);
    //matrix[14] = -glm::dot(cameraZ, lightPosition);


    _lightCamera = std::make_unique<Camera>(data.camera);
    _lightCamera->setPositionVec3(lightPosition);
    _lightCamera->setRotation(glm::dquat(glm::inverse(cameraRotationMatrix)));
    //=======================================================================
    //=======================================================================


    //============= Light Matrix by Camera Matrices Composition =============
    //=======================================================================
    const glm::dmat4 lightProjectionMatrix = glm::dmat4(_lightCamera->projectionMatrix());

    // The model transformation missing in the final shadow matrix is add when rendering
    // each object (using its transformations provided by the RenderData structure)
    _shadowData.shadowMatrix =
        ToTextureCoordsMatrix * lightProjectionMatrix *
        _lightCamera->combinedViewMatrix();


    // Saves current state
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &_currentFBO);
    global::renderEngine->openglStateCache().viewport(_viewport.data());

    glBindFramebuffer(GL_FRAMEBUFFER, _shadowFBO);
    std::array<GLenum, 3> drawBuffers = { GL_COLOR_ATTACHMENT0, GL_NONE, GL_NONE };
    glDrawBuffers(3, drawBuffers.data());
    glViewport(0, 0, _shadowDepthTextureWidth, _shadowDepthTextureHeight);
    glClearDepth(1.f);
    glDepthFunc(GL_LEQUAL);
    glEnable(GL_DEPTH_TEST);
    glClearColor(0.f, 0.f, 0.f, 0.f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    //glEnable(GL_CULL_FACE);
    //checkGLError("begin() -- enabled cull face");
    //glCullFace(GL_FRONT);
    //checkGLError("begin() -- set cullface to front");
    //glEnable(GL_POLYGON_OFFSET_FILL);
    //checkGLError("begin() -- enabled polygon offset fill");
    //glPolygonOffset(2.5f, 10.f);
    //checkGLError("begin() -- set values for polygon offset");

    RenderData lightRenderData{
        *_lightCamera,
        data.time,
        data.renderBinMask,
        data.modelTransform
    };

    return lightRenderData;
}

void ShadowComponent::end() {
    if (_executeDepthTextureSave) {
        saveDepthBuffer();
        _executeDepthTextureSave = false;
    }

    // Restores system state
    glBindFramebuffer(GL_FRAMEBUFFER, _currentFBO);
    std::array<GLenum, 3> drawBuffers = {
        GL_COLOR_ATTACHMENT0,
        GL_COLOR_ATTACHMENT1,
        GL_COLOR_ATTACHMENT2
    };
    glDrawBuffers(3, drawBuffers.data());
    glViewport(_viewport[0], _viewport[1], _viewport[2], _viewport[3]);

    // Restores OpenGL Rendering State
    global::renderEngine->openglStateCache().resetColorState();
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
    global::renderEngine->openglStateCache().resetPolygonAndClippingState();

    if (_blendIsEnabled) {
        glEnable(GL_BLEND);
    }
}

void ShadowComponent::update(const UpdateData&) {
    ZoneScoped;

    SceneGraphNode* sun = global::renderEngine->scene()->sceneGraphNode("Sun");
    if (sun) {
        _sunPosition = sun->worldPosition();
    }
    else {
        _sunPosition = glm::dvec3(0.0);
    }
}

void ShadowComponent::createDepthTexture() {
    glGenTextures(1, &_shadowDepthTexture);
    updateDepthTexture();

    _shadowData.shadowDepthTexture = _shadowDepthTexture;
    //_shadowData.positionInLightSpaceTexture = _positionInLightSpaceTexture;
}

void ShadowComponent::createShadowFBO() {
    // Saves current FBO first
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &_currentFBO);

    glGenFramebuffers(1, &_shadowFBO);
    glBindFramebuffer(GL_FRAMEBUFFER, _shadowFBO);
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_DEPTH_ATTACHMENT,
        _shadowDepthTexture,
        0
    );

    std::array<GLenum, 3> drawBuffers = { GL_NONE, GL_NONE, GL_NONE };
    glDrawBuffers(3, drawBuffers.data());

    checkFrameBufferState("createShadowFBO()");

    // Restores system state
    glBindFramebuffer(GL_FRAMEBUFFER, _currentFBO);
}

void ShadowComponent::updateDepthTexture() const {
    glBindTexture(GL_TEXTURE_2D, _shadowDepthTexture);

    //glTexStorage2D(
    //    GL_TEXTURE_2D,
    //    1,
    //    GL_DEPTH_COMPONENT32F,
    //    _shadowDepthTextureWidth,
    //    _shadowDepthTextureHeight
    //);

    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_DEPTH_COMPONENT32F,
        _shadowDepthTextureWidth,
        _shadowDepthTextureHeight,
        0,
        GL_DEPTH_COMPONENT,
        GL_FLOAT,
        nullptr
    );

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
    glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, ShadowBorder.data());
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC, GL_LEQUAL);

    //glGenTextures(1, &_positionInLightSpaceTexture);
    //glBindTexture(GL_TEXTURE_2D, _positionInLightSpaceTexture);
    //glTexImage2D(
    //    GL_TEXTURE_2D,
    //    0,
    //    GL_RGB32F,
    //    _shadowDepthTextureWidth,
    //    _shadowDepthTextureHeight,
    //    0,
    //    GL_RGBA,
    //    GL_FLOAT,
    //    nullptr
    //);
    //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    glBindTexture(GL_TEXTURE_2D, 0);
}

void ShadowComponent::buildDDepthTexture() {
    glGenTextures(1, &_dDepthTexture);
    glBindTexture(GL_TEXTURE_2D, _dDepthTexture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_DEPTH_COMPONENT32F,
        1,
        1,
        0,
        GL_DEPTH_COMPONENT,
        GL_FLOAT,
        nullptr
    );

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC, GL_LEQUAL);

    glBindTexture(GL_TEXTURE_2D, 0);
}

void ShadowComponent::saveDepthBuffer() const {
    const int size = _shadowDepthTextureWidth * _shadowDepthTextureHeight;
    std::vector<GLubyte> buffer(size);

    glReadPixels(
        0,
        0,
        _shadowDepthTextureWidth,
        _shadowDepthTextureHeight,
        GL_DEPTH_COMPONENT,
        GL_UNSIGNED_BYTE,
        buffer.data()
    );

    std::fstream ppmFile;

    ppmFile.open("depthBufferShadowMapping.ppm", std::fstream::out);
    if (ppmFile.is_open()) {

        ppmFile << "P3\n";
        ppmFile << _shadowDepthTextureWidth << " " << _shadowDepthTextureHeight << '\n';
        ppmFile << "255\n";

        LDEBUG("Saving depth texture to file depthBufferShadowMapping.ppm");
        int k = 0;
        for (int i = 0; i < _shadowDepthTextureWidth; i++) {
            for (int j = 0; j < _shadowDepthTextureHeight; j++, k++) {
                const unsigned int val = static_cast<unsigned int>(buffer[k]);
                ppmFile << std::format("{0} {0} {0} ", val);
            }
            ppmFile << '\n';
        }

        ppmFile.close();
        LDEBUG("Texture saved to file depthBufferShadowMapping.ppm");
    }

    buffer.clear();

    std::vector<GLfloat> bBuffer(size * 4);

    glReadBuffer(GL_COLOR_ATTACHMENT3);
    glReadPixels(
        0,
        0,
        _shadowDepthTextureWidth,
        _shadowDepthTextureHeight,
        GL_RGBA,
        GL_FLOAT,
        bBuffer.data()
    );

    ppmFile.clear();

    ppmFile.open("positionBufferShadowMapping.ppm", std::fstream::out);
    if (ppmFile.is_open()) {

        ppmFile << "P3\n";
        ppmFile << _shadowDepthTextureWidth << " " << _shadowDepthTextureHeight << '\n';
        ppmFile << "255\n";

        LDEBUG("Saving texture position to positionBufferShadowMapping.ppm");

        float biggestValue = 0.f;

        int k = 0;
        for (int i = 0; i < _shadowDepthTextureWidth; i++) {
            for (int j = 0; j < _shadowDepthTextureHeight; j++) {
                biggestValue = bBuffer[k] > biggestValue ?
                    bBuffer[k] : biggestValue;
                k += 4;
            }
        }

        biggestValue /= 255.f;

        k = 0;
        for (int i = 0; i < _shadowDepthTextureWidth; i++) {
            for (int j = 0; j < _shadowDepthTextureHeight; j++) {
                ppmFile << static_cast<unsigned int>(bBuffer[k] / biggestValue) << " "
                    << static_cast<unsigned int>(bBuffer[k + 1] / biggestValue) << " "
                    << static_cast<unsigned int>(bBuffer[k + 2] / biggestValue) << " ";
                k += 4;
            }
            ppmFile << '\n';
        }

        ppmFile.close();

        LDEBUG("Texture saved to file positionBufferShadowMapping.ppm");
    }
}

bool ShadowComponent::isEnabled() const {
    return _enabled;
}

ShadowComponent::ShadowMapData ShadowComponent::shadowMapData() const {
    return _shadowData;
}

GLuint ShadowComponent::dDepthTexture() const {
    return _dDepthTexture;
}

} // namespace openspace
