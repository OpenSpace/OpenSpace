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

#include <modules/globebrowsing/src/shadowcomponent.h>
#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/src/renderableglobe.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/updatestructures.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/moduleengine.h>

#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/interaction/keyframenavigator.h>

#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>

#include <openspace/util/camera.h>

#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

#include <glm/gtc/matrix_transform.hpp>

#include <fstream>
#include <cstdlib>
#include <locale>

namespace {
    constexpr const char* _loggerCat = "ShadowComponent";
    
    constexpr openspace::properties::Property::PropertyInfo SaveDepthTextureInfo = {
        "SaveDepthTextureInfo",
        "Save Depth Texture",
        "Debug"
    };

    constexpr openspace::properties::Property::PropertyInfo DistanceFractionInfo = {
        "DistanceFraction",
        "Distance Fraction",
        "Distance fraction of original distance from light source to the globe to be "
        "considered as the new light source distance."
    };

    void checkFrameBufferState(const std::string& codePosition)
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
} // namespace

namespace openspace {

    documentation::Documentation ShadowComponent::Documentation() {
        using namespace documentation;
        return {
            "ShadowsRing Component",
            "globebrowsing_shadows_component",
            {
                {
                    DistanceFractionInfo.identifier,
                    new DoubleVerifier,
                    Optional::Yes,
                    DistanceFractionInfo.description
                }
            }
        };
    }

    ShadowComponent::ShadowComponent(const ghoul::Dictionary& dictionary)
        : properties::PropertyOwner({ "Shadows" })		
        , _saveDepthTexture(SaveDepthTextureInfo)
        , _distanceFraction(DistanceFractionInfo, 3, 1, 10000)
        , _enabled({ "Enabled", "Enabled", "Enable/Disable Shadows" }, true)
        , _shadowMapDictionary(dictionary)
        , _shadowDepthTextureHeight(4096)
        , _shadowDepthTextureWidth(4096)
        , _shadowDepthTexture(-1)
        , _positionInLightSpaceTexture(-1)
        , _shadowFBO(-1)
        , _firstPassSubroutine(-1)
        , _secondPassSubroutine(1)
        , _defaultFBO(-1)
        , _sunPosition(0.0)
        , _shadowMatrix(1.0)
        , _executeDepthTextureSave(false)
    {
        using ghoul::filesystem::File;

        if (dictionary.hasKey("Rings")) {
            ghoul::Dictionary ringsDic;
            dictionary.getValue("Rings", ringsDic);
            if (ringsDic.hasKey("Shadows")) {
                ringsDic.getValue("Shadows", _shadowMapDictionary);
            }
        }

        documentation::testSpecificationAndThrow(
            Documentation(),
            _shadowMapDictionary,
            "ShadowComponent"
        );

        if (_shadowMapDictionary.hasKey(DistanceFractionInfo.identifier)) {
            _distanceFraction = static_cast<int>(
                _shadowMapDictionary.value<float>(DistanceFractionInfo.identifier)
                );
        }

        _saveDepthTexture.onChange([&]() {
            _executeDepthTextureSave = true;
        });

        addProperty(_enabled);
        addProperty(_saveDepthTexture);
        addProperty(_distanceFraction);
    }

    void ShadowComponent::initialize()
    {
        using ghoul::filesystem::File;
    }

    bool ShadowComponent::isReady() const {
        return true;
    }

    void ShadowComponent::initializeGL() {
        createDepthTexture();
        createShadowFBO();
    }

    void ShadowComponent::deinitializeGL() {
        glDeleteTextures(1, &_shadowDepthTexture);
        glDeleteTextures(1, &_positionInLightSpaceTexture);
        glDeleteFramebuffers(1, &_shadowFBO);
        checkGLError("ShadowComponent::deinitializeGL() -- Deleted Textures and Framebuffer");
    }

    RenderData ShadowComponent::begin(const RenderData& data) {
        // ===========================================
        // Builds light's ModelViewProjectionMatrix:
        // ===========================================
        
        glm::dvec3 diffVector = glm::dvec3(_sunPosition) - data.modelTransform.translation;
        double originalLightDistance = glm::length(diffVector);
        glm::dvec3 lightDirection = glm::normalize(diffVector);
        
        // Percentage of the original light source distance (to avoid artifacts)
        /*double multiplier = originalLightDistance * 
            (static_cast<double>(_distanceFraction)/1.0E5);*/

        double multiplier = originalLightDistance *
            (static_cast<double>(_distanceFraction) / 10000.0);
        
        // New light source position
        glm::dvec3 lightPosition = data.modelTransform.translation + 
            (lightDirection * multiplier);

        //// Light Position
        //glm::dvec3 lightPosition = glm::dvec3(_sunPosition);
       
        //=============== Manually Created Camera Matrix ===================
        //==================================================================
        // camera Z
        glm::dvec3 cameraZ = lightDirection;
        
        // camera X
        glm::dvec3 upVector = glm::dvec3(0.0, 1.0, 0.0);
        glm::dvec3 cameraX = glm::normalize(glm::cross(upVector, cameraZ)); 
        
        // camera Y
        glm::dvec3 cameraY = glm::cross(cameraZ, cameraX);

        // init 4x4 matrix
        glm::dmat4 cameraRotationMatrix(1.0);
        
        double* matrix = glm::value_ptr(cameraRotationMatrix);
        matrix[0]  = cameraX.x;
        matrix[4]  = cameraX.y;
        matrix[8]  = cameraX.z;
        matrix[1]  = cameraY.x;
        matrix[5]  = cameraY.y;
        matrix[9]  = cameraY.z;
        matrix[2]  = cameraZ.x;
        matrix[6]  = cameraZ.y;
        matrix[10] = cameraZ.z;

        // set translation part
        // We aren't setting the position here because it is set in 
        // the camera->setPosition()
        //matrix[12] = -glm::dot(cameraX, lightPosition);
        //matrix[13] = -glm::dot(cameraY, lightPosition);
        //matrix[14] = -glm::dot(cameraZ, lightPosition);

       
        _lightCamera = std::move(std::unique_ptr<Camera>(new Camera(data.camera)));
        _lightCamera->setPositionVec3(lightPosition);
        _lightCamera->setRotation(glm::dquat(glm::inverse(cameraRotationMatrix)));
        //=======================================================================
        //=======================================================================
        

        //============= Light Matrix by Camera Matrices Composition =============
        //=======================================================================
        glm::dmat4 lightProjectionMatrix = glm::dmat4(_lightCamera->projectionMatrix());
        
        // The model transformation missing in the final shadow matrix is add when rendering each
        // object (using its transformations provided by the RenderData structure)
        _shadowData.shadowMatrix = 
            _toTextureCoordsMatrix * 
            lightProjectionMatrix * 
            _lightCamera->combinedViewMatrix();

        
        // Saves current state
        glGetIntegerv(GL_FRAMEBUFFER_BINDING, &_defaultFBO);
        glGetIntegerv(GL_VIEWPORT, _mViewport);
        _faceCulling = glIsEnabled(GL_CULL_FACE);
        glGetIntegerv(GL_CULL_FACE_MODE, &_faceToCull);
        _polygonOffSet = glIsEnabled(GL_POLYGON_OFFSET_FILL);
        glGetFloatv(GL_POLYGON_OFFSET_FACTOR, &_polygonOffSetFactor);
        glGetFloatv(GL_POLYGON_OFFSET_UNITS, &_polygonOffSetUnits);
        glGetFloatv(GL_COLOR_CLEAR_VALUE, _colorClearValue);
        glGetFloatv(GL_DEPTH_CLEAR_VALUE, &_depthClearValue);
        _depthIsEnabled = glIsEnabled(GL_DEPTH_TEST);
        glGetIntegerv(GL_DEPTH_FUNC, &_depthFunction);
        _blendIsEnabled = glIsEnabled(GL_BLEND);


        glBindFramebuffer(GL_FRAMEBUFFER, _shadowFBO);
        GLenum drawBuffers[] = { GL_COLOR_ATTACHMENT0, GL_NONE, GL_NONE };
        glDrawBuffers(3, drawBuffers);
        glViewport(0, 0, _shadowDepthTextureWidth, _shadowDepthTextureHeight);
        glClearDepth(1.0f);
        glDepthFunc(GL_LEQUAL);
        glEnable(GL_DEPTH_TEST);
        glClearColor(0.0, 0.0, 0.0, 0.0);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        
        
        /*glEnable(GL_CULL_FACE);
        checkGLError("begin() -- enabled cull face");
        glCullFace(GL_FRONT);
        checkGLError("begin() -- set cullface to front");*/
        /*glEnable(GL_POLYGON_OFFSET_FILL);
        checkGLError("begin() -- enabled polygon offset fill");
        glPolygonOffset(2.5f, 10.0f);
        checkGLError("begin() -- set values for polygon offset");*/

        RenderData lightRenderData{
            *_lightCamera,
            data.time,
            data.doPerformanceMeasurement,
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
        glBindFramebuffer(GL_FRAMEBUFFER, _defaultFBO);
        GLenum drawBuffers[] = {
            GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1, GL_COLOR_ATTACHMENT2
        };
        glDrawBuffers(3, drawBuffers);
        glViewport(
            _mViewport[0],
            _mViewport[1],
            _mViewport[2],
            _mViewport[3]
        );
        
        if (_faceCulling) {
            glEnable(GL_CULL_FACE);
            glCullFace(_faceToCull);
        }
        else {
            glDisable(GL_CULL_FACE);
        }

        if (_depthIsEnabled) {
            glEnable(GL_DEPTH_TEST);
        }
        else {
            glDisable(GL_DEPTH_TEST);
        }

        glDepthFunc(_depthFunction);

        if (_polygonOffSet) {
            glEnable(GL_POLYGON_OFFSET_FILL);
            glPolygonOffset(_polygonOffSetFactor, _polygonOffSetUnits);
        }
        else {
            glDisable(GL_POLYGON_OFFSET_FILL);
        }

        glClearColor(
            _colorClearValue[0], 
            _colorClearValue[1], 
            _colorClearValue[2], 
            _colorClearValue[3]
        );
        glClearDepth(_depthClearValue);

        if (_blendIsEnabled) {
            glEnable(GL_BLEND);
        }
    }

    void ShadowComponent::update(const UpdateData& /*data*/) {
        _sunPosition = global::renderEngine.scene()->sceneGraphNode("Sun")->worldPosition();
    }

    void ShadowComponent::createDepthTexture() {
        glGenTextures(1, &_shadowDepthTexture);
        glBindTexture(GL_TEXTURE_2D, _shadowDepthTexture);
        glTexStorage2D(
            GL_TEXTURE_2D, 
            1, 
            GL_DEPTH_COMPONENT32F,
            _shadowDepthTextureWidth, 
            _shadowDepthTextureHeight
        );

        /*glTexImage2D(
            GL_TEXTURE_2D,
            0,
            GL_DEPTH_COMPONENT32F,
            _shadowDepthTextureWidth,
            _shadowDepthTextureHeight,
            0,
            GL_DEPTH_COMPONENT,
            GL_FLOAT,
            0
        );*/
        
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
        glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, ShadowBorder);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC, GL_LEQUAL);
        
        checkGLError("createDepthTexture() -- Depth testure created");

        /*glGenTextures(1, &_positionInLightSpaceTexture);
        glBindTexture(GL_TEXTURE_2D, _positionInLightSpaceTexture);        
        glTexImage2D(
            GL_TEXTURE_2D, 
            0, 
            GL_RGB32F, 
            _shadowDepthTextureWidth,
            _shadowDepthTextureHeight, 
            0, 
            GL_RGBA, 
            GL_FLOAT, 
            nullptr
        );
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);*/
        
        glBindTexture(GL_TEXTURE_2D, 0);

        _shadowData.shadowDepthTexture = _shadowDepthTexture;
        //_shadowData.positionInLightSpaceTexture = _positionInLightSpaceTexture;
    }

    void ShadowComponent::createShadowFBO() {
        // Saves current FBO first
        glGetIntegerv(GL_FRAMEBUFFER_BINDING, &_defaultFBO);

       
        glGenFramebuffers(1, &_shadowFBO);
        glBindFramebuffer(GL_FRAMEBUFFER, _shadowFBO);
        glFramebufferTexture(
            GL_FRAMEBUFFER, 
            GL_DEPTH_ATTACHMENT, 
            _shadowDepthTexture, 
            0
        );

        /*glFramebufferTexture(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            _positionInLightSpaceTexture,
            0
        );*/
        
        checkGLError("createShadowFBO() -- Created Shadow Framebuffer");
        //GLenum drawBuffers[] = { GL_COLOR_ATTACHMENT0, GL_NONE, GL_NONE };
        GLenum drawBuffers[] = { GL_NONE, GL_NONE, GL_NONE };
        glDrawBuffers(3, drawBuffers);

        checkFrameBufferState("createShadowFBO()");

        // Restores system state
        glBindFramebuffer(GL_FRAMEBUFFER, _defaultFBO);
    }

    void ShadowComponent::saveDepthBuffer() {
        int size = _shadowDepthTextureWidth * _shadowDepthTextureHeight;
        GLubyte * buffer = new GLubyte[size];

        glReadPixels(
            0,
            0,
            _shadowDepthTextureWidth,
            _shadowDepthTextureHeight,
            GL_DEPTH_COMPONENT,
            GL_UNSIGNED_BYTE,
            buffer
        );

        checkGLError("readDepthBuffer To buffer");
        std::fstream ppmFile;

        ppmFile.open("depthBufferShadowMapping.ppm", std::fstream::out);
        if (ppmFile.is_open()) {

            ppmFile << "P3" << std::endl;
            ppmFile << _shadowDepthTextureWidth << " " << _shadowDepthTextureHeight 
                    << std::endl;
            ppmFile << "255" << std::endl;

            std::cout << "\n\nSaving depth texture to file depthBufferShadowMapping.ppm\n\n";
            int k = 0;
            for (int i = 0; i < _shadowDepthTextureWidth; i++) {
                for (int j = 0; j < _shadowDepthTextureHeight; j++, k++) {
                    unsigned int val = static_cast<unsigned int>(buffer[k]);
                    ppmFile << val << " " << val << " " << val << " ";
                }
                ppmFile << std::endl;
            }

            ppmFile.close();

            std::cout << "Texture saved to file depthBufferShadowMapping.ppm\n\n";
        }

        delete[] buffer;

        GLfloat * bBuffer = new GLfloat[size * 4];

        glReadBuffer(GL_COLOR_ATTACHMENT3);
        glReadPixels(
            0,
            0,
            _shadowDepthTextureWidth,
            _shadowDepthTextureHeight,
            GL_RGBA,
            GL_FLOAT,
            bBuffer
        );

        checkGLError("readPositionBuffer To buffer");
        ppmFile.clear();

        ppmFile.open("positionBufferShadowMapping.ppm", std::fstream::out);
        if (ppmFile.is_open()) {

            ppmFile << "P3" << std::endl;
            ppmFile << _shadowDepthTextureWidth << " " << _shadowDepthTextureHeight 
                    << std::endl;
            ppmFile << "255" << std::endl;

            std::cout << "\n\nSaving texture position to positionBufferShadowMapping.ppm\n\n";

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
                ppmFile << std::endl;
            }

            ppmFile.close();

            std::cout << "Texture saved to file positionBufferShadowMapping.ppm\n\n";
        }

        delete[] bBuffer;
    }

    void ShadowComponent::checkGLError(const std::string & where) const {
        const GLenum error = glGetError();
        switch (error) {
        case GL_NO_ERROR:
            break;
        case GL_INVALID_ENUM:
            LERRORC(
                "OpenGL Invalid State",
                fmt::format("Function {}: GL_INVALID_ENUM", where)
            );
            break;
        case GL_INVALID_VALUE:
            LERRORC(
                "OpenGL Invalid State",
                fmt::format("Function {}: GL_INVALID_VALUE", where)
            );
            break;
        case GL_INVALID_OPERATION:
            LERRORC(
                "OpenGL Invalid State",
                fmt::format(
                    "Function {}: GL_INVALID_OPERATION", where
                ));
            break;
        case GL_INVALID_FRAMEBUFFER_OPERATION:
            LERRORC(
                "OpenGL Invalid State",
                fmt::format(
                    "Function {}: GL_INVALID_FRAMEBUFFER_OPERATION",
                    where
                )
            );
            break;
        case GL_OUT_OF_MEMORY:
            LERRORC(
                "OpenGL Invalid State",
                fmt::format("Function {}: GL_OUT_OF_MEMORY", where)
            );
            break;
        default:
            LERRORC(
                "OpenGL Invalid State",
                fmt::format("Unknown error code: {0:x}", static_cast<int>(error))
            );
        }
    }

    bool ShadowComponent::isEnabled() const {
        return _enabled;
    }

    ShadowComponent::ShadowMapData ShadowComponent::shadowMapData() const {
        return _shadowData;
    }
} // namespace openspace
