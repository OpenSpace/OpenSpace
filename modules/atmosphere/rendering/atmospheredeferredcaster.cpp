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

#include <modules/atmosphere/rendering/atmospheredeferredcaster.h>

#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/programobject.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/spicemanager.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <glm/gtx/string_cast.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtx/transform.hpp>
#include <glm/gtx/vector_angle.hpp>
#include <glm/gtc/quaternion.hpp>

#include <sstream>
#include <fstream>

#define _USE_MATH_DEFINES
#include <math.h>


namespace {
    const std::string _loggerCat       = "AtmosphereDeferredcaster";
    const char* GlslDeferredcastPath   = "${MODULES}/atmosphere/shaders/deferred_test_fs.glsl";
    const char* GlslDeferredcastFSPath = "${MODULES}/atmosphere/shaders/deferred_test_fs.glsl";
    const char* GlslDeferredcastVsPath = "${MODULES}/atmosphere/shaders/atmosphere_deferred_vs.glsl";
}

namespace openspace {

AtmosphereDeferredcaster::AtmosphereDeferredcaster()
    : //_programObject(nullptr)
    _transmittanceProgramObject(nullptr)
    , _irradianceProgramObject(nullptr)
    , _irradianceSupTermsProgramObject(nullptr)
    , _inScatteringProgramObject(nullptr)
    , _inScatteringSupTermsProgramObject(nullptr)
    , _deltaEProgramObject(nullptr)
    , _irradianceFinalProgramObject(nullptr)
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
    , _atmosphereDepthTexture(0)
    , _atmosphereFBO(0)
    , _atmosphereRenderVAO(0)
    , _atmosphereRenderVBO(0)
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
    , _atmosphereCalculated(false)
    , _atmosphereEnabled(false)
    , _ozoneEnabled(false)
    , _atmosphereRadius(0.f)
    , _atmospherePlanetRadius(0.f)
    , _planetAverageGroundReflectance(0.f)
    , _rayleighHeightScale(0.f)
    , _ozoneHeightScale(0.f)
    , _mieHeightScale(0.f)
    , _miePhaseConstant(0.f)    
    , _rayleighScatteringCoeff(glm::vec3(0.f))
    , _ozoneExtinctionCoeff(glm::vec3(0.f))
    , _mieScatteringCoeff(glm::vec3(0.f))
    , _mieExtinctionCoeff(glm::vec3(0.f))
    , _ellipsoidRadii(glm::dvec3(0.0))
    , _sunRadianceIntensity(50.0f)
    , _exposureConstant(0.4f)
    , _gammaConstant(1.8f)
    , _renderableClass(NoRenderableClass)
    , _calculationTextureScale(1.0)
    , _saveCalculationTextures(false)


{}

AtmosphereDeferredcaster::~AtmosphereDeferredcaster() {}

void AtmosphereDeferredcaster::initialize()
{
    if (!_atmosphereCalculated) {
        preCalculateAtmosphereParam();
    }
}

void AtmosphereDeferredcaster::deinitialize()
{
    // TODO
    // Review if the programs should be part of the renderEngine.
    RenderEngine& renderEngine = OsEng.renderEngine();
//    if (_programObject) {
//        renderEngine.removeRenderProgram(_programObject);
//        _programObject = nullptr;
//    }

    if (_transmittanceProgramObject) {
        _transmittanceProgramObject = nullptr;
    }

    if (_irradianceProgramObject) {
        _irradianceProgramObject = nullptr;
    }

    if (_irradianceSupTermsProgramObject) {
        _irradianceSupTermsProgramObject = nullptr;
    }

    if (_inScatteringProgramObject) {
        _inScatteringProgramObject = nullptr;
    }

    if (_inScatteringSupTermsProgramObject) {
        _inScatteringSupTermsProgramObject = nullptr;
    }

    if (_deltaEProgramObject) {
        _deltaEProgramObject = nullptr;
    }

    if (_deltaSProgramObject) {
        _deltaSProgramObject = nullptr;
    }

    if (_deltaSSupTermsProgramObject) {
        _deltaSSupTermsProgramObject = nullptr;
    }

    if (_deltaJProgramObject) {
        _deltaJProgramObject = nullptr;
    }

    glDeleteTextures(1, &_transmittanceTableTexture);
    glDeleteTextures(1, &_irradianceTableTexture);
    glDeleteTextures(1, &_inScatteringTableTexture);
    glDeleteTextures(1, &_deltaETableTexture);
    glDeleteTextures(1, &_deltaSRayleighTableTexture);
    glDeleteTextures(1, &_deltaSMieTableTexture);
    glDeleteTextures(1, &_deltaJTableTexture);
    glDeleteTextures(1, &_atmosphereTexture);

}

void AtmosphereDeferredcaster::preRaycast(const RenderData & renderData, const DeferredcastData& deferredData,
                                          ghoul::opengl::ProgramObject& program) 
{    
    program.setUniform("Rg", _atmospherePlanetRadius);
    program.setUniform("Rt", _atmosphereRadius);
    program.setUniform("AverageGroundReflectance", _planetAverageGroundReflectance);
    program.setUniform("HR", _rayleighHeightScale);
    program.setUniform("betaRayleigh", _rayleighScatteringCoeff);
    program.setUniform("HM", _mieHeightScale);
    program.setUniform("betaMieScattering", _mieScatteringCoeff);
    program.setUniform("betaMieExtinction", _mieExtinctionCoeff);
    program.setUniform("mieG", _miePhaseConstant);
    program.setUniform("sunRadiance", _sunRadianceIntensity);
    program.setUniform("ozoneLayerEnabled", (bool)_ozoneEnabled);
    program.setUniform("HO", _ozoneHeightScale);
    program.setUniform("betaOzoneExtinction", _ozoneExtinctionCoeff);

    program.setUniform("exposure", _exposureConstant);
    program.setUniform("gamma", _gammaConstant);
    program.setUniform("RenderableClass", static_cast<int>(_renderableClass));
    
    program.setUniform("TRANSMITTANCE_W", (int)_transmittance_table_width);
    program.setUniform("TRANSMITTANCE_H", (int)_transmittance_table_height);
    program.setUniform("SKY_W", (int)_irradiance_table_width);
    program.setUniform("SKY_H", (int)_irradiance_table_height);
    program.setUniform("OTHER_TEXTURES_W", (int)_delta_e_table_width);
    program.setUniform("OTHER_TEXTURES_H", (int)_delta_e_table_height);
    program.setUniform("SAMPLES_R", (int)_r_samples);
    program.setUniform("SAMPLES_MU", (int)_mu_samples);
    program.setUniform("SAMPLES_MU_S", (int)_mu_s_samples);
    program.setUniform("SAMPLES_NU", (int)_nu_samples);

    program.setUniform("ModelTransformMatrix", _modelTransform);
    program.setUniform("dInverseModelTransformMatrix", glm::inverse(_modelTransform));
    program.setUniform("dInverseOSEyeTransformMatrix", glm::inverse(renderData.camera.combinedViewMatrix()));

    GLenum err;
    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("After inverse matrices. OpenGL error: " << errString);
    }

    // Object Space
    //program.setUniform("inverseTransformMatrix", glm::inverse(_modelTransform));
    program.setUniform("dInverseTransformMatrix", glm::inverse(_modelTransform));

    // The following scale comes from PSC transformations.
    float fScaleFactor = renderData.camera.scaling().x * pow(10.0, renderData.camera.scaling().y);
    glm::dmat4 dfScaleCamTransf = glm::scale(glm::dvec3(fScaleFactor));
    program.setUniform("dInverseScaleTransformMatrix", glm::inverse(dfScaleCamTransf));

    // Object Space to World Space (in meters)
    //glm::dmat4 dObj2World = glm::translate(data.position.dvec3()) * glm::dmat4(transform);
    //glm::dmat4 dWorld2Obj = glm::inverse(dObj2World);
    //program.setUniform("dObjToWorldTransform", dObj2World);
    //program.setUniform("dWorldToObjectTransform", dWorld2Obj);

    // World to Eye Space in OS
    //glm::dmat4 dWorld2Eye = dfScaleCamTransf * renderData.camera.viewRotationMatrix() *
    //    glm::translate(-renderData.camera.positionVec3());
    //glm::dmat4 dEye2World = glm::inverse(dWorld2Eye);
    program.setUniform("dInverseCamRotTransform", glm::mat4_cast((glm::dquat)renderData.camera.rotationQuaternion()));
    //program.setUniform("dWorldToOsEyeTransform", dWorld2Eye);
    //program.setUniform("dOsEyeToWorldTransform", dEye2World);

    program.setUniform("dInverseSgctEyeToWorldTranform", glm::inverse(renderData.camera.combinedViewMatrix()));

    // Eye Space in OS to Eye Space in SGCT
    glm::dmat4 dOsEye2SGCTEye = glm::dmat4(renderData.camera.viewMatrix());
    glm::dmat4 dSgctEye2OSEye = glm::inverse(dOsEye2SGCTEye);
    
    //program.setUniform("dOsEyeToSGCTEyeTranform", dOsEye2SGCTEye);
    program.setUniform("dSgctEyeToOSEyeTranform", dSgctEye2OSEye);

    // Eye Space in SGCT to Projection (Clip) Space in SGCT
    glm::dmat4 dSgctEye2Clip = glm::dmat4(renderData.camera.projectionMatrix());
    glm::dmat4 dInverseProjection = glm::inverse(dSgctEye2Clip);

    //program.setUniform("dSgctEyeToClipTranform", dSgctEye2Clip);
    program.setUniform("dInverseSgctProjectionMatrix", dInverseProjection);
    //program.setUniform("dSgctProjectionMatrix", glm::dmat4(data.camera.projectionMatrix()));           

    program.setUniform("dObjpos", glm::dvec4(renderData.position.dvec3(), 1.0));
    program.setUniform("dCampos", renderData.camera.positionVec3());
    //program.setUniform("dCamrot", glm::dmat3(data.camera.viewRotationMatrix()));
    // I know it is (0,0,0). It is here just for sake of sanity. :-p
    double lt;
    glm::dvec3 sunPosWorld = SpiceManager::ref().targetPosition("SUN", "SUN", "GALACTIC", {}, _time, lt);
    glm::dvec4 sunPosObj = glm::dvec4(0.0);

    if (_renderableClass == RenderablePlanet) {
        sunPosObj = glm::inverse(_modelTransform) * 
            glm::dvec4(sunPosWorld - renderData.position.dvec3(), 1.0);
    }
    else if (_renderableClass == RenderableGlobe) {
        sunPosObj = glm::inverse(_modelTransform) * 
            glm::dvec4(sunPosWorld - renderData.modelTransform.translation, 1.0);
    }

    program.setUniform("ellipsoidRadii", _ellipsoidRadii);    

    //program.setUniform("sunPositionObj", sunPosObj);
    program.setUniform("sunDirectionObj", glm::normalize(glm::dvec3(sunPosObj)));
    //program.setUniform("_performShading", _performShading);

    _transmittanceTableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
    program.setUniform("transmittanceTexture", _transmittanceTableTextureUnit);

    _irradianceTableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D, _irradianceTableTexture);
    program.setUniform("irradianceTexture", _irradianceTableTextureUnit);

    _inScatteringTableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_3D, _inScatteringTableTexture);
    program.setUniform("inscatterTexture", _inScatteringTableTextureUnit); 

    // DEBUG:
    /*
    if (_renderableClass == RenderablePlanet) {
        glm::dvec3 objP = glm::dvec3(renderData.position[0] * pow(10, renderData.position[3]),
            renderData.position[1] * pow(10, renderData.position[3]), renderData.position[2] * pow(10, renderData.position[3]));
        glm::dvec4 cameraP = glm::inverse(_modelTransform) * glm::dvec4(-objP + renderData.camera.positionVec3(), 1.0);
        std::cout << "====== Planet's position in KM: " << glm::to_string( objP/glm::dvec3(1000.0, 1000.0, 1000.0) ) 
            << " =======" << std::endl;
        std::cout << "====== Distance from Planet's ground in KM: " 
            << glm::length(glm::dvec3(cameraP / glm::dvec4(1000.0, 1000.0, 1000.0, 1.0))) - _atmospherePlanetRadius 
            << " =======" << std::endl;
        std::cout << "====== Camera Position: " << glm::to_string(renderData.camera.positionVec3()) << " =====" << std::endl;        
    }
    else if (_renderableClass == RenderableGlobe) {
        glm::dvec3 objP = renderData.modelTransform.translation;
        glm::dvec4 cameraP = glm::inverse(_modelTransform) * glm::dvec4(renderData.camera.positionVec3(), 1);
        std::cout << "====== Planet's position in KM: " << glm::to_string( objP/glm::dvec3(1000.0, 1000.0, 1000.0) ) 
            << " =======" << std::endl;
        std::cout << "====== Distance from Planet's ground in KM: " 
            << glm::length(glm::dvec3(cameraP / glm::dvec4(1000.0, 1000.0, 1000.0, 1.0))) - _atmospherePlanetRadius 
            << " =======" << std::endl;
        std::cout << "====== Camera Position: " << glm::to_string(renderData.camera.positionVec3()) << " =====" << std::endl;
        std::cout << "--- Ellipsoid Radii: " << glm::to_string(_ellipsoidRadii) << " ----" << std::endl;

        // Testing Transformations:
        //glm::dvec4 tObjCoords = glm::dvec4(0.0); tObjCoords.w = 1.0;
        //std::cout << "==== Obj Coordinates:          " << glm::to_string(tObjCoords) << " ====" << std::endl;
        //glm::dvec4 tWorldCoords = _modelTransform * tObjCoords;
        //std::cout << "==== World Coordinates:        " << glm::to_string(tWorldCoords) << " ====" << std::endl;
        //glm::dvec4 tSGCTEyeCoords = renderData.camera.combinedViewMatrix() * tWorldCoords;
        //std::cout << "==== SGCT Eye Coordinates:     " << glm::to_string(tSGCTEyeCoords) << " ====" << std::endl;
        //glm::dvec4 tSGCTViewCoords = renderData.camera.projectionMatrix() * tSGCTEyeCoords;
        //std::cout << "==== SGCT View Coordinates:    " << glm::to_string(tSGCTViewCoords) << " ====" << std::endl;
        //glm::dvec4 tSGCTEyeCoordsInv = dInverseProjection * tSGCTViewCoords;
        //std::cout << "==== SGCT Eye Coordinates Inv: " << glm::to_string(tSGCTEyeCoords) << " ====" << std::endl;
        //glm::dvec4 tWorldCoordsInv = glm::inverse(renderData.camera.combinedViewMatrix()) * tSGCTEyeCoordsInv;
        //tWorldCoordsInv /= tWorldCoordsInv.w;
        //std::cout << "==== World Coordinates Inv:    " << glm::to_string(tWorldCoordsInv) << " ====" << std::endl;
        //glm::dvec4 tObjCoordsInv = glm::inverse(_modelTransform) * tWorldCoordsInv;
        //std::cout << "==== Obj Coordinates Inv:      " << glm::to_string(tObjCoordsInv) << " ====" << std::endl;

        
        ////glm::dmat4 cameraTranslation = glm::inverse(glm::translate(glm::dmat4(1.0), static_cast<glm::dvec3>(renderData.camera.positionVec3())));
        ////glm::dmat4 sgctViewMatrix = renderData.camera.viewMatrix();
        ////glm::dmat4 camRotationMatrix = glm::mat4_cast(glm::inverse((glm::dquat)renderData.camera.rotationQuaternion()));

        ////glm::dmat4 SGCTEyeToWorld = glm::inverse(cameraTranslation) * glm::inverse(camRotationMatrix) * glm::inverse(sgctViewMatrix);
        ////glm::dvec4 tWorldCoordsInv2 = SGCTEyeToWorld * tSGCTEyeCoordsInv;
        ////tWorldCoordsInv2 /= tWorldCoordsInv2.w;
        ////std::cout << "==== World Coordinates Inv2:   " << glm::to_string(tWorldCoordsInv2) << " ====" << std::endl;
        ////glm::dvec4 tObjCoordsInv2 = glm::inverse(_modelTransform) * tWorldCoordsInv2;
        ////std::cout << "==== Obj Coordinates Inv2:     " << glm::to_string(tObjCoordsInv2) << " ====" << std::endl;


        ////glm::dmat4 objTranslation = glm::translate(glm::dmat4(1.0), renderData.modelTransform.translation);
        ////glm::dmat4 objRotation = glm::dmat4(renderData.modelTransform.rotation);
        ////glm::dmat4 objScaling = glm::scale(glm::dmat4(1.0), glm::dvec3(renderData.modelTransform.scale,
        ////        renderData.modelTransform.scale, renderData.modelTransform.scale));

        ////glm::dmat4 modelTrans = objTranslation * objRotation * objScaling;

        ////std::cout << "==== Obj Coordinates Inv3:     " << glm::to_string(glm::inverse(modelTrans) * tWorldCoordsInv) << " ====" << std::endl;

        ////glm::dmat4 invModelTrans = glm::inverse(objScaling) * glm::inverse(objRotation) * glm::inverse(objTranslation);

        ////std::cout << "==== Obj Coordinates Inv3:     " << glm::to_string(invModelTrans * tWorldCoordsInv) << " ====" << std::endl;

        ////glm::dvec4 tmp = tWorldCoordsInv + glm::dvec4(-renderData.modelTransform.translation, 0.0);
        ////glm::dvec3 tmp2 = glm::dmat3(glm::transpose(objRotation)) * glm::dvec3(tmp);
        ////glm::dvec3 tmp3 = glm::dmat3(glm::inverse(objScaling)) * tmp2;

        ////std::cout << "==== Obj Coordinates Inv4:     " << glm::to_string(tmp3) << " ====" << std::endl;

        ////glm::dvec4 tmp4 = glm::inverse(sgctViewMatrix) * tSGCTEyeCoordsInv;
        //////glm::dvec4 tmp5 = glm::transpose(camRotationMatrix) * tmp4;
        ////glm::dvec4 tmp5 = glm::inverse(camRotationMatrix) * tmp4;
        ////glm::dvec3 tWorldCoordsInvHand = glm::dvec3(tmp5) - renderData.camera.positionVec3();
        ////std::cout << "==== World Coordinates Inv3:   " << glm::to_string(tWorldCoordsInvHand) << " ====" << std::endl;

        ////glm::dvec3 tmp6 = tWorldCoordsInvHand - renderData.modelTransform.translation;
        ////glm::dvec3 tmp7 = glm::dmat3(glm::transpose(objRotation)) * tmp6;
        ////glm::dvec3 tmp8 = glm::dmat3(glm::inverse(objScaling)) * tmp7;
        ////std::cout << "==== Obj Coordinates Inv5:     " << glm::to_string(tmp8) << " ====" << std::endl;
        
        //// Doing it by parts like in RenderablePlanet:
        //glm::dvec3 cameraPos  = renderData.camera.positionVec3();
        //glm::dmat4 invSGCTEye = glm::inverse(glm::dmat4(renderData.camera.viewMatrix()));
        //glm::dmat4 invCamRotationMatrix = glm::mat4_cast((glm::dquat)renderData.camera.rotationQuaternion());

        //glm::dvec4 tOSEyeCoordsInv = invSGCTEye * tSGCTEyeCoordsInv;
        //glm::dvec4 tmpRInv = invCamRotationMatrix * tOSEyeCoordsInv;
        //glm::dvec4 tOSWorldCoordsInv = glm::dvec4(glm::dvec3(tmpRInv) + cameraPos, 1.0);
        //std::cout << "==== World Coordinates Inv J:  " << glm::to_string(tOSWorldCoordsInv) << " ====" << std::endl;

        //glm::dmat3 objRotationMatInv = glm::transpose(glm::dmat3(renderData.modelTransform.rotation));
        //glm::dmat4 objScalingMatInv = glm::scale(glm::dvec3(1.0/renderData.modelTransform.scale));

        //glm::dvec4 objTrans = tOSWorldCoordsInv - glm::dvec4(renderData.modelTransform.translation, 0.0);        
        //glm::dvec4 objRot = glm::dvec4(objRotationMatInv * glm::dvec3(objTrans), 1.0);
        //glm::dvec4 objPosInvFinal = objScalingMatInv * objRot;
        //std::cout << "==== Obj Coordinates Inv J:    " << glm::to_string(objPosInvFinal) << " ====" << std::endl;


        //std::cout << "==== Obj Coordinates Inv J2:   " << glm::to_string(glm::inverse(_modelTransform) * tOSWorldCoordsInv) 
        //    << " ====" << std::endl;

        //std::cout << "\n\n---> ModelTrans: " << glm::to_string(_modelTransform) << std::endl;
        //std::cout << "\n\n---> ModelTrans2: " << glm::to_string(modelTrans) << std::endl;


    }
    */
    
}

void AtmosphereDeferredcaster::postRaycast(const RenderData & renderData, const DeferredcastData& deferredData,
                                           ghoul::opengl::ProgramObject& program)
{
    // Deactivate the texture units 
    _transmittanceTableTextureUnit.deactivate();
    _irradianceTableTextureUnit.deactivate();
    _inScatteringTableTextureUnit.deactivate();
}

std::string AtmosphereDeferredcaster::getDeferredcastPath() const {
    return GlslDeferredcastPath;
}

std::string AtmosphereDeferredcaster::getDeferredcastFSPath() const {
    return GlslDeferredcastFSPath;
}

std::string AtmosphereDeferredcaster::getDeferredcastVSPath() const {
    return GlslDeferredcastVsPath;
}

std::string AtmosphereDeferredcaster::getHelperPath() const {
    return ""; // no helper file
}

void AtmosphereDeferredcaster::setModelTransform(const glm::dmat4 &transform) {
    _modelTransform = transform;
}

void AtmosphereDeferredcaster::setTime(const double time) {
    _time = time;
}

void AtmosphereDeferredcaster::setAtmosphereRadius(const float atmRadius) {
    _atmosphereRadius = atmRadius;
}

void AtmosphereDeferredcaster::setPlanetRadius(const float planetRadius) {
    _atmospherePlanetRadius = planetRadius;
}

void AtmosphereDeferredcaster::setPlanetAverageGroundReflectance(const float averageGReflectance) {
    _planetAverageGroundReflectance = averageGReflectance;
}

void AtmosphereDeferredcaster::setRayleighHeightScale(const float rayleighHeightScale) {
    _rayleighHeightScale = rayleighHeightScale;
}

void AtmosphereDeferredcaster::enableOzone(const bool enable) {
    _ozoneEnabled = enable;
}

void AtmosphereDeferredcaster::setOzoneHeightScale(const float ozoneHeightScale) {
    _ozoneHeightScale = ozoneHeightScale;
}


void AtmosphereDeferredcaster::setMieHeightScale(const float mieHeightScale) {
    _mieHeightScale = mieHeightScale;
}

void AtmosphereDeferredcaster::setMiePhaseConstant(const float miePhaseConstant) {
    _miePhaseConstant = miePhaseConstant;
}

void AtmosphereDeferredcaster::setSunRadianceIntensity(const float sunRadiance) {
    _sunRadianceIntensity = sunRadiance;
}

void AtmosphereDeferredcaster::setHDRConstant(const float hdrConstant) {
    _exposureConstant = hdrConstant;
}

void AtmosphereDeferredcaster::setGammaConstant(const float gammaConstant) {
    _gammaConstant = gammaConstant;
}

void AtmosphereDeferredcaster::setRayleighScatteringCoefficients(const glm::vec3 & rayScattCoeff) {
    _rayleighScatteringCoeff = rayScattCoeff;
}

void AtmosphereDeferredcaster::setOzoneExtinctionCoefficients(const glm::vec3 & ozoneExtCoeff) {
    _ozoneExtinctionCoeff = ozoneExtCoeff;
}

void AtmosphereDeferredcaster::setMieScatteringCoefficients(const glm::vec3 & mieScattCoeff) {
    _mieScatteringCoeff = mieScattCoeff;
}

void AtmosphereDeferredcaster::setMieExtinctionCoefficients(const glm::vec3 & mieExtCoeff) {
    _mieExtinctionCoeff = mieExtCoeff;
}

void AtmosphereDeferredcaster::setEllipsoidRadii(const glm::dvec3 & radii) {
    _ellipsoidRadii = radii;
}

void AtmosphereDeferredcaster::setRenderableClass(const AtmosphereDeferredcaster::AtmospherRenderableClass rc)
{
    _renderableClass = rc;
}

void AtmosphereDeferredcaster::setPrecalculationTextureScale(const float _preCalculatedTexturesScale) {
    _calculationTextureScale = _preCalculatedTexturesScale;
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
    //preCalculateAtmosphereParam();
}

void AtmosphereDeferredcaster::enablePrecalculationTexturesSaving() {
    _saveCalculationTextures = true;
}

void AtmosphereDeferredcaster::loadComputationPrograms() {

    RenderEngine& renderEngine = OsEng.renderEngine();

    //============== Transmittance T =================
    if (_transmittanceProgramObject == nullptr) {
        _transmittanceProgramObject = ghoul::opengl::ProgramObject::Build(
            "transmittanceCalcProgram",
            "${MODULE_ATMOSPHERE}/shaders/transmittance_calc_vs.glsl",
            "${MODULE_ATMOSPHERE}/shaders/transmittance_calc_fs.glsl");
        if (!_transmittanceProgramObject) {
            return;
        }
    }
    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    _transmittanceProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _transmittanceProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    //============== Irradiance E =================
    if (_irradianceProgramObject == nullptr) {
        _irradianceProgramObject = ghoul::opengl::ProgramObject::Build(
            "irradianceCalcProgram",
            "${MODULE_ATMOSPHERE}/shaders/irradiance_calc_vs.glsl",
            "${MODULE_ATMOSPHERE}/shaders/irradiance_calc_fs.glsl");
        if (!_irradianceProgramObject) {
            if (_transmittanceProgramObject) {
                _transmittanceProgramObject.reset();
                _transmittanceProgramObject = nullptr;
            }

            return;
        }
    }
    _irradianceProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _irradianceProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    if (_irradianceSupTermsProgramObject == nullptr) {
        _irradianceSupTermsProgramObject = ghoul::opengl::ProgramObject::Build(
            "irradianceSupTermsCalcProgram",
            "${MODULE_ATMOSPHERE}/shaders/irradiance_sup_calc_vs.glsl",
            "${MODULE_ATMOSPHERE}/shaders/irradiance_sup_calc_fs.glsl");
        if (!_irradianceSupTermsProgramObject) {
            if (_transmittanceProgramObject) {
                _transmittanceProgramObject.reset();
                _transmittanceProgramObject = nullptr;
            }

            if (_irradianceProgramObject) {
                _irradianceProgramObject.reset();
                _irradianceProgramObject = nullptr;
            }

            return;
        }
    }
    _irradianceSupTermsProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _irradianceSupTermsProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    //============== InScattering S =================
    if (_inScatteringProgramObject == nullptr) {
        _inScatteringProgramObject = ghoul::opengl::ProgramObject::Build(
            "inScatteringCalcProgram",
            "${MODULE_ATMOSPHERE}/shaders/inScattering_calc_vs.glsl",
            "${MODULE_ATMOSPHERE}/shaders/inScattering_calc_fs.glsl",
            "${MODULE_ATMOSPHERE}/shaders/inScattering_calc_gs.glsl");
        if (!_inScatteringProgramObject) {
            if (_transmittanceProgramObject) {
                _transmittanceProgramObject.reset();
                _transmittanceProgramObject = nullptr;
            }

            if (_irradianceProgramObject) {
                _irradianceProgramObject.reset();
                _irradianceProgramObject = nullptr;
            }

            if (_irradianceSupTermsProgramObject) {
                _irradianceSupTermsProgramObject.reset();
                _irradianceSupTermsProgramObject = nullptr;
            }

            return;
        }
    }
    _inScatteringProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _inScatteringProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    if (_inScatteringSupTermsProgramObject == nullptr) {
        _inScatteringSupTermsProgramObject = ghoul::opengl::ProgramObject::Build(
            "inScatteringSupTermsCalcProgram",
            "${MODULE_ATMOSPHERE}/shaders/inScattering_sup_calc_vs.glsl",
            "${MODULE_ATMOSPHERE}/shaders/inScattering_sup_calc_fs.glsl",
            "${MODULE_ATMOSPHERE}/shaders/inScattering_sup_calc_gs.glsl");
        if (!_inScatteringSupTermsProgramObject) {
            if (_transmittanceProgramObject) {
                _transmittanceProgramObject.reset();
                _transmittanceProgramObject = nullptr;
            }

            if (_irradianceProgramObject) {
                _irradianceProgramObject.reset();
                _irradianceProgramObject = nullptr;
            }

            if (_irradianceSupTermsProgramObject) {
                _irradianceSupTermsProgramObject.reset();
                _irradianceSupTermsProgramObject = nullptr;
            }

            if (_inScatteringProgramObject) {
                _inScatteringProgramObject.reset();
                _inScatteringProgramObject = nullptr;
            }

            return;
        }
    }
    _inScatteringSupTermsProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _inScatteringSupTermsProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    //============== Delta E =================
    if (_deltaEProgramObject == nullptr) {
        _deltaEProgramObject = ghoul::opengl::ProgramObject::Build(
            "deltaECalcProgram",
            "${MODULE_ATMOSPHERE}/shaders/deltaE_calc_vs.glsl",
            "${MODULE_ATMOSPHERE}/shaders/deltaE_calc_fs.glsl");
        if (!_deltaEProgramObject) {
            if (_transmittanceProgramObject) {
                _transmittanceProgramObject.reset();
                _transmittanceProgramObject = nullptr;
            }

            if (_irradianceProgramObject) {
                _irradianceProgramObject.reset();
                _irradianceProgramObject = nullptr;
            }

            if (_irradianceSupTermsProgramObject) {
                _irradianceSupTermsProgramObject.reset();
                _irradianceSupTermsProgramObject = nullptr;
            }

            if (_inScatteringProgramObject) {
                _inScatteringProgramObject.reset();
                _inScatteringProgramObject = nullptr;
            }

            if (_inScatteringSupTermsProgramObject) {
                _inScatteringSupTermsProgramObject.reset();
                _inScatteringSupTermsProgramObject = nullptr;
            }

            return;
        }
    }
    _deltaEProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _deltaEProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    //============== Irradiance finel E =================
    if (_irradianceFinalProgramObject == nullptr) {
        _irradianceFinalProgramObject = ghoul::opengl::ProgramObject::Build(
            "irradianceEFinalProgram",
            "${MODULE_ATMOSPHERE}/shaders/irradiance_final_vs.glsl",
            "${MODULE_ATMOSPHERE}/shaders/irradiance_final_fs.glsl");
        if (!_irradianceFinalProgramObject) {
            if (_transmittanceProgramObject) {
                _transmittanceProgramObject.reset();
                _transmittanceProgramObject = nullptr;
            }

            if (_irradianceProgramObject) {
                _irradianceProgramObject.reset();
                _irradianceProgramObject = nullptr;
            }

            if (_irradianceSupTermsProgramObject) {
                _irradianceSupTermsProgramObject.reset();
                _irradianceSupTermsProgramObject = nullptr;
            }

            if (_inScatteringProgramObject) {
                _inScatteringProgramObject.reset();
                _inScatteringProgramObject = nullptr;
            }

            if (_inScatteringSupTermsProgramObject) {
                _inScatteringSupTermsProgramObject.reset();
                _inScatteringSupTermsProgramObject = nullptr;
            }

            if (_deltaEProgramObject) {
                _deltaEProgramObject.reset();
                _deltaEProgramObject = nullptr;
            }

            return;
        }
    }
    _irradianceFinalProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _irradianceFinalProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    //============== Delta S =================
    if (_deltaSProgramObject == nullptr) {
        _deltaSProgramObject = ghoul::opengl::ProgramObject::Build(
            "deltaSCalcProgram",
            "${MODULE_ATMOSPHERE}/shaders/deltaS_calc_vs.glsl",
            "${MODULE_ATMOSPHERE}/shaders/deltaS_calc_fs.glsl",
            "${MODULE_ATMOSPHERE}/shaders/deltaS_calc_gs.glsl");
        if (!_deltaSProgramObject) {
            if (_transmittanceProgramObject) {
                _transmittanceProgramObject.reset();
                _transmittanceProgramObject = nullptr;
            }

            if (_irradianceProgramObject) {
                _irradianceProgramObject.reset();
                _irradianceProgramObject = nullptr;
            }

            if (_irradianceSupTermsProgramObject) {
                _irradianceSupTermsProgramObject.reset();
                _irradianceSupTermsProgramObject = nullptr;
            }

            if (_inScatteringProgramObject) {
                _inScatteringProgramObject.reset();
                _inScatteringProgramObject = nullptr;
            }

            if (_inScatteringSupTermsProgramObject) {
                _inScatteringSupTermsProgramObject.reset();
                _inScatteringSupTermsProgramObject = nullptr;
            }

            if (_deltaEProgramObject) {
                _deltaEProgramObject.reset();
                _deltaEProgramObject = nullptr;
            }

            if (_irradianceFinalProgramObject) {
                _irradianceFinalProgramObject.reset();
                _irradianceFinalProgramObject = nullptr;
            }

            return;
        }
    }
    _deltaSProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _deltaSProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    if (_deltaSSupTermsProgramObject == nullptr) {
        _deltaSSupTermsProgramObject = ghoul::opengl::ProgramObject::Build(
            "deltaSSUPTermsCalcProgram",
            "${MODULE_ATMOSPHERE}/shaders/deltaS_sup_calc_vs.glsl",
            "${MODULE_ATMOSPHERE}/shaders/deltaS_sup_calc_fs.glsl",
            "${MODULE_ATMOSPHERE}/shaders/deltaS_sup_calc_gs.glsl");
        if (!_deltaSSupTermsProgramObject) {
            if (_transmittanceProgramObject) {
                _transmittanceProgramObject.reset();
                _transmittanceProgramObject = nullptr;
            }

            if (_irradianceProgramObject) {
                _irradianceProgramObject.reset();
                _irradianceProgramObject = nullptr;
            }

            if (_irradianceSupTermsProgramObject) {
                _irradianceSupTermsProgramObject.reset();
                _irradianceSupTermsProgramObject = nullptr;
            }

            if (_inScatteringProgramObject) {
                _inScatteringProgramObject.reset();
                _inScatteringProgramObject = nullptr;
            }

            if (_inScatteringSupTermsProgramObject) {
                _inScatteringSupTermsProgramObject.reset();
                _inScatteringSupTermsProgramObject = nullptr;
            }

            if (_deltaEProgramObject) {
                _deltaEProgramObject.reset();
                _deltaEProgramObject = nullptr;
            }

            if (_irradianceFinalProgramObject) {
                _irradianceFinalProgramObject.reset();
                _irradianceFinalProgramObject = nullptr;
            }

            if (_deltaSProgramObject) {
                _deltaSProgramObject.reset();
                _deltaSProgramObject = nullptr;
            }

            return;
        }
    }
    _deltaSSupTermsProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _deltaSSupTermsProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    //============== Delta J (Radiance Scattered) =================
    if (_deltaJProgramObject == nullptr) {
        _deltaJProgramObject = ghoul::opengl::ProgramObject::Build(
            "deltaJCalcProgram",
            "${MODULE_ATMOSPHERE}/shaders/deltaJ_calc_vs.glsl",
            "${MODULE_ATMOSPHERE}/shaders/deltaJ_calc_fs.glsl",
            "${MODULE_ATMOSPHERE}/shaders/deltaJ_calc_gs.glsl");
        if (!_deltaJProgramObject) {
            if (_transmittanceProgramObject) {
                _transmittanceProgramObject.reset();
                _transmittanceProgramObject = nullptr;
            }

            if (_irradianceProgramObject) {
                _irradianceProgramObject.reset();
                _irradianceProgramObject = nullptr;
            }

            if (_irradianceSupTermsProgramObject) {
                _irradianceSupTermsProgramObject.reset();
                _irradianceSupTermsProgramObject = nullptr;
            }

            if (_inScatteringProgramObject) {
                _inScatteringProgramObject.reset();
                _inScatteringProgramObject = nullptr;
            }

            if (_inScatteringSupTermsProgramObject) {
                _inScatteringSupTermsProgramObject.reset();
                _inScatteringSupTermsProgramObject = nullptr;
            }

            if (_deltaEProgramObject) {
                _deltaEProgramObject.reset();
                _deltaEProgramObject = nullptr;
            }

            if (_irradianceFinalProgramObject) {
                _irradianceFinalProgramObject.reset();
                _irradianceFinalProgramObject = nullptr;
            }

            if (_deltaSProgramObject) {
                _deltaSProgramObject.reset();
                _deltaSProgramObject = nullptr;
            }

            if (_deltaSSupTermsProgramObject) {
                _deltaSSupTermsProgramObject.reset();
                _deltaSSupTermsProgramObject = nullptr;
            }

            return;
        }

    }
    _deltaJProgramObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _deltaJProgramObject->setIgnoreUniformLocationError(IgnoreError::Yes);
}

void AtmosphereDeferredcaster::unloadComputationPrograms() {

    RenderEngine& renderEngine = OsEng.renderEngine();

    if (_transmittanceProgramObject) {
        _transmittanceProgramObject.reset();
        _transmittanceProgramObject = nullptr;
    }

    if (_irradianceProgramObject) {
        _irradianceProgramObject.reset();
        _irradianceProgramObject = nullptr;
    }

    if (_irradianceSupTermsProgramObject) {
        _irradianceSupTermsProgramObject.reset();
        _irradianceSupTermsProgramObject = nullptr;
    }

    if (_inScatteringProgramObject) {
        _inScatteringProgramObject.reset();
        _inScatteringProgramObject = nullptr;
    }

    if (_inScatteringSupTermsProgramObject) {
        _inScatteringSupTermsProgramObject.reset();
        _inScatteringSupTermsProgramObject = nullptr;
    }

    if (_deltaEProgramObject) {
        _deltaEProgramObject.reset();
        _deltaEProgramObject = nullptr;
    }

    if (_irradianceFinalProgramObject) {
        _irradianceFinalProgramObject.reset();
        _irradianceFinalProgramObject = nullptr;
    }

    if (_deltaSProgramObject) {
        _deltaSProgramObject.reset();
        _deltaSProgramObject = nullptr;
    }

    if (_deltaSSupTermsProgramObject) {
        _deltaSSupTermsProgramObject.reset();
        _deltaSSupTermsProgramObject = nullptr;
    }

    if (_deltaJProgramObject) {
        _deltaJProgramObject.reset();
        _deltaJProgramObject = nullptr;
    }
}

void AtmosphereDeferredcaster::createComputationTextures() {

    //========== Create Atmosphere Tables (textures) ==============

    GLenum err;
    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error before creating OpenGL textures for Atmosphere computation. OpenGL error: " << errString);
    }

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
        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error creating Transmittance T texture for Atmosphere computation. OpenGL error: " << errString);
        }
        //glBindTexture(GL_TEXTURE_2D, 0);

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

        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error creating Irradiance E texture for Atmosphere computation. OpenGL error: " << errString);
        }
        //glBindTexture(GL_TEXTURE_2D, 0);

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
        glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA32F_ARB, _mu_s_samples * _nu_samples,
            _mu_samples, _r_samples, 0, GL_RGB, GL_FLOAT, nullptr);

        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error creating InScattering S texture for Atmosphere computation. OpenGL error: " << errString);
        }
        //glBindTexture(GL_TEXTURE_3D, 0);
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

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error creating Irradiance Delta E texture for Atmosphere computation. OpenGL error: " << errString);
    }
    //glBindTexture(GL_TEXTURE_2D, 0);

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

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error creating Rayleigh InScattering Delta S exture for Atmosphere computation. OpenGL error: " << errString);
    }
    //glBindTexture(GL_TEXTURE_3D, 0);

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

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error creating Mie InScattering Delta S texture for Atmosphere computation. OpenGL error: " << errString);
    }
    //glBindTexture(GL_TEXTURE_3D, 0);

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

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error creating Inscattering Irradiance Delta J texture for Atmosphere computation. OpenGL error: " << errString);
    }
    //glBindTexture(GL_TEXTURE_3D, 0);
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

void AtmosphereDeferredcaster::executeCalculations(const GLuint quadCalcVAO,
                                                   const GLenum drawBuffers[1],
                                                   const GLsizei vertexSize)
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
    GLint blendEquationRGB;
    GLint blendEquationAlpha;
    GLint blendDestAlpha;
    GLint blendDestRGB;
    GLint blendSrcAlpha;
    GLint blendSrcRGB;

    if (blendEnabled)
        glDisable(GL_BLEND);
    glGetIntegerv(GL_BLEND_EQUATION_RGB, &blendEquationRGB);
    glGetIntegerv(GL_BLEND_EQUATION_ALPHA, &blendEquationAlpha);
    glGetIntegerv(GL_BLEND_DST_ALPHA, &blendDestAlpha);
    glGetIntegerv(GL_BLEND_DST_RGB, &blendDestRGB);
    glGetIntegerv(GL_BLEND_SRC_ALPHA, &blendSrcAlpha);
    glGetIntegerv(GL_BLEND_SRC_RGB, &blendSrcRGB);

    // ===========================================================
    // See Precomputed Atmosphere Scattering from Bruneton et al. paper, algorithm 4.1:
    // ===========================================================
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _transmittanceTableTexture, 0);
    checkFrameBufferState("_transmittanceTableTexture");
    glViewport(0, 0, _transmittance_table_width, _transmittance_table_height);
    _transmittanceProgramObject->activate();
    loadAtmosphereDataIntoShaderProgram(_transmittanceProgramObject);
    //glClear(GL_COLOR_BUFFER_BIT);
    static const float black[] = { 0.0f, 0.0f, 0.0f, 0.0f };
    glClearBufferfv(GL_COLOR, 0, black);
    renderQuadForCalc(quadCalcVAO, vertexSize);    
    if (_saveCalculationTextures) {
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, std::string("transmittance_texture.ppm"),
            _transmittance_table_width, _transmittance_table_height);
    }
    _transmittanceProgramObject->deactivate();
    GLenum err;
    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error computing Transmittance T Table. OpenGL error: " << errString);
    }

    // line 2 in algorithm 4.1
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _deltaETableTexture, 0);
    checkFrameBufferState("_deltaETableTexture");
    glViewport(0, 0, _delta_e_table_width, _delta_e_table_height);
    _irradianceProgramObject->activate();
    transmittanceTableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
    _irradianceProgramObject->setUniform("transmittanceTexture", transmittanceTableTextureUnit);
    loadAtmosphereDataIntoShaderProgram(_irradianceProgramObject);
    glClear(GL_COLOR_BUFFER_BIT);
    renderQuadForCalc(quadCalcVAO, vertexSize);
    if (_saveCalculationTextures) {
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, std::string("deltaE_table_texture.ppm"),
            _delta_e_table_width, _delta_e_table_height);
    }
    _irradianceProgramObject->deactivate();
    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error computing Irradiance Delta E Table. OpenGL error: " << errString);
    }

    // line 3 in algorithm 4.1
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _deltaSRayleighTableTexture, 0);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, _deltaSMieTableTexture, 0);
    GLenum colorBuffers[2] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
    glDrawBuffers(2, colorBuffers);
    checkFrameBufferState("_deltaSRay and _deltaSMie TableTexture");
    glViewport(0, 0, _mu_s_samples * _nu_samples, _mu_samples);
    _inScatteringProgramObject->activate();
    transmittanceTableTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
    _inScatteringProgramObject->setUniform("transmittanceTexture", transmittanceTableTextureUnit);
    loadAtmosphereDataIntoShaderProgram(_inScatteringProgramObject);
    glClear(GL_COLOR_BUFFER_BIT);
    for (int layer = 0; layer < static_cast<int>(_r_samples); ++layer) {
        step3DTexture(_inScatteringProgramObject, layer);
        renderQuadForCalc(quadCalcVAO, vertexSize);
    }
    if (_saveCalculationTextures) {
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, std::string("deltaS_rayleigh_texture.ppm"),
            _mu_s_samples * _nu_samples, _mu_samples);
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT1, std::string("deltaS_mie_texture.ppm"),
            _mu_s_samples * _nu_samples, _mu_samples);
    }
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, 0, 0);
    glDrawBuffers(1, drawBuffers);

    _inScatteringProgramObject->deactivate();
    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error computing InScattering Rayleigh and Mie Delta Tables. OpenGL error: " << errString);
    }

    // line 4 in algorithm 4.1
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _irradianceTableTexture, 0);
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
    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error computing Irradiance E Table. OpenGL error: " << errString);
    }

    // line 5 in algorithm 4.1
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _inScatteringTableTexture, 0);
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
    for (int layer = 0; layer < static_cast<int>(_r_samples); ++layer) {
        step3DTexture(_deltaSProgramObject, layer, false);
        renderQuadForCalc(quadCalcVAO, vertexSize);
    }
    if (_saveCalculationTextures) {
        saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, std::string("S_texture.ppm"),
            _mu_s_samples * _nu_samples, _mu_samples);
    }
    _deltaSProgramObject->deactivate();
    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error computing InScattering S Table. OpenGL error: " << errString);
    }

    // loop in line 6 in algorithm 4.1
    for (int scatteringOrder = 2; scatteringOrder <= 4; ++scatteringOrder) {

        // line 7 in algorithm 4.1
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _deltaJTableTexture, 0);
        checkFrameBufferState("_deltaJTableTexture");
        glViewport(0, 0, _mu_s_samples * _nu_samples, _mu_samples);
        _deltaJProgramObject->activate();
        if (scatteringOrder == 2)
            _deltaJProgramObject->setUniform("firstIteraction", 1);
        else
            _deltaJProgramObject->setUniform("firstIteraction", 0);
        transmittanceTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
        _deltaJProgramObject->setUniform("transmittanceTexture", transmittanceTableTextureUnit);
        deltaETableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_2D, _deltaETableTexture);
        _deltaJProgramObject->setUniform("deltaETexture", deltaETableTextureUnit);
        deltaSRayleighTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_3D, _deltaSRayleighTableTexture);
        _deltaJProgramObject->setUniform("deltaSRTexture", deltaSRayleighTableTextureUnit);
        deltaSMieTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_3D, _deltaSMieTableTexture);
        _deltaJProgramObject->setUniform("deltaSMTexture", deltaSMieTableTextureUnit);
        loadAtmosphereDataIntoShaderProgram(_deltaJProgramObject);
        for (int layer = 0; layer < static_cast<int>(_r_samples); ++layer) {
            step3DTexture(_deltaJProgramObject, layer);
            renderQuadForCalc(quadCalcVAO, vertexSize);
        }
        std::stringstream sst;
        if (_saveCalculationTextures) {
                sst << "deltaJ_texture-scattering_order-" << scatteringOrder << ".ppm";
                saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, sst.str(),
                    _mu_s_samples * _nu_samples, _mu_samples);
        }
        _deltaJProgramObject->deactivate();
        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error computing Delta J Table (Sup. Terms). OpenGL error: " << errString);
        }

        // line 8 in algorithm 4.1
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _deltaETableTexture, 0);
        checkFrameBufferState("_deltaETableTexture");
        glViewport(0, 0, _delta_e_table_width, _delta_e_table_height);
        _irradianceSupTermsProgramObject->activate();
        if (scatteringOrder == 2)
            _irradianceSupTermsProgramObject->setUniform("firstIteraction", (int)1);
        else
            _irradianceSupTermsProgramObject->setUniform("firstIteraction", (int)0);
        transmittanceTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
        _irradianceSupTermsProgramObject->setUniform("transmittanceTexture", transmittanceTableTextureUnit);
        deltaSRayleighTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_3D, _deltaSRayleighTableTexture);
        _irradianceSupTermsProgramObject->setUniform("deltaSRTexture", deltaSRayleighTableTextureUnit);
        deltaSMieTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_3D, _deltaSMieTableTexture);
        _irradianceSupTermsProgramObject->setUniform("deltaSMTexture", deltaSMieTableTextureUnit);
        loadAtmosphereDataIntoShaderProgram(_irradianceSupTermsProgramObject);
        renderQuadForCalc(quadCalcVAO, vertexSize);
        if (_saveCalculationTextures) {
                sst.str(std::string());
                sst << "deltaE_texture-scattering_order-" << scatteringOrder << ".ppm";
                saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, sst.str(),
                    _delta_e_table_width, _delta_e_table_height);
        }
        _irradianceSupTermsProgramObject->deactivate();
        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error computing Delta E Table (Sup. Terms). OpenGL error: " << errString);
        }

        // line 9 in algorithm 4.1
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _deltaSRayleighTableTexture, 0);
        checkFrameBufferState("_deltaSRayleighTableTexture");
        glViewport(0, 0, _mu_s_samples * _nu_samples, _mu_samples);
        _inScatteringSupTermsProgramObject->activate();
        /*if (scatteringOrder == 2)
            _inScatteringSupTermsProgramObject->setUniform("firstIteraction", (int)1);
        else
            _inScatteringSupTermsProgramObject->setUniform("firstIteraction", (int)0);*/
        transmittanceTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_2D, _transmittanceTableTexture);
        _inScatteringSupTermsProgramObject->setUniform("transmittanceTexture", transmittanceTableTextureUnit);
        deltaJTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_3D, _deltaJTableTexture);        
        _inScatteringSupTermsProgramObject->setUniform("deltaJTexture", deltaJTableTextureUnit);
        loadAtmosphereDataIntoShaderProgram(_inScatteringSupTermsProgramObject);
        for (int layer = 0; layer < static_cast<int>(_r_samples); ++layer) {
            step3DTexture(_inScatteringSupTermsProgramObject, layer);
            renderQuadForCalc(quadCalcVAO, vertexSize);
        }
        if (_saveCalculationTextures) {
                sst.str(std::string());
                sst << "deltaS_texture-scattering_order-" << scatteringOrder << ".ppm";
                saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, sst.str(),
                    _mu_s_samples * _nu_samples, _mu_samples);
        }
        _inScatteringSupTermsProgramObject->deactivate();
        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error computing Delta S Table (Sup. Terms). OpenGL error: " << errString);
        }

        glEnable(GL_BLEND);
        glBlendEquationSeparate(GL_FUNC_ADD, GL_FUNC_ADD);
        glBlendFuncSeparate(GL_ONE, GL_ONE, GL_ONE, GL_ONE);

        // line 10 in algorithm 4.1
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _irradianceTableTexture, 0);
        checkFrameBufferState("_irradianceTableTexture");
        glViewport(0, 0, _delta_e_table_width, _delta_e_table_height);
        _irradianceFinalProgramObject->activate();
        deltaETableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_2D, _deltaETableTexture);
        _irradianceFinalProgramObject->setUniform("deltaETexture", deltaETableTextureUnit);
        loadAtmosphereDataIntoShaderProgram(_irradianceFinalProgramObject);
        renderQuadForCalc(quadCalcVAO, vertexSize);
        if (_saveCalculationTextures) {
                sst.str(std::string());
                sst << "irradianceTable_order-" << scatteringOrder << ".ppm";
                saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, sst.str(),
                    _delta_e_table_width, _delta_e_table_height);
        }
        _irradianceFinalProgramObject->deactivate();
        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error computing E Table (Sup. Terms). OpenGL error: " << errString);
        }

        // line 11 in algorithm 4.1
        glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _inScatteringTableTexture, 0);
        checkFrameBufferState("_inScatteringTableTexture");
        glViewport(0, 0, _mu_s_samples * _nu_samples, _mu_samples);
        _deltaSSupTermsProgramObject->activate();
        deltaSRayleighTableTextureUnit.activate();
        glBindTexture(GL_TEXTURE_3D, _deltaSRayleighTableTexture);
        _deltaSSupTermsProgramObject->setUniform("deltaSTexture", deltaSRayleighTableTextureUnit);
        loadAtmosphereDataIntoShaderProgram(_deltaSSupTermsProgramObject);
        for (int layer = 0; layer < static_cast<int>(_r_samples); ++layer) {
            step3DTexture(_deltaSSupTermsProgramObject, layer, false);
            renderQuadForCalc(quadCalcVAO, vertexSize);
        }
        if (_saveCalculationTextures) {
                sst.str(std::string());
                sst << "inscatteringTable_order-" << scatteringOrder << ".ppm";
                saveTextureToPPMFile(GL_COLOR_ATTACHMENT0, sst.str(),
                    _mu_s_samples * _nu_samples, _mu_samples);
        }
        _deltaSSupTermsProgramObject->deactivate();
        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errString = gluErrorString(err);
            LERROR("Error computing S Table (Sup. Terms). OpenGL error: " << errString);
        }

        glDisable(GL_BLEND);
    }

    // Restores OpenGL blending state
    if (blendEnabled)
        glEnable(GL_BLEND);

    glBlendEquationSeparate(blendEquationRGB, blendEquationAlpha);
    glBlendFuncSeparate(blendSrcRGB, blendDestRGB, blendSrcAlpha, blendDestAlpha);

}

void AtmosphereDeferredcaster::preCalculateAtmosphereParam()
{
    std::stringstream ss;
    ss << "\n\n==== Atmosphere Values Used in Pre-Computation ====\n"
        << "Atmosphere Radius: " << _atmosphereRadius << std::endl
        << "Planet Radius: " << _atmospherePlanetRadius << std::endl
        << "Average Reflection: " << _planetAverageGroundReflectance << std::endl
        << "Rayleigh HR: " << _rayleighHeightScale << std::endl
        << "Mie HR: " << _mieHeightScale << std::endl
        << "Mie G phase constant: " << _miePhaseConstant << std::endl
        << "Mie Extinction coeff: " << glm::to_string(_mieExtinctionCoeff) << std::endl
        << "Rayleigh Scattering coeff: " << glm::to_string(_rayleighScatteringCoeff) << std::endl
        << "Mie Scattering coeff: " << glm::to_string(_mieScatteringCoeff) << std::endl;
    std::cout << ss.str() << std::endl;

    //==========================================================
    //========= Load Shader Programs for Calculations ==========
    //==========================================================
    loadComputationPrograms();

    GLenum err;
    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error loading shader programs for Atmosphere computation. OpenGL error: " << errString);
    }

    //==========================================================
    //============ Create Textures for Calculations ============
    //==========================================================
    createComputationTextures();

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error creating textures for Atmosphere computation. OpenGL error: " << errString);
    }

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

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error creating FrameBuffer Object for Atmosphere pre-computation. OpenGL error: " << errString);
    }

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
    glViewport(m_viewport[0], m_viewport[1],
        m_viewport[2], m_viewport[3]);
    glDeleteBuffers(1, &quadCalcVBO);
    glDeleteVertexArrays(1, &quadCalcVAO);
    glDeleteFramebuffers(1, &calcFBO);

    LDEBUG("Ended precalculations for Atmosphere effects...");
}



void AtmosphereDeferredcaster::resetAtmosphereTextures()
{

}

void AtmosphereDeferredcaster::createRenderQuad(GLuint * vao, GLuint * vbo,
                                                const GLfloat size) {

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
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 4, reinterpret_cast<GLvoid*>(0));
    glEnableVertexAttribArray(0);

    glBindVertexArray(0);

    GLenum err;
    while ((err = glGetError()) != GL_NO_ERROR) {
        LERROR("Error creating vertexbuffer for computation. OpenGL error: " << err);
    }
}

void AtmosphereDeferredcaster::loadAtmosphereDataIntoShaderProgram(std::unique_ptr<ghoul::opengl::ProgramObject> & shaderProg) {
    shaderProg->setUniform("Rg", _atmospherePlanetRadius);
    shaderProg->setUniform("Rt", _atmosphereRadius);
    shaderProg->setUniform("AverageGroundReflectance", _planetAverageGroundReflectance);
    shaderProg->setUniform("HR", _rayleighHeightScale);
    shaderProg->setUniform("betaRayleigh", _rayleighScatteringCoeff);
    shaderProg->setUniform("HM", _mieHeightScale);
    shaderProg->setUniform("betaMieScattering", _mieScatteringCoeff);
    shaderProg->setUniform("betaMieExtinction", _mieExtinctionCoeff);
    shaderProg->setUniform("mieG", _miePhaseConstant);
    shaderProg->setUniform("sunRadiance", _sunRadianceIntensity);
    shaderProg->setUniform("exposure", _exposureConstant);
    shaderProg->setUniform("gamma", _gammaConstant);
    shaderProg->setUniform("RenderableClass", static_cast<int>(_renderableClass));
    shaderProg->setUniform("TRANSMITTANCE_W", (int)_transmittance_table_width);
    shaderProg->setUniform("TRANSMITTANCE_H", (int)_transmittance_table_height);
    shaderProg->setUniform("SKY_W", (int)_irradiance_table_width);
    shaderProg->setUniform("SKY_H", (int)_irradiance_table_height);
    shaderProg->setUniform("OTHER_TEXTURES_W", (int)_delta_e_table_width);
    shaderProg->setUniform("OTHER_TEXTURES_H", (int)_delta_e_table_height);
    shaderProg->setUniform("SAMPLES_R", (int)_r_samples);
    shaderProg->setUniform("SAMPLES_MU", (int)_mu_samples);
    shaderProg->setUniform("SAMPLES_MU_S", (int)_mu_s_samples);
    shaderProg->setUniform("SAMPLES_NU", (int)_nu_samples); 
    shaderProg->setUniform("ozoneLayerEnabled", _ozoneEnabled);
    shaderProg->setUniform("HO", _ozoneHeightScale);
    shaderProg->setUniform("betaOzoneExtinction", _ozoneExtinctionCoeff);
}

void AtmosphereDeferredcaster::checkFrameBufferState(const std::string & codePosition) const {
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
            LERROR("Returned if the value of GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE is GL_NONE \
        for any color attachment point(s) named by GL_DRAW_BUFFERi.");
            break;
        case GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER:
            LERROR("Returned if GL_READ_BUFFER is not GL_NONE and the value of \
            GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE is GL_NONE for the color attachment point \
            named by GL_READ_BUFFER.");
            break;
        case GL_FRAMEBUFFER_UNSUPPORTED:
            LERROR("Returned if the combination of internal formats of the attached images \
            violates an implementation - dependent set of restrictions.");
            break;
        case GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE:
            LERROR("Returned if the value of GL_RENDERBUFFE_r_samples is not the same for all \
        attached renderbuffers; if the value of GL_TEXTURE_SAMPLES is the not same for all \
        attached textures; or , if the attached images are a mix of renderbuffers and textures, \
        the value of GL_RENDERBUFFE_r_samples does not match the value of GL_TEXTURE_SAMPLES.");
            LERROR("Returned if the value of GL_TEXTURE_FIXED_SAMPLE_LOCATIONS is not the same \
        for all attached textures; or , if the attached images are a mix of renderbuffers and \
        textures, the value of GL_TEXTURE_FIXED_SAMPLE_LOCATIONS is not GL_TRUE for all attached textures.");
            break;
        case GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS:
            LERROR("Returned if any framebuffer attachment is layered, and any populated attachment \
        is not layered, or if all populated color attachments are not from textures of the same target.");
            break;
        default:
            LDEBUG("No error found checking framebuffer: " + codePosition);
            break;
        }
    }
}

void AtmosphereDeferredcaster::renderQuadForCalc(const GLuint vao, const GLsizei numberOfVertices)
{
    glBindVertexArray(vao);
    glDrawArrays(GL_TRIANGLES, 0, numberOfVertices);
    glBindVertexArray(0);
}

void AtmosphereDeferredcaster::step3DTexture(std::unique_ptr<ghoul::opengl::ProgramObject> & shaderProg,
                                             const int layer, const bool doCalc)
{
    // See OpenGL redbook 8th Edition page 556 for Layered Rendering
    if (doCalc)
    {
        float earth2  = _atmospherePlanetRadius * _atmospherePlanetRadius;
        float atm2    = _atmosphereRadius * _atmosphereRadius;
        float diff    = atm2 - earth2;
        float ri      = static_cast<float>(layer) / static_cast<float>(_r_samples - 1);
        float ri_2    = ri * ri;
        float epsilon = (layer == 0) ? 0.01f : (layer == (static_cast<int>(_r_samples) - 1)) ? -0.001f : 0.0f;
        float r       = sqrtf(earth2 + ri_2 * diff) + epsilon;
        float dminG   = r - _atmospherePlanetRadius;
        float dminT   = _atmosphereRadius - r;
        float dh      = sqrtf(r * r - earth2);
        float dH      = dh + sqrtf(diff);

        shaderProg->setUniform("r", r);
        shaderProg->setUniform("dhdH", dminT, dH, dminG, dh);
    }

    shaderProg->setUniform("layer", static_cast<int>(layer));
}

void AtmosphereDeferredcaster::saveTextureToPPMFile(const GLenum color_buffer_attachment,
                                                    const std::string & fileName,
                                                    const int width, const int height) const {
    std::fstream ppmFile;

    ppmFile.open(fileName.c_str(), std::fstream::out);
    if (ppmFile.is_open()) {
        unsigned char * pixels = new unsigned char[width*height * 3];
        for (int t = 0; t < width*height * 3; ++t)
            pixels[t] = 255;

        // check OpenGL error
        GLenum err;
        while ((err = glGetError()) != GL_NO_ERROR) {
            const GLubyte * errorString = gluErrorString(err);

            std::cout << "\n\nBefore Reading Texture from card. OpenGL error: "
                << err << " - " << errorString << std::endl;
        }

        if (color_buffer_attachment != GL_DEPTH_ATTACHMENT) {
            glReadBuffer(color_buffer_attachment);
            glReadPixels(0, 0, width, height, GL_RGB, GL_UNSIGNED_BYTE, pixels);

        }
        else {
            glReadPixels(0, 0, width, height, GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE, pixels);
        }

        ppmFile << "P3" << std::endl;
        ppmFile << width << " " << height << std::endl;
        ppmFile << "255" << std::endl;

        std::cout << "\n\nFILE\n\n";
        int k = 0;
        for (int i = 0; i < width; i++) {
            for (int j = 0; j < height; j++) {
                ppmFile << (unsigned int)pixels[k] << " " << (unsigned int)pixels[k + 1] << " " << (unsigned int)pixels[k + 2] << " ";
                k += 3;
            }
            ppmFile << std::endl;
        }
        delete[] pixels;

        ppmFile.close();
    }
}


} // openspace
