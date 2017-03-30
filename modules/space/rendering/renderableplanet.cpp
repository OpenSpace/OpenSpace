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

// open space includes
#include <modules/space/rendering/renderableplanet.h>

#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <modules/space/rendering/planetgeometry.h>
#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>
#include <openspace/scene/scenegraphnode.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

#include <memory>
#include <fstream>

#define _USE_MATH_DEFINES
#include <math.h>


namespace {
    static const std::string _loggerCat = "RenderablePlanet";

    const char* keyFrame                         = "Frame";
    const char* keyGeometry                      = "Geometry";
    const char* keyRadius                        = "Radius";
    const char* keyShading                       = "PerformShading";
    const char* keyShadowGroup                   = "Shadow_Group";
    const char* keyShadowSource                  = "Source";
    const char* keyShadowCaster                  = "Caster";
    const char* keyPlanetRadius                  = "PlanetRadius";
    const char* keyBody                          = "Body";
}

namespace openspace {

RenderablePlanet::RenderablePlanet(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _colorTexturePath("colorTexture", "Color Texture")
    , _nightTexturePath("nightTexture", "Night Texture")
    , _heightMapTexturePath("heightMap", "Heightmap Texture")
    , _heightExaggeration("heightExaggeration", "Height Exaggeration", 1.f, 0.f, 10.f)
    , _programObject(nullptr)
    , _texture(nullptr)
    , _nightTexture(nullptr)
    , _geometry(nullptr)
    , _performShading("performShading", "Perform Shading", true)
    , _rotation("rotation", "Rotation", 0, 0, 360)
    , _alpha(1.f)
    , _planetRadius(0.f)
    , _hasNightTexture(false)
    , _hasHeightTexture(false)
    , _shadowEnabled(false)
{
    std::string name;
    bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
    ghoul_assert(
        success,
        std::string("RenderablePlanet need the '") + SceneGraphNode::KeyName +
            "' be specified"
    );

    ghoul::Dictionary geometryDictionary;
    success = dictionary.getValue(keyGeometry, geometryDictionary);
    if (success) {
        geometryDictionary.setValue(SceneGraphNode::KeyName, name);
        _geometry = planetgeometry::PlanetGeometry::createFromDictionary(geometryDictionary);

        glm::vec2 planetRadiusVec;
        success = geometryDictionary.getValue(keyRadius, planetRadiusVec);
        if (success) {
            _planetRadius = static_cast<float>(
                planetRadiusVec[0] * glm::pow(10, planetRadiusVec[1])
            );
        }
        else {
            LWARNING("No Radius value expecified for " << name << " planet.");
        }
    }

    //===============================================================
    //======== Reads Body and Frame Entries in mod file =============
    //===============================================================
    dictionary.getValue(keyFrame, _frame);
    dictionary.getValue(keyBody, _target);


    //============================================================
    //======== Reads the Texture Entries in mod file =============
    //============================================================
    // TODO: textures need to be replaced by a good system similar to the geometry as soon
    // as the requirements are fixed (ab)
    std::string texturePath = "";
    success = dictionary.getValue("Textures.Color", texturePath);
    if (success)
        _colorTexturePath = absPath(texturePath);

    std::string nightTexturePath = "";
    dictionary.getValue("Textures.Night", nightTexturePath);
    if (nightTexturePath != ""){
        _hasNightTexture = true;
        _nightTexturePath = absPath(nightTexturePath);
    }
    
    std::string heightMapTexturePath = "";
    dictionary.getValue("Textures.Height", heightMapTexturePath);
    if (heightMapTexturePath != "") {
        _hasHeightTexture = true;
        _heightMapTexturePath = absPath(heightMapTexturePath);
    }

    addPropertySubOwner(_geometry.get());

    addProperty(_colorTexturePath);
    _colorTexturePath.onChange(std::bind(&RenderablePlanet::loadTexture, this));

    addProperty(_nightTexturePath);
    _nightTexturePath.onChange(std::bind(&RenderablePlanet::loadTexture, this));

    addProperty(_heightMapTexturePath);
    _heightMapTexturePath.onChange(std::bind(&RenderablePlanet::loadTexture, this));

    addProperty(_heightExaggeration);


    //=========================================================
    //======== Shading and Rotation as Properties =============
    //=========================================================
    if (dictionary.hasKeyAndValue<bool>(keyShading)) {
        bool shading;
        dictionary.getValue(keyShading, shading);
        _performShading = shading;
    }

    addProperty(_performShading);
    // Mainly for debugging purposes @AA
    addProperty(_rotation);

    //================================================================
    //======== Reads Shadow (Eclipses) Entries in mod file ===========
    //================================================================
    ghoul::Dictionary shadowDictionary;
    success = dictionary.getValue(keyShadowGroup, shadowDictionary);
    bool disableShadows = false;
    if (success) {
        std::vector< std::pair<std::string, float > > sourceArray;
        unsigned int sourceCounter = 1;
        while (success) {
            std::string sourceName;
            std::stringstream ss;
            ss << keyShadowSource << sourceCounter << ".Name";
            success = shadowDictionary.getValue(ss.str(), sourceName);
            if (success) {
                glm::vec2 sourceRadius;
                ss.str(std::string());
                ss << keyShadowSource << sourceCounter << ".Radius";
                success = shadowDictionary.getValue(ss.str(), sourceRadius);
                if (success) {
                    sourceArray.push_back(std::pair< std::string, float>(
                        sourceName, sourceRadius[0] * pow(10.f, sourceRadius[1])));
                }
                else {
                    LWARNING("No Radius value expecified for Shadow Source Name " 
                        << sourceName << " from " << name 
                        << " planet.\nDisabling shadows for this planet.");
                    disableShadows = true;
                    break;
                }
            }
            sourceCounter++;
        }

        if (!disableShadows && !sourceArray.empty()) {
            success = true;
            std::vector< std::pair<std::string, float > > casterArray;
            unsigned int casterCounter = 1;
            while (success) {
                std::string casterName;
                std::stringstream ss;
                ss << keyShadowCaster << casterCounter << ".Name";
                success = shadowDictionary.getValue(ss.str(), casterName);
                if (success) {
                    glm::vec2 casterRadius;
                    ss.str(std::string());
                    ss << keyShadowCaster << casterCounter << ".Radius";
                    success = shadowDictionary.getValue(ss.str(), casterRadius);
                    if (success) {
                        casterArray.push_back(std::pair< std::string, float>(
                            casterName, casterRadius[0] * pow(10.f, casterRadius[1])));
                    }
                    else {
                        LWARNING("No Radius value expecified for Shadow Caster Name "
                            << casterName << " from " << name
                            << " planet.\nDisabling shadows for this planet.");
                        disableShadows = true;
                        break;
                    }
                }

                casterCounter++;
            }

            if (!disableShadows && (!sourceArray.empty() && !casterArray.empty())) {
                for (const auto & source : sourceArray)
                    for (const auto & caster : casterArray) {
                        ShadowConf sc;
                        sc.source = source;
                        sc.caster = caster;
                        _shadowConfArray.push_back(sc);
                    }
                _shadowEnabled = true;
            }
        }
    }
}

RenderablePlanet::~RenderablePlanet() {
}

bool RenderablePlanet::initialize() {
    RenderEngine& renderEngine = OsEng.renderEngine();

    GLenum err;
    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Checking System State before initialization. OpenGL error: " << errString);
    }

    //===================================================================
    //=========== Defines the shading program to be executed ============
    //===================================================================
    if (_programObject == nullptr && _shadowEnabled && _hasNightTexture) {
        // shadow program
        _programObject = renderEngine.buildRenderProgram(
            "shadowNightProgram",
            "${MODULE_SPACE}/shaders/shadow_nighttexture_vs.glsl",
            "${MODULE_SPACE}/shaders/shadow_nighttexture_fs.glsl");
        if (!_programObject)
            return false;
    } 
    else if (_programObject == nullptr && _shadowEnabled) {
        // shadow program
        _programObject = renderEngine.buildRenderProgram(
            "shadowProgram",
            "${MODULE_SPACE}/shaders/shadow_vs.glsl",
            "${MODULE_SPACE}/shaders/shadow_fs.glsl");
        if (!_programObject)
            return false;
    } 
    else if (_programObject == nullptr && _hasNightTexture) {
        // Night texture program
        _programObject = renderEngine.buildRenderProgram(
            "nightTextureProgram",
            "${MODULE_SPACE}/shaders/nighttexture_vs.glsl",
            "${MODULE_SPACE}/shaders/nighttexture_fs.glsl");
        if (!_programObject) 
            return false;
    }
    else if (_programObject == nullptr) {
        // pscstandard
        _programObject = renderEngine.buildRenderProgram(
            "pscstandard",
            "${MODULE_SPACE}/shaders/renderableplanet_vs.glsl",
            "${MODULE_SPACE}/shaders/renderableplanet_fs.glsl");
        if (!_programObject)
            return false;
    }

    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    _programObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _programObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error after load shading programs. OpenGL error: " << errString);
    }

    //===================================================================
    //=========== Load textures defined in mod file to GPU ==============
    //===================================================================
    loadTexture();

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error loading textures. OpenGL error: " << errString);
    }
    
    //========================================================================
    //======== Initialize the current geometry (SimpleSphereGeometry) ========
    //========================================================================
    _geometry->initialize(this);    

    // Deactivate any previously activated shader program.
    _programObject->deactivate();

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Shader Programs Creation. OpenGL error: " << errString);
    }

    return isReady();
}

bool RenderablePlanet::deinitialize() {
    if(_geometry) {
        _geometry->deinitialize();
        _geometry = nullptr;
    }

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_programObject) {
        renderEngine.removeRenderProgram(_programObject);
        _programObject = nullptr;
    }

    _geometry                   = nullptr;
    _texture                    = nullptr;
    _nightTexture               = nullptr;

    return true;
}

bool RenderablePlanet::isReady() const {
    bool ready = true;
    ready &= (_programObject != nullptr);
    ready &= (_texture != nullptr);
    ready &= (_geometry != nullptr);
    return ready;
}

void RenderablePlanet::render(const RenderData& data) {
    // activate shader
    _programObject->activate();

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)));

    // scale the planet to appropriate size since the planet is a unit sphere
    //glm::mat4 transform = glm::mat4(1);
    
    //earth needs to be rotated for that to work.
    glm::dmat4 rot = glm::rotate(glm::dmat4(1.0), M_PI_2, glm::dvec3(1, 0, 0));
    glm::dmat4 roty = glm::rotate(glm::dmat4(1.0), M_PI_2, glm::dvec3(0, -1, 0));
    //glm::dmat4 rotProp = glm::rotate(glm::dmat4(1.0), glm::radians(static_cast<double>(_rotation)), glm::dvec3(0, 1, 0));
    modelTransform = modelTransform * rot * roty /** rotProp*/;

    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;
    
    _programObject->setUniform("transparency", _alpha);
    _programObject->setUniform(
        "modelViewProjectionTransform", 
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform)
    );
    _programObject->setUniform("ModelTransform", glm::mat4(modelTransform));

    // Normal Transformation
    glm::mat4 translateObjTrans = glm::translate(glm::mat4(1.0), data.position.vec3());
    glm::mat4 translateCamTrans = glm::translate(glm::mat4(1.0), -data.camera.position().vec3());
    float scaleFactor = data.camera.scaling().x * powf(10.0, data.camera.scaling().y);
    glm::mat4 scaleCamTrans = glm::scale(glm::mat4(1.0), glm::vec3(scaleFactor));

    glm::mat4 ModelViewTrans = data.camera.viewMatrix() * scaleCamTrans *
        translateCamTrans * translateObjTrans * glm::mat4(modelTransform);
    
    setPscUniforms(*_programObject.get(), data.camera, data.position);
    
    _programObject->setUniform("_performShading", _performShading);
    _programObject->setUniform("_hasHeightMap", _hasHeightTexture);
    _programObject->setUniform("_heightExaggeration", _heightExaggeration);

    // Bind texture
    ghoul::opengl::TextureUnit dayUnit;
    ghoul::opengl::TextureUnit nightUnit;
    ghoul::opengl::TextureUnit heightUnit;

    dayUnit.activate();
    _texture->bind();
    _programObject->setUniform("texture1", dayUnit);

    // Bind possible night texture
    if (_hasNightTexture) {
        nightUnit.activate();
        _nightTexture->bind();
        _programObject->setUniform("nightTex", nightUnit);
    }

    if (_hasHeightTexture) {
        heightUnit.activate();
        _heightMapTexture->bind();
        _programObject->setUniform("heightTex", heightUnit);
    }

    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    //=============================================================================
    //============= Eclipse Shadow Calculations and Uniforms Loading ==============
    //=============================================================================
    // TODO: Move Calculations to VIEW SPACE (let's avoid precision problems...)
    double lt;
    if (!_shadowConfArray.empty()) {
        std::vector<ShadowRenderingStruct> shadowDataArray;
        shadowDataArray.reserve(_shadowConfArray.size());

        for (const auto & shadowConf : _shadowConfArray) {
            // TO REMEMBER: all distances and lengths in world coordinates are in meters!!! We need to move this to view space...
            // Getting source and caster:
            glm::dvec3 sourcePos = SpiceManager::ref().targetPosition(shadowConf.source.first, "SUN", "GALACTIC", {}, _time, lt);
            sourcePos           *= 1000.0; // converting to meters
            glm::dvec3 casterPos = SpiceManager::ref().targetPosition(shadowConf.caster.first, "SUN", "GALACTIC", {}, _time, lt);
            casterPos           *= 1000.0; // converting to meters
            psc caster_pos       = PowerScaledCoordinate::CreatePowerScaledCoordinate(casterPos.x, casterPos.y, casterPos.z);
            
            // First we determine if the caster is shadowing the current planet (all calculations in World Coordinates):
            glm::vec3 planetCasterVec   = (caster_pos - data.position).vec3();
            glm::vec3 sourceCasterVec   = glm::vec3(casterPos - sourcePos);
            float sc_length             = glm::length(sourceCasterVec);
            glm::vec3 planetCaster_proj = (glm::dot(planetCasterVec, sourceCasterVec) / (sc_length*sc_length)) * sourceCasterVec;
            float d_test                = glm::length(planetCasterVec - planetCaster_proj);
            float xp_test               = shadowConf.caster.second * sc_length / (shadowConf.source.second + shadowConf.caster.second);
            float rp_test               = shadowConf.caster.second * (glm::length(planetCaster_proj) + xp_test) / xp_test;
                        
            double casterDistSun = glm::length(casterPos);
            float planetDistSun = glm::length(data.position.vec3());

            ShadowRenderingStruct shadowData;
            shadowData.isShadowing = false;

            if ( ((d_test - rp_test) < _planetRadius) &&
                 (casterDistSun < planetDistSun) ) {
                // The current caster is shadowing the current planet
                shadowData.isShadowing       = true;
                shadowData.rs                = shadowConf.source.second;
                shadowData.rc                = shadowConf.caster.second;
                shadowData.sourceCasterVec   = sourceCasterVec;
                shadowData.xp                = xp_test;
                shadowData.xu                = shadowData.rc * sc_length / (shadowData.rs - shadowData.rc);
                shadowData.casterPositionVec = glm::vec3(casterPos);
            }
            shadowDataArray.push_back(shadowData);
        }

        const std::string uniformVarName("shadowDataArray[");
        unsigned int counter = 0;
        for (const auto & sd : shadowDataArray) {
            std::stringstream ss;
            ss << uniformVarName << counter << "].isShadowing";
            _programObject->setUniform(ss.str(), sd.isShadowing);
            if (sd.isShadowing) {
                ss.str(std::string());
                ss << uniformVarName << counter << "].xp";
                _programObject->setUniform(ss.str(), sd.xp);
                ss.str(std::string());
                ss << uniformVarName << counter << "].xu";
                _programObject->setUniform(ss.str(), sd.xu);
                /*ss.str(std::string());
                ss << uniformVarName << counter << "].rs";
                _programObject->setUniform(ss.str(), sd.rs);*/
                ss.str(std::string());
                ss << uniformVarName << counter << "].rc";
                _programObject->setUniform(ss.str(), sd.rc);
                ss.str(std::string());
                ss << uniformVarName << counter << "].sourceCasterVec";
                _programObject->setUniform(ss.str(), sd.sourceCasterVec);
                ss.str(std::string());
                ss << uniformVarName << counter << "].casterPositionVec";
                _programObject->setUniform(ss.str(), sd.casterPositionVec);
            }
            counter++;
        }
    }

    // render
    _geometry->render();

    // disable shader
    _programObject->deactivate();
}

void RenderablePlanet::update(const UpdateData& data) {
    // set spice-orientation in accordance to timestamp
    _stateMatrix = data.modelTransform.rotation;
    //_stateMatrix = SpiceManager::ref().positionTransformMatrix(_frame, "GALACTIC", data.time);
    _time = data.time;
}

void RenderablePlanet::loadTexture() {
    _texture = nullptr;
    if (_colorTexturePath.value() != "") {
        _texture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath(_colorTexturePath)));
        if (_texture) {
            if (_texture->numberOfChannels() == 1) {
                _texture->setSwizzleMask({ GL_RED, GL_RED, GL_RED, GL_RED });
            }

            LDEBUG("Loaded texture from '" << _colorTexturePath << "'");
            _texture->uploadTexture();

            // Textures of planets looks much smoother with AnisotropicMipMap rather than linear
            // TODO: AnisotropicMipMap crashes on ATI cards ---abock
            //_texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            _texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
        }
    }

    GLenum err;
    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error after reading color texture. OpenGL error: " << errString);
    }

    if (_hasNightTexture) {
        _nightTexture = nullptr;
        if (_nightTexturePath.value() != "") {
            _nightTexture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath(_nightTexturePath)));
            if (_nightTexture) {
                LDEBUG("Loaded texture from '" << _nightTexturePath << "'");
                _nightTexture->uploadTexture();
                _nightTexture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
                //_nightTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            }
        }
    }

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error after reading night texture. OpenGL error: " << errString);
    }
    
    if (_hasHeightTexture) {
        _heightMapTexture = nullptr;
        if (_heightMapTexturePath.value() != "") {
            _heightMapTexture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath(_heightMapTexturePath)));
            if (_heightMapTexture) {
                LDEBUG("Loaded texture from '" << _heightMapTexturePath << "'");
                _heightMapTexture->uploadTexture();
                _heightMapTexture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
                //_nightTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            }
        }
    }

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error after reading height mapping texture. OpenGL error: " << errString);
    }
}
}  // namespace openspace