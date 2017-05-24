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

#include <modules/space/rendering/renderableplanet.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

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
#include <ghoul/misc/invariants.h>

#include <memory>
#include <fstream>

#define _USE_MATH_DEFINES
#include <math.h>


namespace {
    const char* KeyGeometry = "Geometry";
    const char* KeyRadius = "Radius";
    const char* KeyColorTexture = "Textures.Color";
    const char* KeyNightTexture = "Textures.Night";
    const char* KeyHeightTexture = "Textures.Height";
    const char* KeyShading = "PerformShading";

    
    
    static const std::string _loggerCat = "RenderablePlanet";

    const char* keyFrame                         = "Frame";
    const char* keyShadowGroup                   = "Shadow_Group";
    const char* keyShadowSource                  = "Source";
    const char* keyShadowCaster                  = "Caster";
    const char* keyBody                          = "Body";
} // namespace

namespace openspace {

documentation::Documentation RenderablePlanet::Documentation() {
    using namespace documentation;
    return {
        "RenderablePlanet",
        "space_renderable_planet",
        {
            {
                KeyGeometry,
                new ReferencingVerifier("space_geometry_planet"),
                "Specifies the planet geometry that is used for this RenderablePlanet.",
                Optional::No
            },
            {
                KeyRadius,
                new DoubleVerifier,
                "Specifies the radius of the planet. If this value is not specified, it "
                "will try to query the SPICE library for radius values.",
                Optional::Yes
            },
            {
                KeyColorTexture,
                new StringVerifier,
                "Specifies the color texture that is used for this RenderablePlanet.",
                Optional::Yes
            },
            {
                KeyHeightTexture,
                new StringVerifier,
                "Specifies the height texture that is used for this RenderablePlanet.",
                Optional::Yes
            },
            {
                KeyNightTexture,
                new StringVerifier,
                "Specifies the texture that is used for the night side of this "
                "RenderablePlanet.",
                Optional::Yes
            },
            {
                KeyShading,
                new BoolVerifier,
                "Specifies whether the planet should be rendered shaded by the Sun. If "
                "this value is 'false', any existing night texture will not be used. "
                "This value defaults to 'true'.",
                Optional::Yes
            }
        }
    };
}

RenderablePlanet::RenderablePlanet(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _colorTexturePath("colorTexture", "Color Texture")
    , _nightTexturePath("nightTexture", "Night Texture")
    , _heightMapTexturePath("heightMap", "Heightmap Texture")
    , _programObject(nullptr)
    , _texture(nullptr)
    , _nightTexture(nullptr)
    , _heightExaggeration("heightExaggeration", "Height Exaggeration", 1.f, 0.f, 10.f)
    , _geometry(nullptr)
    , _performShading("performShading", "Perform Shading", true)
    , _alpha(1.f)
    , _planetRadius(0.f)
    , _hasNightTexture(false)
    , _hasHeightTexture(false)
    , _shadowEnabled(false)
{
    ghoul_precondition(
        dictionary.hasKeyAndValue<std::string>(SceneGraphNode::KeyName),
        "RenderablePlanet needs the name to be specified"
    );

    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderablePlanet"
    );

    const std::string name = dictionary.value<std::string>(SceneGraphNode::KeyName);

    ghoul::Dictionary geomDict = dictionary.value<ghoul::Dictionary>(KeyGeometry);

    if (dictionary.hasKey(KeyRadius)) {
        // If the user specified a radius, we want to use this
        _planetRadius = dictionary.value<double>(KeyRadius);
    }
    else if (SpiceManager::ref().hasValue(name, "RADII") ) {
        // If the user didn't specfify a radius, but Spice has a radius, we can use this
        glm::dvec3 radius;
        SpiceManager::ref().getValue(name, "RADII", radius);
        radius *= 1000.0; // Spice gives radii in KM.
        std::swap(radius[1], radius[2]); // z is equivalent to y in our coordinate system
        geomDict.setValue(KeyRadius, radius);

        _planetRadius = (radius.x + radius.y + radius.z) / 3.0;
    }
    else {
        LERRORC("RenderablePlanet", "Missing radius specification");
    }

    _geometry = planetgeometry::PlanetGeometry::createFromDictionary(geomDict);

    if (dictionary.hasKey(KeyColorTexture)) {
        _colorTexturePath = absPath(dictionary.value<std::string>(KeyColorTexture));
    }

    if (dictionary.hasKey(KeyNightTexture)) {
        _hasNightTexture = true;
        _nightTexturePath = absPath(dictionary.value<std::string>(KeyNightTexture));
    }

    if (dictionary.hasKey(KeyHeightTexture)) {
        _hasHeightTexture = true;
        _heightMapTexturePath = absPath(dictionary.value<std::string>(KeyHeightTexture));
    }

    if (dictionary.hasKey(KeyShading)) {
        _performShading = dictionary.value<bool>(KeyShading);
    }

    addPropertySubOwner(_geometry.get());

    auto loadTextureCallback = [this]() {loadTexture(); };
    addProperty(_colorTexturePath);
    _colorTexturePath.onChange(loadTextureCallback);

    addProperty(_nightTexturePath);
    _nightTexturePath.onChange(loadTextureCallback);

    addProperty(_heightMapTexturePath);
    _heightMapTexturePath.onChange(loadTextureCallback);

    addProperty(_heightExaggeration);
    addProperty(_performShading);

    //================================================================
    //======== Reads Shadow (Eclipses) Entries in mod file ===========
    //================================================================
    ghoul::Dictionary shadowDictionary;
    bool success = dictionary.getValue(keyShadowGroup, shadowDictionary);
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
                float sourceRadius;
                ss.str(std::string());
                ss << keyShadowSource << sourceCounter << ".Radius";
                success = shadowDictionary.getValue(ss.str(), sourceRadius);
                if (success) {
                    sourceArray.push_back(std::pair< std::string, float>(
                        sourceName, sourceRadius));
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
                    float casterRadius;
                    ss.str(std::string());
                    ss << keyShadowCaster << casterCounter << ".Radius";
                    success = shadowDictionary.getValue(ss.str(), casterRadius);
                    if (success) {
                        casterArray.push_back(std::pair< std::string, float>(
                            casterName, casterRadius));
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
    } 
    else if (_programObject == nullptr && _shadowEnabled) {
        // shadow program
        _programObject = renderEngine.buildRenderProgram(
            "shadowProgram",
            "${MODULE_SPACE}/shaders/shadow_vs.glsl",
            "${MODULE_SPACE}/shaders/shadow_fs.glsl");
    } 
    else if (_programObject == nullptr && _hasNightTexture) {
        // Night texture program
        _programObject = renderEngine.buildRenderProgram(
            "nightTextureProgram",
            "${MODULE_SPACE}/shaders/nighttexture_vs.glsl",
            "${MODULE_SPACE}/shaders/nighttexture_fs.glsl");
    }
    else if (_programObject == nullptr) {
        // pscstandard
        _programObject = renderEngine.buildRenderProgram(
            "pscstandard",
            "${MODULE_SPACE}/shaders/renderableplanet_vs.glsl",
            "${MODULE_SPACE}/shaders/renderableplanet_fs.glsl");
    }

    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    _programObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _programObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error after load shading programs. OpenGL error: " << errString);
    }

    _geometry->initialize(this);

    // Deactivate any previously activated shader program.
    _programObject->deactivate();

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Shader Programs Creation. OpenGL error: " << errString);
    }
    
    loadTexture();

    return isReady();
}

bool RenderablePlanet::deinitialize() {
    if (_geometry) {
        _geometry->deinitialize();
        _geometry = nullptr;
    }

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_programObject) {
        renderEngine.removeRenderProgram(_programObject);
        _programObject = nullptr;
    }

    _geometry = nullptr;
    _texture = nullptr;
    _nightTexture = nullptr;

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

//    glm::mat4 ModelViewTrans = data.camera.viewMatrix() * scaleCamTrans *
//        translateCamTrans * translateObjTrans * glm::mat4(modelTransform);
    
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
    if (_hasNightTexture && _nightTexture) {
        nightUnit.activate();
        _nightTexture->bind();
        _programObject->setUniform("nightTex", nightUnit);
    }

    if (_hasHeightTexture && _heightMapTexture) {
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
    _time = data.time.j2000Seconds();
}

void RenderablePlanet::loadTexture() {
    _texture = nullptr;
    if (_colorTexturePath.value() != "") {
        _texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_colorTexturePath));
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
            _nightTexture = ghoul::io::TextureReader::ref().loadTexture(absPath(_nightTexturePath));
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
            _heightMapTexture = ghoul::io::TextureReader::ref().loadTexture(absPath(_heightMapTexturePath));
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