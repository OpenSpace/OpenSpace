/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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
#include <modules/base/rendering/renderableplanet.h>

#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <modules/base/rendering/planetgeometry.h>
#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>
#include <openspace/scene/scenegraphnode.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

#define _USE_MATH_DEFINES
#include <math.h>

namespace {
    const std::string _loggerCat = "RenderablePlanet";

    const std::string keyFrame = "Frame";
    const std::string keyGeometry = "Geometry";
    const std::string keyRadius = "Radius";
    const std::string keyShading = "PerformShading";
    const std::string keyShadowGroup = "Shadow_Group";
    const std::string keyShadowSource = "Source";
    const std::string keyShadowCaster = "Caster";

const std::string keyBody = "Body";
}

namespace openspace {

RenderablePlanet::RenderablePlanet(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _colorTexturePath("colorTexture", "Color Texture")
    , _programObject(nullptr)
    , _texture(nullptr)
    , _nightTexture(nullptr)
    , _geometry(nullptr)
    , _performShading("performShading", "Perform Shading", true)
    , _rotation("rotation", "Rotation", 0, 0, 360)
    , _alpha(1.f)
    , _planetRadius(0.f)
    , _nightTexturePath("")
    , _hasNightTexture(false)    
{
    std::string name;
    bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
    ghoul_assert(success,
            "RenderablePlanet need the '" << SceneGraphNode::KeyName<<"' be specified");

    //std::string path;
    //success = dictionary.getValue(constants::scenegraph::keyPathModule, path);
    //ghoul_assert(success,
    //        "RenderablePlanet need the '"<<constants::scenegraph::keyPathModule<<"' be specified");

    ghoul::Dictionary geometryDictionary;
    success = dictionary.getValue(keyGeometry, geometryDictionary);
    if (success) {
        geometryDictionary.setValue(SceneGraphNode::KeyName, name);
        //geometryDictionary.setValue(constants::scenegraph::keyPathModule, path);
        _geometry = planetgeometry::PlanetGeometry::createFromDictionary(geometryDictionary);

        glm::vec2 planetRadiusVec;
        success = geometryDictionary.getValue(keyRadius, planetRadiusVec);
        if (success)
            _planetRadius = planetRadiusVec[0] * glm::pow(10, planetRadiusVec[1]);
        else
            LWARNING("No Radius value expecified for " << name << " planet.");
    }

    dictionary.getValue(keyFrame, _frame);
    dictionary.getValue(keyBody, _target);
    if (_target != "")
        setBody(_target);

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

    addPropertySubOwner(_geometry);

    addProperty(_colorTexturePath);
    _colorTexturePath.onChange(std::bind(&RenderablePlanet::loadTexture, this));

    if (dictionary.hasKeyAndValue<bool>(keyShading)) {
        bool shading;
        dictionary.getValue(keyShading, shading);
        _performShading = shading;
    }

    addProperty(_performShading);
    // Mainly for debugging purposes @AA
    addProperty(_rotation);


    // Shadow data:
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
                        sourceName, sourceRadius[0] * glm::pow(10, sourceRadius[1])));
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
                            casterName, casterRadius[0] * glm::pow(10, casterRadius[1])));
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
            }
        }
    }

}

RenderablePlanet::~RenderablePlanet() {
}

bool RenderablePlanet::initialize() {
    RenderEngine& renderEngine = OsEng.renderEngine();

    if (_programObject == nullptr && !_shadowConfArray.empty() && _hasNightTexture) {
        // shadow program
        _programObject = renderEngine.buildRenderProgram(
            "shadowNightProgram",
            "${MODULE_BASE}/shaders/shadow_nighttexture_vs.glsl",
            "${MODULE_BASE}/shaders/shadow_nighttexture_fs.glsl");
        if (!_programObject)
            return false;
    } 
    else if (_programObject == nullptr && !_shadowConfArray.empty()) {
        // shadow program
        _programObject = renderEngine.buildRenderProgram(
            "shadowProgram",
            "${MODULE_BASE}/shaders/shadow_vs.glsl",
            "${MODULE_BASE}/shaders/shadow_fs.glsl");
        if (!_programObject)
            return false;
    } 
    else if (_programObject == nullptr && _hasNightTexture) {
        // Night texture program
        _programObject = renderEngine.buildRenderProgram(
            "nightTextureProgram",
            "${MODULE_BASE}/shaders/nighttexture_vs.glsl",
            "${MODULE_BASE}/shaders/nighttexture_fs.glsl");
        if (!_programObject) 
            return false;
    }
    else if (_programObject == nullptr) {
        // pscstandard
        _programObject = renderEngine.buildRenderProgram(
            "pscstandard",
            "${MODULE_BASE}/shaders/pscstandard_vs.glsl",
            "${MODULE_BASE}/shaders/pscstandard_fs.glsl");
        if (!_programObject) 
            return false;
    }
    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    _programObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);

    loadTexture();
    _geometry->initialize(this);

    return isReady();
}

bool RenderablePlanet::deinitialize() {
    if(_geometry) {
        _geometry->deinitialize();
        delete _geometry;
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

void RenderablePlanet::render(const RenderData& data)
{
    // activate shader
    _programObject->activate();

    // scale the planet to appropriate size since the planet is a unit sphere
    glm::mat4 transform = glm::mat4(1);
    
    //earth needs to be rotated for that to work.
    glm::mat4 rot = glm::rotate(transform, static_cast<float>(M_PI_2), glm::vec3(1, 0, 0));
    glm::mat4 roty = glm::rotate(transform, static_cast<float>(M_PI_2), glm::vec3(0, -1, 0));
    glm::mat4 rotProp = glm::rotate(transform, glm::radians(static_cast<float>(_rotation)), glm::vec3(0, 1, 0));

    for (int i = 0; i < 3; i++){
        for (int j = 0; j < 3; j++){
            transform[i][j] = static_cast<float>(_stateMatrix[i][j]);
        }
    }
    transform = transform * rot * roty * rotProp;

    // setup the data to the shader
    double  lt;
    glm::dvec3 p =
    SpiceManager::ref().targetPosition("SUN", _target, "GALACTIC", {}, _time, lt);
    p *= 1000.0; // from Km to m
    psc sun_pos = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);

    //_programObject->setUniform("light_dir", sun_pos.vec4());

    glm::dvec3 pt =
    SpiceManager::ref().targetPosition(_target, "SUN", "GALACTIC", {}, _time, lt);
    psc tmppos = PowerScaledCoordinate::CreatePowerScaledCoordinate(pt.x, pt.y, pt.z);
    glm::vec3 cam_dir = glm::normalize(data.camera.position().vec3() - tmppos.vec3());

    // This is camera position vector (camera direction) in world coordinates.
    //_programObject->setUniform("cam_dir", cam_dir);

    //glm::mat4 modelview = data.camera.viewMatrix()*data.camera.modelMatrix();
    //glm::vec3 camSpaceEye = (-(modelview*data.position.vec4())).xyz;
    //_programObject->setUniform("camdir", camSpaceEye);

    _programObject->setUniform("transparency", _alpha);
    _programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _programObject->setUniform("ModelTransform", transform);
    setPscUniforms(*_programObject.get(), data.camera, data.position);
    
    _programObject->setUniform("_performShading", _performShading);

    // Bind texture
    ghoul::opengl::TextureUnit dayUnit;
    dayUnit.activate();
    _texture->bind();
    _programObject->setUniform("texture1", dayUnit);

    // Bind possible night texture
    if (_hasNightTexture) {
        ghoul::opengl::TextureUnit nightUnit;
        nightUnit.activate();
        _nightTexture->bind();
        _programObject->setUniform("nightTex", nightUnit);
    }

    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    // Shadow calculations..
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
                        
            ShadowRenderingStruct shadowData;
            shadowData.isShadowing = false;
            if ((d_test - rp_test) < _planetRadius) {
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

void RenderablePlanet::update(const UpdateData& data){
    // set spice-orientation in accordance to timestamp
    _stateMatrix = SpiceManager::ref().positionTransformMatrix(_frame, "GALACTIC", data.time);
    _time = data.time;
}

void RenderablePlanet::loadTexture() {
    _texture = nullptr;
    if (_colorTexturePath.value() != "") {
        _texture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath(_colorTexturePath)));
        if (_texture) {
            LDEBUG("Loaded texture from '" << _colorTexturePath << "'");
            _texture->uploadTexture();

            // Textures of planets looks much smoother with AnisotropicMipMap rather than linear
            // TODO: AnisotropicMipMap crashes on ATI cards ---abock
            //_texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            _texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
        }
    }
    if (_hasNightTexture) {
        _nightTexture = nullptr;
        if (_nightTexturePath != "") {
            _nightTexture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath(_nightTexturePath)));
            if (_nightTexture) {
                LDEBUG("Loaded texture from '" << _nightTexturePath << "'");
                _nightTexture->uploadTexture();
                _nightTexture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
                //_nightTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            }
        }
    }
}

}  // namespace openspace
