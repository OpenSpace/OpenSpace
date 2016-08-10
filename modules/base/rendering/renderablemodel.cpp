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

// temporary includes (will fix as soon as I figure out how class hierarchy should work, 
//                     ie after I see model on screen)

// open space includes
#include <openspace/rendering/renderengine.h>
#include <modules/base/rendering/renderablemodel.h>
#include <modules/base/rendering/modelgeometry.h>
#include <openspace/engine/configurationmanager.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/scene/scenegraphnode.h>

#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>

#include <openspace/engine/openspaceengine.h>

#define _USE_MATH_DEFINES
#include <math.h>

namespace { 
    const std::string _loggerCat     = "RenderableModel";
    const std::string keySource      = "Rotation.Source";
    const std::string keyDestination = "Rotation.Destination";
    const std::string keyGeometry    = "Geometry";
    const std::string keyBody        = "Body";
    const std::string keyStart       = "StartTime";
    const std::string keyEnd         = "EndTime";
    const std::string keyFading      = "Shading.Fadeable";
    
    const std::string keyModelTransform = "Rotation.ModelTransform";
    //const std::string keyGhosting      = "Shading.Ghosting";

}

namespace openspace {

    RenderableModel::RenderableModel(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _colorTexturePath("colorTexture", "Color Texture")
    , _performFade("performFading", "Perform Fading", false)
    , _debugModelRotation("modelrotation", "Model Rotation", glm::vec3(0.f), glm::vec3(0.f), glm::vec3(360.f))
    , _fading("fading", "Fade", 0)
    , _programObject(nullptr)
    , _texture(nullptr)
    , _geometry(nullptr)
    , _alpha(1.f)
    //, _isGhost(false)
    , _performShading("performShading", "Perform Shading", true)
    , _frameCount(0)
{
    std::string name;
    bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
    ghoul_assert(success, "Name was not passed to RenderableModel");

    ghoul::Dictionary geometryDictionary;
    success = dictionary.getValue(keyGeometry, geometryDictionary);
    if (success) {
        geometryDictionary.setValue(SceneGraphNode::KeyName, name);
        _geometry = modelgeometry::ModelGeometry::createFromDictionary(geometryDictionary);
    }

    std::string texturePath = "";
    success = dictionary.getValue("Textures.Color", texturePath);
    if (success)
        _colorTexturePath = absPath(texturePath);

    addPropertySubOwner(_geometry);
    addProperty(_colorTexturePath);
    _colorTexturePath.onChange(std::bind(&RenderableModel::loadTexture, this));

    addProperty(_debugModelRotation);

    dictionary.getValue(keySource, _source);
    dictionary.getValue(keyDestination, _destination);
    if (dictionary.hasKeyAndValue<glm::dmat3>(keyModelTransform))
        dictionary.getValue(keyModelTransform, _modelTransform);
    else
        _modelTransform = glm::dmat3(1.f);
    dictionary.getValue(keyBody, _target);

    openspace::SpiceManager::ref().addFrame(_target, _source);

    //setBoundingSphere(pss(1.f, 9.f));
    addProperty(_performShading);

    if (dictionary.hasKeyAndValue<bool>(keyFading)) {
        bool fading;
        dictionary.getValue(keyFading, fading);
        _performFade = fading;
    }
    addProperty(_performFade);

    //if (dictionary.hasKeyAndValue<bool>(keyGhosting)) {
    //    bool ghosting;
    //    dictionary.getValue(keyGhosting, ghosting);
    //    _isGhost = ghosting;
    //}
}

bool RenderableModel::isReady() const {
    bool ready = true;
    ready &= (_programObject != nullptr);
    ready &= (_texture != nullptr);
    return ready;
}

bool RenderableModel::initialize() {
    bool completeSuccess = true;
    if (_programObject == nullptr) {
        // NH shader

        RenderEngine& renderEngine = OsEng.renderEngine();
        _programObject = renderEngine.buildRenderProgram("ModelProgram",
            "${MODULE_BASE}/shaders/model_vs.glsl",
            "${MODULE_BASE}/shaders/model_fs.glsl");

        if (!_programObject)
            return false;
    }

    loadTexture();

    completeSuccess &= (_texture != nullptr);
    completeSuccess &= _geometry->initialize(this); 
    completeSuccess &= !_source.empty();
    completeSuccess &= !_destination.empty();

    return completeSuccess;
}

bool RenderableModel::deinitialize() {
    if (_geometry) {
        _geometry->deinitialize();
        delete _geometry;
        _geometry = nullptr;
    }
    _texture = nullptr;


    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_programObject) {
        renderEngine.removeRenderProgram(_programObject);
        _programObject = nullptr;
    }

    return true;
}

void RenderableModel::render(const RenderData& data) {

    _programObject->activate();
    
    double lt;
    
    // Fade away if it does not have spice coverage
    double time = openspace::Time::ref().currentTime();
    bool targetPositionCoverage = openspace::SpiceManager::ref().hasSpkCoverage(_target, time);
    if (!targetPositionCoverage) {
        int frame = _frameCount % 180;

        float fadingFactor = static_cast<float>(sin((frame * M_PI) / 180));
        _alpha = 0.5f + fadingFactor * 0.5f;
    }
    else
        _alpha = 1.0f;

    // Fading
    if (_performFade && _fading > 0.f) {
        _fading = _fading - 0.01f;
    }
    else if (!_performFade && _fading < 1.f) {
        _fading = _fading + 0.01f;

    }



    // Calculate variables to be used as uniform variables in shader
    glm::dvec3 bodyPosition = data.positionVec3;

    // debug rotation controlled from GUI
    glm::mat4 unitMat4(1);
    glm::vec3 debugEulerRot = glm::radians(_debugModelRotation.value());
    glm::mat4 rotX = glm::rotate(unitMat4, debugEulerRot.x, glm::vec3(1, 0, 0));
    glm::mat4 rotY = glm::rotate(unitMat4, debugEulerRot.y, glm::vec3(0, 1, 0));
    glm::mat4 rotZ = glm::rotate(unitMat4, debugEulerRot.z, glm::vec3(0, 0, 1));
    glm::dmat4 debugModelRotation = rotX * rotY * rotZ;

    // Model transform and view transform needs to be in double precision
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), bodyPosition) * // Translation
        glm::dmat4(_stateMatrix) *  // Spice rotation
        debugModelRotation; // debug model rotation controlled from GUI
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;
    glm::vec3 directionToSun = glm::normalize(_sunPosition.vec3() - glm::vec3(bodyPosition));
    glm::vec3 directionToSunViewSpace = glm::mat3(data.camera.combinedViewMatrix()) * directionToSun;

    _programObject->setUniform("transparency", _alpha);
    _programObject->setUniform("directionToSunViewSpace", directionToSunViewSpace);
    _programObject->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
    _programObject->setUniform("projectionTransform", data.camera.projectionMatrix());
    _programObject->setUniform("performShading", _performShading);
    _programObject->setUniform("fading", _fading);

    _geometry->setUniforms(*_programObject);

    // Bind texture
    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _programObject->setUniform("texture1", unit);

    _geometry->render();

    // disable shader
    _programObject->deactivate();
}

void RenderableModel::update(const UpdateData& data) {
    if (_programObject->isDirty())
        _programObject->rebuildFromFile();
    double _time = data.time;

    //if (_isGhost){
    //    futureTime = openspace::ImageSequencer::ref().getNextCaptureTime();
    //    double remaining = openspace::ImageSequencer::ref().getNextCaptureTime() - data.time;
    //    double interval = openspace::ImageSequencer::ref().getIntervalLength();
    //    double t = 1.f - remaining / openspace::ImageSequencer::ref().getIntervalLength();
    //    if (interval > 60) {
    //        if (t < 0.8)
    //            _fading = static_cast<float>(t);
    //        else if (t >= 0.95)
    //            _fading = _fading - 0.5f;
    //    }
    //    else
    //        _fading = 0.f;
    //    _time = futureTime;
    //}

    // set spice-orientation in accordance to timestamp
    if (!_source.empty()) {
        _stateMatrix = SpiceManager::ref().positionTransformMatrix(_source, _destination, _time) * _modelTransform;
    }

    double  lt;
    glm::dvec3 p =
    openspace::SpiceManager::ref().targetPosition("SUN", _target, "GALACTIC", {}, _time, lt);
    _sunPosition = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);
}

void RenderableModel::loadTexture() {
    _texture = nullptr;
    if (_colorTexturePath.value() != "") {
        _texture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath(_colorTexturePath)));
        if (_texture) {
            LDEBUG("Loaded texture from '" << absPath(_colorTexturePath) << "'");
            _texture->uploadTexture();
            _texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
        }
    }
}

}  // namespace openspace
