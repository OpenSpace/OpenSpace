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

#include <modules/galaxy/rendering/renderablegalaxy.h>
#include <modules/galaxy/rendering/galaxyraycaster.h>

#include <ghoul/io/texture/texturereader.h>


#include <openspace/rendering/renderable.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/raycastermanager.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <ghoul/opengl/ghoul_gl.h>

#include <modules/volume/rawvolumereader.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/texture.h>

#include <fstream>


namespace {
    const std::string GlslRayCastPath  = "${MODULES}/toyvolume/shaders/rayCast.glsl";
    const std::string GlslBoundsVsPath = "${MODULES}/toyvolume/shaders/boundsVs.glsl";
    const std::string GlslBoundsFsPath = "${MODULES}/toyvolume/shaders/boundsFs.glsl";
    const std::string _loggerCat       = "Renderable Galaxy";
}

namespace openspace {

    RenderableGalaxy::RenderableGalaxy(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _scalingExponent("scalingExponent", "Scaling Exponent", 1, -10, 20)
    , _stepSize("stepSize", "Step Size", 0.002, 0.001, 0.05)
    , _scaling("scaling", "Scaling", glm::vec3(1.0, 1.0, 1.0), glm::vec3(0.0), glm::vec3(10.0))
    , _translation("translation", "Translation", glm::vec3(0.0, 0.0, 0.0), glm::vec3(0.0), glm::vec3(10.0))
    , _rotation("rotation", "Euler rotation", glm::vec3(0.0, 0.0, 0.0), glm::vec3(0), glm::vec3(6.28)) {

    float scalingExponent, stepSize;
    glm::vec3 scaling, translation, rotation;
    glm::vec4 color;
    ghoul::Dictionary volumeDictionary, pointsDictionary;

    if (dictionary.getValue("ScalingExponent", scalingExponent)) {
        _scalingExponent = scalingExponent;
    }
    if (dictionary.getValue("Scaling", scaling)) {
        _scaling = scaling;
    }
    if (dictionary.getValue("Translation", translation)) {
        _translation = translation;
    }
    if (dictionary.getValue("Rotation", rotation)) {
        _rotation = rotation;
    }
    if (dictionary.getValue("StepSize", stepSize)) {
        _stepSize = stepSize;
    }
    if (dictionary.getValue("Volume", volumeDictionary)) {
        std::string volumeFilename;
        if (volumeDictionary.getValue("Filename", volumeFilename)) {
            _volumeFilename = absPath(volumeFilename);
        } else {
            LERROR("No volume filename specified.");
        }
        glm::vec3 volumeDimensions;
        if (volumeDictionary.getValue("Dimensions", volumeDimensions)) {
            _volumeDimensions = static_cast<glm::ivec3>(volumeDimensions);
        } else {
            LERROR("No volume dimensions specified.");
        }
    } else {
        LERROR("No volume dictionary specified.");
    }
    if (dictionary.getValue("Points", pointsDictionary)) {
        std::string pointsFilename;
        if (pointsDictionary.getValue("Filename", pointsFilename)) {
            _pointsFilename = absPath(pointsFilename);
        } else {
            LERROR("No points filename specified.");
        }
    } else {
        LERROR("No points dictionary specified.");
    }

}
    
RenderableGalaxy::~RenderableGalaxy() {}

bool RenderableGalaxy::initialize() {
    // Aspect is currently hardcoded to cubic voxels.
    _aspect = static_cast<glm::vec3>(_volumeDimensions);
    _aspect = _aspect / std::max(std::max(_aspect.x, _aspect.y), _aspect.z);

    RawVolumeReader<glm::tvec4<GLfloat>> reader(_volumeFilename, _volumeDimensions);
    _volume = reader.read();
    
    _texture = std::make_unique<ghoul::opengl::Texture>(
        _volumeDimensions,
        ghoul::opengl::Texture::Format::RGBA,
        GL_RGBA32F,
        GL_FLOAT,
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::Clamp);
   
    _texture->setPixelData(reinterpret_cast<char*>(_volume->data()), ghoul::opengl::Texture::TakeOwnership::No);
    _texture->setDimensions(_volume->dimensions());
    _texture->uploadTexture();

    _raycaster = std::make_unique<GalaxyRaycaster>(*_texture);
    _raycaster->initialize();

    OsEng.renderEngine().raycasterManager().attachRaycaster(*_raycaster.get());

    std::function<void(bool)> onChange = [&](bool enabled) {
        if (enabled) {
            OsEng.renderEngine().raycasterManager().attachRaycaster(*_raycaster.get());
        }
        else {
            OsEng.renderEngine().raycasterManager().detachRaycaster(*_raycaster.get());
        }
    };

    onEnabledChange(onChange);

    addProperty(_scaling);
    addProperty(_scalingExponent);
    addProperty(_stepSize);
    addProperty(_translation);
    addProperty(_rotation);
    
    // initialize points.
    std::ifstream pointFile(_pointsFilename, std::ios::in);

    std::vector<glm::vec3> pointPositions;
    std::vector<glm::vec3> pointColors;

    std::string format;
    pointFile >> format >> _nPoints;

    // temporarily decrease number of points.
    _nPoints = std::min(static_cast<size_t>(100000), _nPoints);

    float x, y, z, r, g, b, a;
    for (size_t i = 0; i < _nPoints; ++i) {
        pointFile >> x >> y >> z >> r >> g >> b >> a;
        if (pointFile.good()) {                       
            pointPositions.push_back(glm::vec3(x, y, z));
            pointColors.push_back(glm::vec3(r, g, b));
        }
        else {
            LERROR("Could not read points.");
            break;
        }
    }
    pointFile.close();

    glGenVertexArrays(1, &_pointsVao);
    glGenBuffers(1, &_positionVbo);
    glGenBuffers(1, &_colorVbo);

    glBindVertexArray(_pointsVao);
    glBindBuffer(GL_ARRAY_BUFFER, _positionVbo);
    glBufferData(GL_ARRAY_BUFFER,
        pointPositions.size()*sizeof(glm::vec3),
        pointPositions.data(),
        GL_STATIC_DRAW);

    glBindBuffer(GL_ARRAY_BUFFER, _colorVbo);
    glBufferData(GL_ARRAY_BUFFER,
        pointColors.size()*sizeof(glm::vec3),
        pointColors.data(),
        GL_STATIC_DRAW);


    RenderEngine& renderEngine = OsEng.renderEngine();
    _pointsProgram = renderEngine.buildRenderProgram("Galaxy points",
        "${MODULE_GALAXY}/shaders/points.vs",
        "${MODULE_GALAXY}/shaders/points.fs");

    GLint positionAttrib = _pointsProgram->attributeLocation("inPosition");
    GLint colorAttrib = _pointsProgram->attributeLocation("inColor");

    glBindBuffer(GL_ARRAY_BUFFER, _positionVbo);
    glEnableVertexAttribArray(positionAttrib);    
    glVertexAttribPointer(positionAttrib, 3, GL_FLOAT, GL_FALSE, 0, 0);

    glBindBuffer(GL_ARRAY_BUFFER, _colorVbo);
    glEnableVertexAttribArray(colorAttrib);
    glVertexAttribPointer(colorAttrib, 3, GL_FLOAT, GL_FALSE, 0, 0);
        
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);

    return true;
}
    
bool RenderableGalaxy::deinitialize() {
    if (_raycaster) {
        OsEng.renderEngine().raycasterManager().detachRaycaster(*_raycaster.get());
        _raycaster = nullptr;
    }
    return true;
}
    
bool RenderableGalaxy::isReady() const {
    return true;
}
    
void RenderableGalaxy::update(const UpdateData& data) {
    if (_raycaster) {

        glm::mat4 transform = glm::translate(glm::mat4(1.0), static_cast<glm::vec3>(_translation) * std::powf(10.0, static_cast<float>(_scalingExponent)));
        glm::vec3 eulerRotation = static_cast<glm::vec3>(_rotation);
        transform = glm::rotate(transform, eulerRotation.x, glm::vec3(1, 0, 0));
        transform = glm::rotate(transform, eulerRotation.y, glm::vec3(0, 1, 0));
        transform = glm::rotate(transform, eulerRotation.z,  glm::vec3(0, 0, 1));
        transform = glm::scale(transform, _aspect * static_cast<glm::vec3>(_scaling) * std::powf(10.0, static_cast<float>(_scalingExponent)));
        
        _raycaster->setStepSize(_stepSize);
        _raycaster->setAspect(_aspect);
        _raycaster->setModelTransform(transform);
        _raycaster->setTime(data.time);
    }
}

void RenderableGalaxy::render(const RenderData& data, RendererTasks& tasks) {
    RaycasterTask task{ _raycaster.get(), data };
    tasks.raycasterTasks.push_back(task);

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    _pointsProgram->activate();
    setPscUniforms(*_pointsProgram.get(), data.camera, data.position);


    glm::mat4 modelMatrix = glm::mat4(1.0);
    glm::mat4 viewMatrix = data.camera.viewMatrix();
    glm::mat4 projectionMatrix = data.camera.projectionMatrix();

    _pointsProgram->setUniform("model", modelMatrix);
    _pointsProgram->setUniform("view", viewMatrix);
    _pointsProgram->setUniform("projection", projectionMatrix);

    glBindVertexArray(_pointsVao);
    glDepthMask(false);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    glDrawArrays(GL_POINTS, 0, _nPoints);
    glBindVertexArray(0);
    glDepthMask(true);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

}
       
}
