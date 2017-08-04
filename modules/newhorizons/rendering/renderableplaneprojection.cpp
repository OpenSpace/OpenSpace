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

#include <modules/newhorizons/rendering/renderableplaneprojection.h>

#include <modules/newhorizons/util/imagesequencer.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/configurationmanager.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/filesystem/filesystem>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <openspace/util/time.h>

#include <glm/gtx/projection.hpp>

namespace {
    const std::string _loggerCat = "RenderablePlaneProjection";
    const char* KeySpacecraft = "Spacecraft";
    const char* KeyInstrument = "Instrument";
    const char* KeyMoving = "Moving";
    const char* KeyTexture = "Texture";
    const char* KeyName = "Name";
    const char* KeyTarget = "DefaultTarget";
    const char* GalacticFrame = "GALACTIC";
} // namespace

namespace openspace {

RenderablePlaneProjection::RenderablePlaneProjection(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _texturePath("")
    , _planeIsDirty(false)
    , _shader(nullptr)
    , _textureIsDirty(false)
    , _texture(nullptr)
    , _quad(0)
    , _vertexPositionBuffer(0)
    , _previousTime(0)
    , _name("ImagePlane")
    , _moving(false)
    , _hasImage(false)
{
    dictionary.getValue(KeySpacecraft, _spacecraft);
    dictionary.getValue(KeyInstrument, _instrument);
    dictionary.getValue(KeyMoving, _moving);
    dictionary.getValue(KeyName, _name);
    dictionary.getValue(KeyTarget, _defaultTarget);

    std::string texturePath = "";
    bool success = dictionary.getValue(KeyTexture, _texturePath);
    if (success) {
        _texturePath = absPath(_texturePath);
        _textureFile = new ghoul::filesystem::File(_texturePath);
    }

    loadTexture();
}

RenderablePlaneProjection::~RenderablePlaneProjection() {
    delete _textureFile;
}

bool RenderablePlaneProjection::isReady() const {
    return _shader && _texture;
}

void RenderablePlaneProjection::initialize() {
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
    
    // Image Plane Program
    RenderEngine& renderEngine = OsEng.renderEngine();
    _shader = renderEngine.buildRenderProgram(
        "Image Plane",
        "${MODULE_BASE}/shaders/imageplane_vs.glsl",
        "${MODULE_BASE}/shaders/imageplane_fs.glsl"
    );

    setTarget(_defaultTarget);
    loadTexture();
}

void RenderablePlaneProjection::deinitialize() {
    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shader) {
        renderEngine.removeRenderProgram(_shader);
        _shader = nullptr;
    }

    glDeleteVertexArrays(1, &_quad);
    _quad = 0;
    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;
    _texture = nullptr;
}

void RenderablePlaneProjection::render(const RenderData& data, RendererTasks&) {
    bool active = ImageSequencer::ref().instrumentActive(_instrument);
    if (!_hasImage || (_moving && !active))
        return;

    // Activate shader
    _shader->activate();

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(_stateMatrix);
    glm::mat4 modelViewProjectionTransform =
        data.camera.projectionMatrix() *
        glm::mat4(data.camera.combinedViewMatrix() *
            modelTransform);

    _shader->setUniform("modelViewProjectionTransform", modelViewProjectionTransform);

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _shader->setUniform("texture1", unit);

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    _shader->deactivate();
}

void RenderablePlaneProjection::update(const UpdateData& data) {
    double time = data.time.j2000Seconds();
    const Image img = openspace::ImageSequencer::ref().getLatestImageForInstrument(_instrument);
    
    if (img.path == "")
        return;
    else
        _hasImage = true;

    _stateMatrix = SpiceManager::ref().positionTransformMatrix(_target.frame, GalacticFrame, time);
    
    double timePast = std::abs(img.timeRange.start - _previousTime);
    
    std::string tex = _texturePath;
    if (_moving || _planeIsDirty)
        updatePlane(img, time);

    else if (timePast > DBL_EPSILON) {
        _previousTime = time = img.timeRange.start;
        updatePlane(img, time);
    }

    if (_shader->isDirty())
        _shader->rebuildFromFile();

    if (_textureIsDirty) {
        loadTexture();
        _textureIsDirty = false;
    }
}

void RenderablePlaneProjection::loadTexture() {
    if (_texturePath != "") {
        std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_texturePath));
        if (texture) {
            if (texture->format() == ghoul::opengl::Texture::Format::Red)
                texture->setSwizzleMask({ GL_RED, GL_RED, GL_RED, GL_ONE });
            texture->uploadTexture();
            // TODO: AnisotropicMipMap crashes on ATI cards ---abock
            //texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
            _texture = std::move(texture);

            delete _textureFile;
            _textureFile = new ghoul::filesystem::File(_texturePath);
            _textureFile->setCallback([&](const ghoul::filesystem::File&) { _textureIsDirty = true; });
        }
    }
}

void RenderablePlaneProjection::updatePlane(const Image& img, double currentTime) {

    std::string frame;
    std::vector<glm::dvec3> bounds;
    glm::dvec3 boresight;

    std::string target = _defaultTarget;
    // Turned on if the plane should be attached to the closest target, 
    // rather than the target specified in img 
    //if (!_moving) {
    //    target = findClosestTarget(currentTime);
    //}
    if (img.path != "")
        target = img.target;

    setTarget(target);

    try {
        SpiceManager::FieldOfViewResult res = SpiceManager::ref().fieldOfView(_instrument);

        frame = std::move(res.frameName);
        bounds = std::move(res.bounds);
        boresight = std::move(res.boresightVector);
    }
    catch (const SpiceManager::SpiceException& e) {
        LERROR(e.what());
    }

    double lt;
    psc projection[4];

    glm::dvec3 vecToTarget = SpiceManager::ref().targetPosition(
        _target.body,
        _spacecraft,
        GalacticFrame,
        { SpiceManager::AberrationCorrection::Type::ConvergedNewtonianStellar, SpiceManager::AberrationCorrection::Direction::Reception },
        currentTime,
        lt
    );
    // The apparent position, CN+S, makes image align best with target

    for (size_t j = 0; j < bounds.size(); ++j) {
        bounds[j] = SpiceManager::ref().frameTransformationMatrix(frame, GalacticFrame, currentTime) * bounds[j];
        glm::dvec3 cornerPosition = glm::proj(vecToTarget, bounds[j]);

        if (!_moving) {
            cornerPosition -= vecToTarget;
        }
        cornerPosition = SpiceManager::ref().frameTransformationMatrix(GalacticFrame, _target.frame, currentTime) * cornerPosition;

        projection[j] = PowerScaledCoordinate::CreatePowerScaledCoordinate(cornerPosition[0], cornerPosition[1], cornerPosition[2]);
        projection[j][3] += 3;
    }

    if (!_moving) {
        SceneGraphNode* thisNode = OsEng.renderEngine().scene()->sceneGraphNode(_name);
        SceneGraphNode* newParent = OsEng.renderEngine().scene()->sceneGraphNode(_target.node);
        if (thisNode && newParent) {
            thisNode->setParent(*newParent);
        }   
    }
    
    const GLfloat vertex_data[] = { // square of two triangles drawn within fov in target coordinates
        //      x      y     z     w     s     t
        projection[1][0], projection[1][1], projection[1][2], projection[1][3], 0, 0, // Lower left 1
        projection[3][0], projection[3][1], projection[3][2], projection[3][3], 1, 1, // Upper right 2
        projection[2][0], projection[2][1], projection[2][2], projection[2][3], 0, 1, // Upper left 3
        projection[1][0], projection[1][1], projection[1][2], projection[1][3], 0, 0, // Lower left 4 = 1
        projection[0][0], projection[0][1], projection[0][2], projection[0][3], 1, 0, // Lower right 5
        projection[3][0], projection[3][1], projection[3][2], projection[3][3], 1, 1, // Upper left 6 = 2
        //projection[1][0], projection[1][1], projection[1][2], projection[1][3], 0, 1, // Lower left 1
        //projection[3][0], projection[3][1], projection[3][2], projection[3][3], 1, 0, // Upper right 2
        //projection[2][0], projection[2][1], projection[2][2], projection[2][3], 0, 0, // Upper left 3
        //projection[1][0], projection[1][1], projection[1][2], projection[1][3], 0, 1, // Lower left 4 = 1
        //projection[0][0], projection[0][1], projection[0][2], projection[0][3], 1, 1, // Lower right 5
        //projection[3][0], projection[3][1], projection[3][2], projection[3][3], 1, 0, // Upper left 6 = 2

    };
    //projection[1][0], projection[1][1], projection[1][2], projection[1][3], 0, 1, // Lower left 1
    //    projection[3][0], projection[3][1], projection[3][2], projection[3][3], 1, 0, // Upper right 2
    //    projection[2][0], projection[2][1], projection[2][2], projection[2][3], 0, 0, // Upper left 3
    //    projection[1][0], projection[1][1], projection[1][2], projection[1][3], 0, 1, // Lower left 4 = 1
    //    projection[0][0], projection[0][1], projection[0][2], projection[0][3], 1, 1, // Lower right 5
    //    projection[3][0], projection[3][1], projection[3][2], projection[3][3], 1, 0, // Upper left 6 = 2


    glBindVertexArray(_quad); // bind array
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(sizeof(GLfloat) * 4));

    if (!_moving && img.path != "") {
        _texturePath = img.path;
        loadTexture();
    }
}

void RenderablePlaneProjection::setTarget(std::string body) {
    if (body == "")
        return;

    std::vector<SceneGraphNode*> nodes = OsEng.renderEngine().scene()->allSceneGraphNodes();

    _target.body = body;
    _target.frame = openspace::SpiceManager::ref().frameFromBody(body);
}

} // namespace openspace
