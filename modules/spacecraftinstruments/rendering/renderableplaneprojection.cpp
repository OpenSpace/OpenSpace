/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/spacecraftinstruments/rendering/renderableplaneprojection.h>

#include <modules/spacecraftinstruments/spacecraftinstrumentsmodule.h>
#include <modules/spacecraftinstruments/util/imagesequencer.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <glm/gtx/projection.hpp>

namespace {
    constexpr const char* _loggerCat = "RenderablePlaneProjection";
    constexpr const char* KeySpacecraft = "Spacecraft";
    constexpr const char* KeyInstrument = "Instrument";
    constexpr const char* KeyMoving = "Moving";
    constexpr const char* KeyTexture = "Texture";
    constexpr const char* KeyName = "Name";
    constexpr const char* KeyTarget = "DefaultTarget";
    constexpr const char* GalacticFrame = "GALACTIC";
} // namespace

namespace openspace {

RenderablePlaneProjection::RenderablePlaneProjection(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
{
    dictionary.getValue(KeySpacecraft, _spacecraft);
    dictionary.getValue(KeyInstrument, _instrument);
    dictionary.getValue(KeyMoving, _moving);
    dictionary.getValue(KeyName, _name);
    dictionary.getValue(KeyTarget, _defaultTarget);

    if (dictionary.hasKeyAndValue<std::string>(KeyTexture)) {
        _texturePath = dictionary.value<std::string>(KeyTexture);
        _texturePath = absPath(_texturePath);
        _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath);
    }
}

RenderablePlaneProjection::~RenderablePlaneProjection() {} // NOLINT

bool RenderablePlaneProjection::isReady() const {
    return _shader && _texture;
}

void RenderablePlaneProjection::initializeGL() {
    glGenVertexArrays(1, &_quad);
    glGenBuffers(1, &_vertexPositionBuffer);

    _shader = global::renderEngine.buildRenderProgram(
        "Image Plane",
        absPath("${MODULE_BASE}/shaders/imageplane_vs.glsl"),
        absPath("${MODULE_BASE}/shaders/imageplane_fs.glsl")
    );

    setTarget(_defaultTarget);
    loadTexture();
}

void RenderablePlaneProjection::deinitializeGL() {
    if (_shader) {
        global::renderEngine.removeRenderProgram(_shader.get());
        _shader = nullptr;
    }

    glDeleteVertexArrays(1, &_quad);
    _quad = 0;
    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;
    _texture = nullptr;
}

void RenderablePlaneProjection::render(const RenderData& data, RendererTasks&) {
    bool active = ImageSequencer::ref().isInstrumentActive(
        data.time.j2000Seconds(),
        _instrument
    );

    if (!_hasImage || (_moving && !active)) {
        return;
    }

    _shader->activate();

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(_stateMatrix);
    glm::mat4 modelViewProjectionTransform = data.camera.projectionMatrix() *
                             glm::mat4(data.camera.combinedViewMatrix() * modelTransform);

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
    const double time = data.time.j2000Seconds();
    const Image& img = openspace::ImageSequencer::ref().latestImageForInstrument(
        _instrument
    );

    if (img.path.empty()) {
        return;
    }

    _hasImage = true;

    _stateMatrix = SpiceManager::ref().positionTransformMatrix(
        _target.frame,
        GalacticFrame,
        time
    );

    const double timePast = std::abs(img.timeRange.start - _previousTime);

    if (_moving || _planeIsDirty) {
        updatePlane(img, time);
    }

    else if (timePast > DBL_EPSILON) {
        _previousTime = img.timeRange.start;
        updatePlane(img, img.timeRange.start);
    }

    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
    }

    if (_textureIsDirty) {
        loadTexture();
        _textureIsDirty = false;
    }
}

void RenderablePlaneProjection::loadTexture() {
    if (!_texturePath.empty()) {
        using TR = ghoul::io::TextureReader;
        std::unique_ptr<ghoul::opengl::Texture> texture = TR::ref().loadTexture(
            absPath(_texturePath)
        );
        if (texture) {
            if (texture->format() == ghoul::opengl::Texture::Format::Red) {
                texture->setSwizzleMask({ GL_RED, GL_RED, GL_RED, GL_ONE });
            }
            texture->uploadTexture();
            // TODO: AnisotropicMipMap crashes on ATI cards ---abock
            //texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
            _texture = std::move(texture);

            _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath);
            _textureFile->setCallback(
                [&](const ghoul::filesystem::File&) { _textureIsDirty = true; }
            );
        }
    }
}

void RenderablePlaneProjection::updatePlane(const Image& img, double currentTime) {
    std::string target = img.path.empty() ? _defaultTarget : img.target;
    setTarget(std::move(target));

    std::string frame;
    std::vector<glm::dvec3> bounds;
    glm::dvec3 boresight;
    try {
        SpiceManager::FieldOfViewResult r = SpiceManager::ref().fieldOfView(_instrument);

        frame = std::move(r.frameName);
        bounds = std::move(r.bounds);
        boresight = std::move(r.boresightVector);
    }
    catch (const SpiceManager::SpiceException& e) {
        LERROR(e.what());
    }

    double lt;
    const glm::dvec3 vecToTarget = SpiceManager::ref().targetPosition(
        _target.body,
        _spacecraft,
        GalacticFrame,
        {
            SpiceManager::AberrationCorrection::Type::ConvergedNewtonianStellar,
            SpiceManager::AberrationCorrection::Direction::Reception
        },
        currentTime,
        lt
    );
    // The apparent position, CN+S, makes image align best with target

    glm::dvec3 projection[4];
    for (size_t j = 0; j < bounds.size(); ++j) {
        bounds[j] = SpiceManager::ref().frameTransformationMatrix(
            frame,
            GalacticFrame,
            currentTime
        ) * bounds[j];
        glm::dvec3 cornerPosition = glm::proj(vecToTarget, bounds[j]);

        if (!_moving) {
            cornerPosition -= vecToTarget;
        }
        cornerPosition = SpiceManager::ref().frameTransformationMatrix(
            GalacticFrame,
            _target.frame,
            currentTime
        ) * cornerPosition;

        // km -> m
        projection[j] = cornerPosition * 1000.0;
    }

    if (!_moving) {
        SceneGraphNode* thisNode = global::renderEngine.scene()->sceneGraphNode(_name);
        SceneGraphNode* newParent = global::renderEngine.scene()->sceneGraphNode(
            _target.node
        );
        if (thisNode && newParent) {
            thisNode->setParent(*newParent);
        }
    }

    glm::vec3 p[4] = {
        glm::vec3(projection[0]),
        glm::vec3(projection[1]),
        glm::vec3(projection[2]),
        glm::vec3(projection[3])

    };
    const GLfloat vertex_data[] = {
        // square of two triangles drawn within fov in target coordinates
        //      x      y     z     w     s     t
        // Lower left 1
        p[1].x, p[1].y, p[1].z, 0.f, 0.f, 0.f,
        // Upper right 2
        p[3].x, p[3].y, p[3].z, 0.f, 1.f, 1.f,
        // Upper left 3
        p[2].x, p[2].y, p[2].z, 0.f, 0.f, 1.f,
        // Lower left 4 = 1
        p[1].x, p[1].y, p[1].z, 0.f, 0.f, 0.f,
        // Lower right 5
        p[0].x, p[0].y, p[0].z, 0.f, 1.f, 0.f,
        // Upper left 6 = 2
        p[3].x, p[3].y, p[3].z, 0.f, 1.f, 1.f,
    };

    glBindVertexArray(_quad);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, nullptr);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 6,
        reinterpret_cast<void*>(sizeof(GLfloat) * 4)
    );

    if (!_moving && !img.path.empty()) {
        _texturePath = img.path;
        loadTexture();
    }
}

void RenderablePlaneProjection::setTarget(std::string body) {
    if (body.empty()) {
        return;
    }

    _target.frame =
        global::moduleEngine.module<SpacecraftInstrumentsModule>()->frameFromBody(body);
    _target.body = std::move(body);
}

} // namespace openspace
