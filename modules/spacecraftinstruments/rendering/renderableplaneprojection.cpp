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

#include <modules/spacecraftinstruments/rendering/renderableplaneprojection.h>

#include <modules/spacecraftinstruments/spacecraftinstrumentsmodule.h>
#include <modules/spacecraftinstruments/util/imagesequencer.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
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
    constexpr std::string_view _loggerCat = "RenderablePlaneProjection";

    struct [[codegen::Dictionary(RenderablePlaneProjection)]] Parameters {
        // The SPICE name of the spacecraft from which the projection is performed
        std::string spacecraft;

        // The SPICE name of the instrument that is used to project the image onto this
        // RenderablePlaneProjection
        std::string instrument;

        // The SPICE name of the default target that is imaged by this planet
        std::optional<std::string> defaultTarget;

        // The image that is used on this plane before any image is loaded from the
        // ImageSequencerr
        std::optional<std::string> texture;
    };
#include "renderableplaneprojection_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderablePlaneProjection::Documentation() {
    return codegen::doc<Parameters>("spacecraftinstruments_renderableplaneprojection");
}

RenderablePlaneProjection::RenderablePlaneProjection(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _spacecraft = p.spacecraft;
    _instrument = p.instrument;
    _defaultTarget = p.defaultTarget.value_or(_defaultTarget);

    if (p.texture.has_value()) {
        _texturePath = absPath(*p.texture).string();
        _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath);
    }
}

bool RenderablePlaneProjection::isReady() const {
    return _shader && _texture;
}

void RenderablePlaneProjection::initializeGL() {
    glGenVertexArrays(1, &_quad);
    glGenBuffers(1, &_vertexPositionBuffer);

    _shader = global::renderEngine->buildRenderProgram(
        "Image Plane",
        absPath("${MODULE_BASE}/shaders/imageplane_vs.glsl"),
        absPath("${MODULE_BASE}/shaders/imageplane_fs.glsl")
    );

    setTarget(_defaultTarget);
    loadTexture();
}

void RenderablePlaneProjection::deinitializeGL() {
    if (_shader) {
        global::renderEngine->removeRenderProgram(_shader.get());
        _shader = nullptr;
    }

    glDeleteVertexArrays(1, &_quad);
    _quad = 0;
    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;
    _texture = nullptr;
}

void RenderablePlaneProjection::render(const RenderData& data, RendererTasks&) {
    if (!_hasImage) {
        return;
    }

    _shader->activate();

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(_stateMatrix);

    const glm::dmat4 ModelViewProjectionTransform =
        calcModelViewProjectionTransform(data, modelTransform);

    _shader->setUniform(
        "modelViewProjectionTransform",
        glm::mat4(ModelViewProjectionTransform)
    );

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
    const Image& img = ImageSequencer::ref().latestImageForInstrument(_instrument);

    if (img.path.empty()) {
        return;
    }

    _hasImage = true;

    _stateMatrix = SpiceManager::ref().positionTransformMatrix(
        _target.frame,
        "GALACTIC",
        time
    );

    const double timePast = std::abs(img.timeRange.start - _previousTime);

    if (_planeIsDirty) {
        updatePlane(img, time);
    }
    else if (timePast > std::numeric_limits<double>::epsilon()) {
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
    if (_texturePath.empty()) {
        return;
    }

    std::unique_ptr<ghoul::opengl::Texture> texture =
        ghoul::io::TextureReader::ref().loadTexture(absPath(_texturePath).string(), 2);
    if (!texture) {
        return;
    }

    if (texture->format() == ghoul::opengl::Texture::Format::Red) {
        texture->setSwizzleMask({ GL_RED, GL_RED, GL_RED, GL_ONE });
    }
    texture->uploadTexture();
    // TODO: AnisotropicMipMap crashes on ATI cards ---abock
    //texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
    texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
    _texture = std::move(texture);

    _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath);
    _textureFile->setCallback([this]() { _textureIsDirty = true; });
}

void RenderablePlaneProjection::updatePlane(const Image& img, double currentTime) {
    std::string target = img.path.empty() ? _defaultTarget : img.target;
    setTarget(std::move(target));

    std::string frame;
    std::vector<glm::dvec3> bounds;
    glm::dvec3 boresight = glm::dvec3(0.0);
    try {
        SpiceManager::FieldOfViewResult r = SpiceManager::ref().fieldOfView(_instrument);

        frame = std::move(r.frameName);
        bounds = std::move(r.bounds);
        boresight = std::move(r.boresightVector);
    }
    catch (const SpiceManager::SpiceException& e) {
        LERROR(e.what());
    }

    double lt = 0.0;
    const glm::dvec3 vecToTarget = SpiceManager::ref().targetPosition(
        _target.body,
        _spacecraft,
        "GALACTIC",
        {
            SpiceManager::AberrationCorrection::Type::ConvergedNewtonianStellar,
            SpiceManager::AberrationCorrection::Direction::Reception
        },
        currentTime,
        lt
    );
    // The apparent position, CN+S, makes image align best with target

    std::array<glm::vec3, 4> projection;
    std::fill(projection.begin(), projection.end(), glm::vec3(0.f));
    for (size_t j = 0; j < bounds.size(); j++) {
        bounds[j] = SpiceManager::ref().frameTransformationMatrix(
            frame,
            "GALACTIC",
            currentTime
        ) * bounds[j];
        glm::dvec3 cornerPosition = glm::proj(vecToTarget, bounds[j]);

        cornerPosition -= vecToTarget;
        cornerPosition = SpiceManager::ref().frameTransformationMatrix(
            "GALACTIC",
            _target.frame,
            currentTime
        ) * cornerPosition;

        // km -> m
        projection[j] = glm::vec3(cornerPosition * 1000.0);
    }

    const std::array<GLfloat, 36> VertexData = {
        // square of two triangles drawn within fov in target coordinates
        //      x      y     z     w     s     t
        // Lower left 1
        projection[1].x, projection[1].y, projection[1].z, 0.f, 0.f, 0.f,
        // Upper right 2
        projection[3].x, projection[3].y, projection[3].z, 0.f, 1.f, 1.f,
        // Upper left 3
        projection[2].x, projection[2].y, projection[2].z, 0.f, 0.f, 1.f,
        // Lower left 4 = 1
        projection[1].x, projection[1].y, projection[1].z, 0.f, 0.f, 0.f,
        // Lower right 5
        projection[0].x, projection[0].y, projection[0].z, 0.f, 1.f, 0.f,
        // Upper left 6 = 2
        projection[3].x, projection[3].y, projection[3].z, 0.f, 1.f, 1.f,
    };

    glBindVertexArray(_quad);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(VertexData), VertexData.data(), GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, nullptr);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        6 * sizeof(GLfloat),
        reinterpret_cast<void*>(sizeof(GLfloat) * 4)
    );

    if (!img.path.empty()) {
        _texturePath = img.path;
        loadTexture();
    }
}

void RenderablePlaneProjection::setTarget(std::string body) {
    if (body.empty()) {
        return;
    }

    _target.frame =
        global::moduleEngine->module<SpacecraftInstrumentsModule>()->frameFromBody(body);
    _target.body = std::move(body);
}

} // namespace openspace
