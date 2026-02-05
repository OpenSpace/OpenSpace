/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <modules/solarbrowsing/rendering/spacecraftcameraplane.h>

#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/transferfunction.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>


namespace {
    constexpr const double SUN_RADIUS = (1391600000.0 * 0.5);
    constexpr const char* _loggerCat = "SpacecraftCameraPlane";
} // namespace

namespace openspace {

SpacecraftCameraPlane::SpacecraftCameraPlane(double moveDistance) {
    initialize();
    createPlaneAndFrustum(moveDistance);
}

bool SpacecraftCameraPlane::destroy() {
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;
    glDeleteVertexArrays(1, &_frustum);
    _frustum = 0;

    if (_planeShader) {
        global::renderEngine->removeRenderProgram(_planeShader.get());
        _planeShader = nullptr;
    }
    if (_frustumShader) {
        global::renderEngine->removeRenderProgram(_frustumShader.get());
        _frustumShader = nullptr;
    }
    return true;
}

bool SpacecraftCameraPlane::initialize() {
    // Initialize plane buffer
    glGenVertexArrays(1, &_quad);
    glGenBuffers(1, &_vertexPositionBuffer);
    // Initialize frustum buffer
    glGenVertexArrays(1, &_frustum);
    glGenBuffers(1, &_frustumPositionBuffer);
    if (!_planeShader) {
        _planeShader = global::renderEngine->buildRenderProgram("SpacecraftImagePlaneProgram",
            absPath("${MODULE_SOLARBROWSING}/shaders/spacecraftimageplane_vs.glsl"),
            absPath("${MODULE_SOLARBROWSING}/shaders/spacecraftimageplane_fs.glsl")
        );
        if (!_planeShader) {
            return false;
        }
    }

    if (!_frustumShader) {
        _frustumShader = global::renderEngine->buildRenderProgram("SpacecraftFrustumProgram",
            absPath("${MODULE_SOLARBROWSING}/shaders/spacecraftimagefrustum_vs.glsl"),
            absPath("${MODULE_SOLARBROWSING}/shaders/spacecraftimagefrustum_fs.glsl")
        );
        if (!_frustumShader) {
            return false;
        }
    }
}

const glm::vec3& SpacecraftCameraPlane::normal() const {
    return _normal;
};

const glm::dvec3& SpacecraftCameraPlane::worldPosition() const {
    return _position;
}

const glm::dmat4& SpacecraftCameraPlane::worldRotation() const {
    return _rotation;
}

void SpacecraftCameraPlane::update() {
    if (_planeShader->isDirty()) {
        _planeShader->rebuildFromFile();
    }

    if (_frustumShader->isDirty()) {
        _frustumShader->rebuildFromFile();
    }
}

bool SpacecraftCameraPlane::isReady() {
    return _planeShader && _frustumShader;
}

void SpacecraftCameraPlane::createPlaneAndFrustum(double moveDistance) {
    //const double a = 1;
    //const double b = 0;
    //const double c = 0.31622776601; // sqrt(0.1)
    //_move = a * exp(-(pow((_moveFactor.value() - 1) - b, 2.0)) / (2.0 * pow(c, 2.0)));
    //_move = /*a **/ exp(-(pow((_moveFactor.value() - 1) /*- b*/, 2.0)) / (2.0 /** pow(c, 2.0)*/));

    // Computing the image plane position using linear scale is not sufficient for fine
    // tuning movement near the Sun. A Gaussian function* (3.1) is used to address this
    // issue: *https://www.diva-portal.org/smash/get/diva2:1147161/FULLTEXT01.pdf
    _gaussianMoveFactor = /*a **/ exp(-(pow((moveDistance - 1) /*- b*/, 2.0)) / (2.0 /** pow(c, 2.0)*/));
    _size = static_cast<float>(_gaussianMoveFactor * SUN_RADIUS); /// _scaleFactor;
    createPlane();
    createFrustum();
}

void SpacecraftCameraPlane::createPlane() {
    const GLfloat size = _size;
    const GLfloat vertex_data[] = {
        // x      y     z     w     s     t
        -size, -size, 0.f, 0.f, 0.f, 0.f,
        size, size, 0.f, 0.f, 1.f, 1.f,
        -size, size, 0.f, 0.f, 0.f, 1.f,
        -size, -size, 0.f, 0.f, 0.f, 0.f,
        size, -size, 0.f, 0.f, 1.f, 0.f,
        size, size, 0.f, 0.f, 1.f, 1.f,
    };

    glBindVertexArray(_quad); // bind array
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0,
        4,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 6,
        reinterpret_cast<void*>(0)
    );
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 6,
        reinterpret_cast<void*>(sizeof(GLfloat) * 4)
    );
}

void SpacecraftCameraPlane::createFrustum() {
    // Vertex orders x, y, z, w
    // Where w indicates if vertex should be drawn in spacecraft
    // or planes coordinate system
    const GLfloat vertex_data[] = {
        0.f, 0.f, 0.f, 0.0,
        _size, _size, 0.f , 1.0,
        0.f, 0.f, 0.f, 0.0,
        -_size, -_size, 0.f , 1.0,
        0.f, 0.f, 0.f, 0.0,
        _size, -_size, 0.f , 1.0,
        0.f, 0.f, 0.f, 0.0,
        -_size, _size, 0.f , 1.0,
        // Borders
        // Left
        -_size, -_size, 0.f, 1.0,
        -_size, _size, 0.f, 1.0,
        // Top
        -_size, _size, 0.f, 1.0,
        _size, _size, 0.f, 1.0,
        // Right
        _size, _size, 0.f, 1.0,
        _size, -_size, 0.f, 1.0,
        // Bottom
        _size, -_size, 0.f, 1.0,
        -_size, -_size, 0.f, 1.0,
    };
    glBindVertexArray(_frustum);
    glBindBuffer(GL_ARRAY_BUFFER, _frustumPositionBuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, reinterpret_cast<void*>(0));
}

void SpacecraftCameraPlane::render(const RenderData& data,
                                   ghoul::opengl::Texture& imageryTexture,
                                   TransferFunction* lut,
                                   const glm::dvec3& sunPositionWorld, float planeOpacity,
                                   float contrastValue, float gammaValue,
                                   bool enableBorder, bool enableFrustum,
                                   const glm::vec2& currentCenterPixel,
                                   float currentScale, float multipleImageryOffset,
                                   bool isCoronaGraph)
{
    glEnable(GL_CULL_FACE);

    // Perform necessary transforms
    const glm::dmat4& viewMatrix = data.camera.combinedViewMatrix();
    const glm::mat4& projectionMatrix = data.camera.projectionMatrix();

    // TODO: We want to create sun imagery node from within the module
    const glm::dvec3& spacecraftPosWorld = data.modelTransform.translation;
    const glm::dmat3 spacecraftRotWorld = data.modelTransform.rotation;

    const glm::dvec3 sunDir = sunPositionWorld - spacecraftPosWorld;
    const glm::dvec3 offset = sunDir * (_gaussianMoveFactor + static_cast<double>(multipleImageryOffset));

    _position = spacecraftPosWorld + offset;
    // normal should point from plane toward spacecraft (i.e. plane faces spacecraft)
    _normal = glm::normalize(spacecraftPosWorld - sunPositionWorld);

    // anden88 2025-12-10: An attempt was made to use the glm::lookAt to "simplify" the
    // rotation matrix without having to build the basis vectors ourselves. However, the
    // plane rotation would be rotating in all different kinds of orientations
    // _rotation = glm::lookAt(spacecraftPosWorld, glm::dvec3(sunPositionWorld), glm::normalize(up));
    // _rotation[3] = glm::dvec4(0.0, 0.0, 0.0, 1.0);

    // Pick a world up. Prefer the spacecraft local +Z transformed to world,
    // but fall back to a global up if nearly collinear with the normal.
    glm::vec3 worldUp = spacecraftRotWorld * glm::dvec3(0.0, 0.0, 1.0);
    if (std::abs(glm::dot(worldUp, _normal)) > 0.9999) {
        // nearly parallel: pick another stable up (e.g. world Y)
        worldUp = glm::dvec3(0.0, 1.0, 0.0);
    }

    // build tangent basis for the plane: right, upOnPlane, normal
    glm::vec3 right = glm::normalize(glm::cross(worldUp, _normal));
    glm::vec3 upOnPlane = glm::cross(_normal, right); // already normalized if right and N are normalized

    // Build a rotation matrix that transforms local axes -> world axes.
    // Local axes: +X = right, +Y = upOnPlane, +Z = _normal
    glm::dmat4 rot = glm::dmat4(1.0);
    rot[0] = glm::dvec4(right, 0.0);      // first column = world X for local +X
    rot[1] = glm::dvec4(upOnPlane, 0.0);  // second column = world Y for local +Y
    rot[2] = glm::dvec4(_normal, 0.0);    // third column = world Z for local +Z

    _rotation = std::move(rot);

    const glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), _position) *
        _rotation *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale))
    );
    const glm::dmat4 modelViewTransform = viewMatrix * modelTransform;

    // For frustum
    const glm::dmat4 spacecraftModelTransform =
        glm::translate(glm::dmat4(1.0), spacecraftPosWorld) *
        _rotation *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale))
    );

    _planeShader->activate();
    ghoul::opengl::TextureUnit imageUnit;
    imageUnit.activate();
    imageryTexture.bind();

    _planeShader->setUniform("isCoronaGraph", isCoronaGraph);
    _planeShader->setUniform("scale", currentScale);
    _planeShader->setUniform("centerPixel", currentCenterPixel);
    _planeShader->setUniform("imageryTexture", imageUnit);
    _planeShader->setUniform("planeOpacity", planeOpacity);
    _planeShader->setUniform("gammaValue", gammaValue);
    _planeShader->setUniform("contrastValue", contrastValue);
    _planeShader->setUniform(
        "modelViewProjectionTransform",
        projectionMatrix * glm::mat4(modelViewTransform)
    );

    //_tfMap[_currentActiveInstrument]->bind(); // Calls update internally
    ghoul::opengl::TextureUnit tfUnit;
    tfUnit.activate();
    if (lut) {
        lut->bind();
        _planeShader->setUniform("hasLut", true);
    } else {
        _planeShader->setUniform("hasLut", false);
    }
    // Must bind all sampler2D, otherwise undefined behaviour
    _planeShader->setUniform("lut", tfUnit);

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    _planeShader->deactivate();
    _frustumShader->activate();

    _frustumShader->setUniform("planeOpacity", planeOpacity);
    _frustumShader->setUniform("modelViewProjectionTransform",
                               projectionMatrix
                                     * glm::mat4(viewMatrix * spacecraftModelTransform));
    _frustumShader->setUniform("modelViewProjectionTransformPlane",
                               projectionMatrix * glm::mat4(modelViewTransform));

    _frustumShader->setUniform("scale", currentScale);
    _frustumShader->setUniform("centerPixel", currentCenterPixel);

    glBindVertexArray(_frustum);

    if (enableBorder && enableFrustum) {
        glDrawArrays(GL_LINES, 0, 16);
    } else if (!enableBorder && enableFrustum) {
        glDrawArrays(GL_LINES, 0, 8);
    } else if (!enableFrustum && enableBorder) {
        glDrawArrays(GL_LINES, 8, 16);
    }
    _frustumShader->deactivate();

    glDisable(GL_CULL_FACE);
}

} // namespace openspace
