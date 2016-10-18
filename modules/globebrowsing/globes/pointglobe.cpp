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

#include <modules/globebrowsing/globes/pointglobe.h>

#include <modules/globebrowsing/globes/renderableglobe.h>

// open space includes
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/scene/scenegraphnode.h>

// ghoul includes
#include <ghoul/misc/assert.h>

#define _USE_MATH_DEFINES
#include <math.h>

namespace {
    const std::string _loggerCat = "PointGlobe";
}

namespace openspace {
namespace globebrowsing {

    PointGlobe::PointGlobe(const RenderableGlobe& owner)
        : _owner(owner) {

    }

    PointGlobe::~PointGlobe() {
        glDeleteBuffers(1, &_vertexBufferID);
        glDeleteVertexArrays(1, &_vaoID);
    }

    bool PointGlobe::initialize() {
        _programObject = OsEng.renderEngine().buildRenderProgram(
            "PointGlobe",
            "${MODULE_GLOBEBROWSING}/shaders/pointglobe_vs.glsl",
            "${MODULE_GLOBEBROWSING}/shaders/pointglobe_fs.glsl");

        // Create VAO
        glGenVertexArrays(1, &_vaoID);

        // Create VBOs
        glGenBuffers(1, &_vertexBufferID);
        if (_vertexBufferID == 0) {
            LERROR("Could not create vertex buffer");
            return false;
        }

        // First VAO setup
        glBindVertexArray(_vaoID);

        // Vertex data is only one point in the origin
        glm::vec3 data = glm::vec3(0,0,0);

         // Vertex buffer
        glBindBuffer(GL_ARRAY_BUFFER, _vertexBufferID);
        glBufferData(
            GL_ARRAY_BUFFER,
            sizeof(glm::vec3),
            &data,
            GL_STATIC_DRAW);

        // Position at location 0
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(glm::vec3), 0);

        glBindVertexArray(0);

        return isReady();
    }

    bool PointGlobe::deinitialize() {
        return true;
    }

    bool PointGlobe::isReady() const {
        bool ready = true;
        return ready;
    }
    
    void PointGlobe::render(const RenderData& data) {
        _programObject->activate();

        // Calculate variables to be used as uniform variables in shader
        glm::dvec3 bodyPosition = data.modelTransform.translation;

        // Model transform and view transform needs to be in double precision
        glm::dmat4 modelTransform =
            glm::translate(glm::dmat4(1.0), bodyPosition) * // Translation
            glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)); // Scale
        glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;
        glm::vec3 directionToSun = glm::normalize(glm::vec3(0) - glm::vec3(bodyPosition));
        glm::vec3 directionToSunViewSpace = glm::mat3(data.camera.combinedViewMatrix()) * directionToSun;
        
        int windowWidth = OsEng.windowWrapper().currentWindowSize().x;
        float avgRadius = _owner.ellipsoid().averageRadius();
        
        _programObject->setUniform("windowWidth", windowWidth);
        _programObject->setUniform("globeRadius", avgRadius);
        _programObject->setUniform("directionToSunViewSpace", directionToSunViewSpace);
        _programObject->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
        _programObject->setUniform("projectionTransform", data.camera.projectionMatrix());

        glEnable(GL_VERTEX_PROGRAM_POINT_SIZE);

        glBindVertexArray(_vaoID);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _vertexBufferID);
        glDrawArrays(GL_POINTS, 0, 1);
        glBindVertexArray(0);

        glDisable(GL_VERTEX_PROGRAM_POINT_SIZE);

        _programObject->deactivate();
    }

    void PointGlobe::update(const UpdateData& data) {
        
    }

} // namespace globebrowsing
} // namespace openspace
