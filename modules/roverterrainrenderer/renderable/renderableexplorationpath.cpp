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

#include <modules/roverterrainrenderer/renderable/renderableexplorationpath.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/engine/globals.h>

#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/geodetic3.h>

#include <ghoul/filesystem/filesystem.h>

#include <fstream>
#include "ogr_geometry.h"
#include "ogrsf_frmts.h"
#include <gdal_priv.h>

namespace {
    static const std::string _loggerCat = "RenderableExplorationPath";
    static const openspace::properties::Property::PropertyInfo enabledPropertyInfo = {
        "enabled",
        "enabled",
        "" // @TODO Missing documentation
    };
}

namespace openspace {

RenderableExplorationPath::RenderableExplorationPath()
    : _pathShader(nullptr)
    , _siteShader(nullptr)
    , _fading(0.0)
    , _fading2(0.0)
    , _isEnabled(properties::BoolProperty(enabledPropertyInfo, false))
{}

RenderableExplorationPath::~RenderableExplorationPath() {}
void RenderableExplorationPath::initializeGL() {

}
bool RenderableExplorationPath::initialize(
    globebrowsing::RenderableGlobe* globe,
    const std::vector<globebrowsing::Geodetic2> allCoordinates,
    const std::vector<globebrowsing::Geodetic2> coordinatesWithModels)
{
    std::string vs_path = absPath("${MODULE_ROVERTERRAINRENDERER}/shaders/roverpath_vs.glsl");
    std::string fs_path = absPath("${MODULE_ROVERTERRAINRENDERER}/shaders/roverpath_fs.glsl");
    std::replace(vs_path.begin(), vs_path.end(), '\\', '/');
    std::replace(fs_path.begin(), fs_path.end(), '\\', '/');
    // Shaders for the path (GL_LINES)
    if (_pathShader == nullptr) {
        _pathShader = global::renderEngine.buildRenderProgram(
            "RoverPath",
            vs_path,
            fs_path
        );
    }

    std::string vs_path2 = absPath("${MODULE_ROVERTERRAINRENDERER}/shaders/roversite_vs.glsl");
    std::string fs_path2 = absPath("${MODULE_ROVERTERRAINRENDERER}/shaders/roversite_fs.glsl");
    std::replace(vs_path2.begin(), vs_path2.end(), '\\', '/');
    std::replace(fs_path2.begin(), fs_path2.end(), '\\', '/');

    // Shaders for the sites (GL_POINTS)
    if (_siteShader == nullptr) {
        _siteShader = global::renderEngine.buildRenderProgram(
            "RoverPath",
            vs_path2,
            fs_path2
        );
    }

    _globe = globe;
    _allGeodetics = allCoordinates;
    _geodeticsWithModel = coordinatesWithModels;

    _stationPointsModelCoordinates = calculateModelCoordinates(_allGeodetics, 0);
    _stationPointsModelCoordinates = calculateModelCoordinates(_allGeodetics, 0);

    if (_allGeodetics.size() == 0) return false;

    // Initialize and upload to graphics card
    glGenVertexArrays(1, &_vaPathID);
    //ghoul_assert(_vaPathID != 0, "Could not generate vertex arrays");

    glGenBuffers(1, &_vbPathID);
    //ghoul_assert(_vertexBufferID != 0, "Could not create vertex buffer");

    bufferData(_vaPathID, _vbPathID, _stationPointsModelCoordinates);

    // Stations with models
    // Initialize and upload to graphics card
    glGenVertexArrays(1, &_vaModelsID);
    //ghoul_assert(_vaioID2 != 0, "Could not generate vertex arrays");

    glGenBuffers(1, &_vbModelsID);
    //ghoul_assert(_vertexBufferID2 != 0, "Could not create vertex buffer");

    bufferData(_vaModelsID, _vbModelsID, _stationPointsModelCoordinatesWithModel);

    return true;
}

bool RenderableExplorationPath::deinitialize() {
    if (_pathShader) {
        global::renderEngine.removeRenderProgram(_pathShader.get());
        _pathShader = nullptr;
    }
    if (_siteShader) {
        global::renderEngine.removeRenderProgram(_siteShader.get());
        _siteShader = nullptr;
    }

    glDeleteVertexArrays(1, &_vaPathID);
    glDeleteBuffers(1, &_vbPathID);

    glDeleteVertexArrays(1, &_vaModelsID);
    glDeleteBuffers(1, &_vbModelsID);

    return false;
}

bool RenderableExplorationPath::isReady() const {
    bool ready = true;
    ready &= (_pathShader != nullptr);
    return true;
}

void RenderableExplorationPath::render(const RenderData& data) {
    glm::dmat4 globeModelTransform;
    glm::dmat4 modelViewTransform;

    //if(_currentLevel >= 1) {
        // Only show the path when camera is close enough
        if (_currentLevel > 0 && _fading < 1.0f)
            _fading += 0.05f;
        else if (_currentLevel <= 0 && _fading > 0.0f)
            _fading -= 0.05f;

        // Model transform and view transform needs to be in double precision
        globeModelTransform = _globe->modelTransform();
        modelViewTransform = data.camera.combinedViewMatrix() * globeModelTransform;

        _pathShader->activate();

        // Passing model view transform as double to maintain precision for vertices.
        // Otherwise the path will be twitching.
        _pathShader->setUniform("modelViewTransform", modelViewTransform);
        _pathShader->setUniform("projectionTransform", data.camera.projectionMatrix());
        _pathShader->setUniform("fading", _fading);

        glBindVertexArray(_vaPathID);
        glBindBuffer(GL_ARRAY_BUFFER, _vbPathID);
        glLineWidth(1.0f);
        glDrawArrays(GL_LINE_STRIP, 0, static_cast<GLsizei>(_stationPointsModelCoordinates.size()));
        glBindVertexArray(0);

        _pathShader->deactivate();
    //}

    if(_currentLevel >= 2) {
        if (_currentLevel >= 2 && _fading2 < 1.f)
            _fading2 += 0.01f;
        else if (_currentLevel < 2 && _fading2 > 0.f)
            _fading2 -= 0.01f;

        _siteShader->activate();

        _siteShader->setUniform("modelViewTransform", modelViewTransform);
        _siteShader->setUniform("projectionTransform", data.camera.projectionMatrix());
        _siteShader->setUniform("fading", _fading2);

        glBindVertexArray(_vaModelsID);
        glBindBuffer(GL_ARRAY_BUFFER, _vbModelsID);

        glEnable(GL_PROGRAM_POINT_SIZE);
        glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_stationPointsModelCoordinatesWithModel.size()));
        glBindVertexArray(0);

        _siteShader->deactivate();
    }
}

void RenderableExplorationPath::update(const UpdateData& data) {
}

void RenderableExplorationPath::setLevel(const int level) {
    _currentLevel = level;
    if (level != lastLevel) {
        float offset = 10;
        if (_currentLevel < 2)
            offset = 6;
        else if (_currentLevel == 2)
            offset = 4;
        else if (_currentLevel == 3)
            offset = 1.5;

        _stationPointsModelCoordinates.clear();
        _stationPointsModelCoordinates = calculateModelCoordinates(_allGeodetics, offset);
        reBufferData(_vaPathID, _vbPathID, _stationPointsModelCoordinates);

        _stationPointsModelCoordinatesWithModel.size();
        _stationPointsModelCoordinatesWithModel = calculateModelCoordinates(_geodeticsWithModel, offset);
        reBufferData(_vaModelsID, _vbModelsID, _stationPointsModelCoordinatesWithModel);

        lastLevel = level;
    }
}

std::vector<glm::vec4> RenderableExplorationPath::calculateModelCoordinates(std::vector<globebrowsing::Geodetic2> geodetics,  const float offset) {
    std::vector<glm::vec4> cartesianCoordinates;
    for (auto geodetic : geodetics) {
        glm::dvec3 positionModelSpaceTemp = _globe->ellipsoid().cartesianSurfacePosition(geodetic);

        double heightToSurface = _globe->getHeight(positionModelSpaceTemp);

        globebrowsing::Geodetic3 geo3 = globebrowsing::Geodetic3{ geodetic, heightToSurface + offset };
        glm::dvec3 tempPos2 = _globe->ellipsoid().cartesianPosition(geo3);
        cartesianCoordinates.push_back(glm::dvec4(tempPos2, 1.0));
    }
    return cartesianCoordinates;
}

void RenderableExplorationPath::bufferData(GLuint vaID, GLuint vbID, const std::vector<glm::vec4> coordinates) {
    glBindVertexArray(vaID);
    glBindBuffer(GL_ARRAY_BUFFER, vbID);
    glBufferData(GL_ARRAY_BUFFER,
        coordinates.size() * sizeof(coordinates[0]),
        &coordinates[0],
        GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(coordinates[0]), 0);
    glBindVertexArray(0);
}

void RenderableExplorationPath::reBufferData(GLuint vaID, GLuint vbID, const std::vector<glm::vec4> coordinates) {
    glBindVertexArray(vaID);
    glBindBuffer(GL_ARRAY_BUFFER, vbID);

    glBufferData(GL_ARRAY_BUFFER, coordinates.size() * sizeof(coordinates[0]),
        NULL, GL_STATIC_DRAW);
    glBufferSubData(GL_ARRAY_BUFFER, 0,
        coordinates.size() * sizeof(coordinates[0]),
        &coordinates[0]);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, 0);
    glBindVertexArray(0);
}

} // namespace openspace
