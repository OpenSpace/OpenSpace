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

#include <modules/base/rendering/renderablesphericalgrid.h>

#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/glm.h>

#define _USE_MATH_DEFINES
#include <math.h>

namespace {
    static const std::string _loggerCat = "RenderableSphericalGrid";
    const char* KeyGridType = "GridType";
    const char* KeyGridColor = "GridColor";
    const char* KeyGridMatrix = "GridMatrix";
    const char* KeyGridSegments = "GridSegments";
    const char* KeyGridRadius = "GridRadius";
    const char* KeyGridParentsRotation = "ParentsRotation";
    const glm::vec2 GridRadius = { 1.f, 20.f };
}
namespace openspace {

RenderableSphericalGrid::RenderableSphericalGrid(const ghoul::Dictionary& dictionary)  
    : Renderable(dictionary)
    , _gridProgram(nullptr)
    , _vaoID(0)
    , _vBufferID(0)
    , _iBufferID(0)
    , _mode(GL_LINES)
{
    _gridMatrix = glm::mat4(1);
    dictionary.getValue(KeyGridType, _gridType);
    dictionary.getValue(KeyGridColor, _gridColor);

    staticGrid = dictionary.getValue(KeyGridMatrix, _gridMatrix);
    if (!staticGrid){
        staticGrid = dictionary.getValue(KeyGridParentsRotation, _parentsRotation);
    }
    
    _segments = static_cast<int>(dictionary.value<double>(KeyGridSegments));
    //dictionary.getValue(KeyGridSegments, _segments);


    /*glm::vec2 radius;
    dictionary.getValue(constants::renderablesphericalgrid::gridRadius, radius);
    */

    _isize = int(6 * _segments * _segments);
    _vsize = int((_segments + 1) * (_segments + 1));
    _varray = new Vertex[_vsize];
    _iarray = new int[_isize];

    static_assert(sizeof(Vertex) == 64, "The size of the Vertex needs to be 64 for performance");

    int nr = 0;
    const float fsegments = static_cast<float>(_segments);
    const float r = static_cast<float>(GridRadius[0]);

    //int nr2 = 0;

    for (int nSegment = 0; nSegment <= _segments; ++nSegment) {
        // define an extra vertex around the y-axis due to texture mapping
        for (int j = 0; j <= _segments; j++) {
            const float fi = static_cast<float>(nSegment);
            const float fj = static_cast<float>(j);

            // inclination angle (north to south)
            const float theta = fi * float(M_PI) / fsegments*2.f;  // 0 -> PI

            // azimuth angle (east to west)
            const float phi = fj * float(M_PI) * 2.0f / fsegments;  // 0 -> 2*PI

            const float x = r * sin(phi) * sin(theta);  //
            const float y = r * cos(theta);             // up
            const float z = r * cos(phi) * sin(theta);  //
            
            glm::vec3 normal = glm::vec3(x, y, z);
            if (!(x == 0.f && y == 0.f && z == 0.f))
                normal = glm::normalize(normal);

            //const float t1 = fj / fsegments;
            const float t2 = fi / fsegments;

            // tex coord. not used, use to manip color 
            if (round(y) == 0.0f) _varray[nr].tex[0] = -2;
            _varray[nr].tex[1] = t2;

            glm::vec4 tmp(x, y, z, 1);
            glm::mat4 rot = glm::rotate(glm::mat4(1), static_cast<float>(M_PI_2), glm::vec3(1, 0, 0));
            tmp = _gridMatrix*rot*tmp;
            
            for (int i = 0; i < 3; i++){
                _varray[nr].location[i]  = tmp[i];
                _varray[nr].normal[i] = normal[i];
            }
            _varray[nr].location[3] = static_cast<GLfloat>(GridRadius[1]);
            ++nr;
        }
    }
    nr = 0;
    // define indices for all triangles
    for (int i = 1; i <= _segments; ++i) {
        for (int j = 0; j < _segments; ++j) {
            const int t = _segments + 1;
            _iarray[nr] = t * (i - 1) + j + 0; ++nr;
            _iarray[nr] = t * (i + 0) + j + 0; ++nr;
            _iarray[nr] = t * (i + 0) + j + 1; ++nr;
            _iarray[nr] = t * (i - 1) + j + 1; ++nr;
            _iarray[nr] = t * (i - 1) + j + 0; ++nr;
        }
    }
}

RenderableSphericalGrid::~RenderableSphericalGrid(){
    deinitialize();

    // Delete not done in deinitialize because new is done in constructor
    delete[] _varray;
    delete[] _iarray;
}

bool RenderableSphericalGrid::isReady() const {
    bool ready = true;
    ready &= (_gridProgram != nullptr);
    return ready;
}

bool RenderableSphericalGrid::deinitialize(){
    glDeleteVertexArrays(1,&_vaoID);
    _vaoID = 0;

    glDeleteBuffers(1,&_vBufferID);
    _vBufferID = 0;

    glDeleteBuffers(1,&_iBufferID);
    _iBufferID = 0;

    return true;
}

bool RenderableSphericalGrid::initialize(){
    bool completeSuccess = true;
    if (_gridProgram == nullptr)
        completeSuccess &= OsEng.ref().configurationManager().getValue("GridProgram", _gridProgram);

    // Initialize and upload to graphics card
    glGenVertexArrays(1, &_vaoID);
    glGenBuffers(1, &_vBufferID);
    glGenBuffers(1, &_iBufferID);

    // First VAO setup
    glBindVertexArray(_vaoID);
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
    glBufferData(GL_ARRAY_BUFFER, _vsize * sizeof(Vertex), _varray, GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(Vertex),
        reinterpret_cast<const GLvoid*>(offsetof(Vertex, location)));
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex),
        reinterpret_cast<const GLvoid*>(offsetof(Vertex, tex)));
    glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex),
        reinterpret_cast<const GLvoid*>(offsetof(Vertex, normal)));
    
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iBufferID);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, _isize * sizeof(int), _iarray, GL_STATIC_DRAW);

    glBindVertexArray(0);

    return completeSuccess;
}

void RenderableSphericalGrid::render(const RenderData& data){
    _gridProgram->activate();

    glm::mat4 transform;
    for (int i = 0; i < 3; i++){
        for (int j = 0; j < 3; j++){
            transform[i][j] = static_cast<float>(_parentMatrix[i][j]);
        }
    }


    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;

    // setup the data to the shader
    _gridProgram->setIgnoreUniformLocationError(IgnoreError::Yes);
    _gridProgram->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _gridProgram->setUniform("ModelTransform", transform);
    setPscUniforms(*_gridProgram, data.camera, data.position);
    _gridProgram->setUniform("gridColor", _gridColor);

    glLineWidth(0.5f);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    glEnable(GL_LINE_SMOOTH);

    glBindVertexArray(_vaoID);  // select first VAO
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iBufferID);
    glDrawElements(_mode, _isize, GL_UNSIGNED_INT, 0);
    glBindVertexArray(0);

    _gridProgram->deactivate();
}

void RenderableSphericalGrid::update(const UpdateData& data) {
    _parentMatrix = SpiceManager::ref().positionTransformMatrix("IAU_JUPITER", "GALACTIC", data.time);

}
}