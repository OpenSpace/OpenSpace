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

#include <modules/base/rendering/multimodelgeometry.h>

#include "ghoul/logging/logmanager.h"
#include "ghoul/io/model/modelreadermultiformat.h"
#include "ghoul/opengl/vertexbufferobject.h"

#include <openspace/util/powerscaledcoordinate.h>

namespace {
    const std::string _loggerCat = "MultiModelGeometry";
}

namespace openspace {
namespace modelgeometry {

MultiModelGeometry::MultiModelGeometry(const ghoul::Dictionary& dictionary)
    : ModelGeometry(dictionary)
{
    loadObj(_file);
}

bool MultiModelGeometry::initialize(Renderable* parent) {
    bool success = ModelGeometry::initialize(parent);
    return success;
}

void MultiModelGeometry::deinitialize() {
    ModelGeometry::deinitialize();
}

bool MultiModelGeometry::loadModel(const std::string& filename)  {
    ghoul::io::ModelReaderMultiFormat modelReader;

    std::vector<ghoul::io::ModelReaderBase::Vertex> vertices;
    std::vector<int> indices;

    modelReader.loadModel(filename, vertices, indices);
         
    _vertices.reserve(vertices.size());
    for (const auto & v : vertices) {
        Vertex vv;
        memcpy(vv.location, v.location, sizeof(GLfloat) * 3);
        vv.location[3] = 1.0;
        //memcpy(vv.location, glm::value_ptr(p.vec4()), sizeof(GLfloat) * 4);
        memcpy(vv.tex, v.tex, sizeof(GLfloat) * 2);
        memcpy(vv.normal, v.normal, sizeof(GLfloat) * 3);
        _vertices.push_back(vv);
    }

    _indices.resize(indices.size());                
    std::copy(indices.begin(), indices.end(), _indices.begin());

    return true;
}

}  // namespace modelgeometry
}  // namespace openspace
