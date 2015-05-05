/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include <openspace/rendering/model/wavefrontgeometry.h>

#include <openspace/util/constants.h>
#include "ghoul/logging/logmanager.h"
#include <tiny_obj_loader.h>

namespace {
    const std::string _loggerCat = "WavefrontGeometry";
    const std::string keyObjFile = "ObjFile";
    const int8_t CurrentCacheVersion = 3;
}

namespace openspace {
namespace modelgeometry {

WavefrontGeometry::WavefrontGeometry(const ghoul::Dictionary& dictionary)
    : ModelGeometry(dictionary) 
{
	loadObj(_file);
}

bool WavefrontGeometry::initialize(RenderableModel* parent) {
	bool success = ModelGeometry::initialize(parent);
    return success;
}

void WavefrontGeometry::deinitialize() {
	ModelGeometry::deinitialize();
}

bool WavefrontGeometry::loadModel(const std::string& filename) {
	std::vector<tinyobj::shape_t> shapes;
	std::vector<tinyobj::material_t> materials;
    std::string err = tinyobj::LoadObj(shapes, materials, filename.c_str(), filename.c_str());

    if (!err.empty()) {
        LERROR(err);
        return false;
    }

    if (shapes.size() > 1) {
        LWARNING("Loading models with more than one shape is currently untested");
    }

    int totalSizeIndex = 0;
    int totalSizeVertex = 0;
    for (int i = 0; i < shapes.size(); ++i) {
        totalSizeIndex += shapes[i].mesh.indices.size();
        totalSizeVertex += shapes[i].mesh.positions.size();

        if (shapes[i].mesh.positions.size() != shapes[i].mesh.normals.size())
            LERROR(
                "#positions (" << shapes[i].mesh.positions.size() << ")"
                " != #normals (" << shapes[i].mesh.normals.size()
            );
    }

    _vertices.resize(totalSizeVertex);
    std::memset(_vertices.data(), 0, _vertices.size() * sizeof(Vertex));
    _indices.resize(totalSizeIndex);
    std::memset(_indices.data(), 0, _indices.size() * sizeof(int));

    // We add all shapes of the model into the same vertex array, one after the other
    // The _shapeCounts array stores for each shape, how many vertices that shape has
    int currentPosition = 0;
    int currentIndices = 0;
    int p = 0;
    for (int i = 0; i < shapes.size(); ++i) {
        for (int j = 0; j < shapes[i].mesh.positions.size() / 3; ++j) {
            _vertices[j + currentPosition].location[0] = shapes[i].mesh.positions[3 * j + 0];
            _vertices[j + currentPosition].location[1] = shapes[i].mesh.positions[3 * j + 1];
            _vertices[j + currentPosition].location[2] = shapes[i].mesh.positions[3 * j + 2];
            _vertices[j + currentPosition].location[3] = 5; // Temp size for the power scale coordinate.
			// Could be defined per object as a dictionary key.

            _vertices[j + currentPosition].normal[0] = shapes[i].mesh.normals[3 * j + 0];
            _vertices[j + currentPosition].normal[1] = shapes[i].mesh.normals[3 * j + 1];
            _vertices[j + currentPosition].normal[2] = shapes[i].mesh.normals[3 * j + 2];

            if (2 * j + 1 < shapes[i].mesh.texcoords.size()) {
                _vertices[j + currentPosition].tex[0] = shapes[i].mesh.texcoords[2 * j + 0];
                _vertices[j + currentPosition].tex[1] = shapes[i].mesh.texcoords[2 * j + 1];
            }
			
        }
        currentPosition += shapes[i].mesh.positions.size() / 3;

        std::copy(
            shapes[i].mesh.indices.begin(),
            shapes[i].mesh.indices.end(),
            _indices.begin() + p
            );
        p += shapes[i].mesh.indices.size();
    }

	return true;
}


}  // namespace modelgeometry
}  // namespace openspace
