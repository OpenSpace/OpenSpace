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
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include "ghoul/logging/logmanager.h"

#include <fstream>
#include <numeric>
#include <tiny_obj_loader.h>

namespace {
    const std::string _loggerCat = "WavefrontGeometry";

    const std::string keyObjFile = "ObjFile";

    const int8_t CurrentCacheVersion = 3;
}

namespace openspace {
namespace modelgeometry {

WavefrontGeometry::WavefrontGeometry(const ghoul::Dictionary& dictionary)
    : ModelGeometry()
{
	using constants::scenegraphnode::keyName;

    std::string name;
    bool success = dictionary.getValue(keyName, name);
    ghoul_assert(success, "Name tag was not present");

	std::string file;
	success = dictionary.getValue(keyObjFile, file);
	if (!success) {
        LERROR("WaveFrontGeometry of '" << name << "' did not provide a key '"
			                               << keyObjFile << "'");
	}
	file = FileSys.absolutePath(file);

    if (FileSys.fileExists(file, true))
        loadObj(file);
    else
        LERROR("Could not load OBJ file '" << file << "': File not found");
}

bool WavefrontGeometry::loadObj(const std::string& filename){
    std::string cachedFile = "";
    FileSys.cacheManager()->getCachedFile(filename, cachedFile, true);

    bool hasCachedFile = FileSys.fileExists(cachedFile);
    if (hasCachedFile) {
        LINFO("Cached file '" << cachedFile << "' used for Model file '" << filename << "'");

        bool success = loadCachedFile(cachedFile);
        if (success)
            return true;
        else
            FileSys.cacheManager()->removeCacheFile(filename);
        // Intentional fall-through to the 'else' computation to generate the cache
        // file for the next run
    }
    else {
        LINFO("Cache for Model'" << filename << "' not found");
    }
    LINFO("Loading OBJ file '" << filename << "'");
    bool success = loadModel(filename);
    if (!success)
        return false;

    LINFO("Saving cache");
    success = saveCachedFile(cachedFile);

    return success;
}

bool WavefrontGeometry::initialize(RenderableModel* parent) {
	bool success = ModelGeometry::initialize(parent);
    PowerScaledScalar ps = PowerScaledScalar(1.0, 0.0); // will set proper bounding soon.
    _parent->setBoundingSphere(ps);
	
	if (_vertices.empty())
        return false;
	glGenVertexArrays(1, &_vaoID);
	glGenBuffers(1, &_vbo);
    glGenBuffers(1, &_ibo);

	glBindVertexArray(_vaoID);
	glBindBuffer(GL_ARRAY_BUFFER, _vbo);
	glBufferData(GL_ARRAY_BUFFER, _vertices.size() * sizeof(Vertex), _vertices.data(), GL_STATIC_DRAW);

	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glEnableVertexAttribArray(2);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(Vertex),
		reinterpret_cast<const GLvoid*>(offsetof(Vertex, location)));
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex),
		reinterpret_cast<const GLvoid*>(offsetof(Vertex, tex))); 
	glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex),
		reinterpret_cast<const GLvoid*>(offsetof(Vertex, normal)));

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _ibo);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, _indices.size() * sizeof(int), _indices.data(), GL_STATIC_DRAW);

	glBindVertexArray(0);

    return success;
}

void WavefrontGeometry::deinitialize() {
	glDeleteBuffers(1, &_vbo);
	glDeleteVertexArrays(1, &_vaoID);
    glDeleteBuffers(1, &_ibo);
}

void WavefrontGeometry::render() {
	glBindVertexArray(_vaoID);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _ibo);
    glDrawElements(GL_TRIANGLES, _indices.size(), GL_UNSIGNED_INT, 0);
	glBindVertexArray(0);
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
            _vertices[j + currentPosition].location[3] = 5;

            _vertices[j + currentPosition].normal[0] = shapes[i].mesh.normals[3 * j + 0];
            _vertices[j + currentPosition].normal[1] = shapes[i].mesh.normals[3 * j + 1];
            _vertices[j + currentPosition].normal[2] = shapes[i].mesh.normals[3 * j + 2];

            if (2 * j + 1 < shapes[i].mesh.texcoords.size()) {
                _vertices[j + currentPosition].tex[0] = shapes[0].mesh.texcoords[2 * j + 0];
                _vertices[j + currentPosition].tex[1] = shapes[0].mesh.texcoords[2 * j + 1];
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

bool WavefrontGeometry::saveCachedFile(const std::string& filename) {
    std::ofstream fileStream(filename, std::ofstream::binary);
    if (fileStream.good()) {
        fileStream.write(reinterpret_cast<const char*>(&CurrentCacheVersion),
            sizeof(int8_t));

        int64_t vSize = _vertices.size();
        fileStream.write(reinterpret_cast<const char*>(&vSize), sizeof(int64_t));
        int64_t iSize = _indices.size();
        fileStream.write(reinterpret_cast<const char*>(&iSize), sizeof(int64_t));

        fileStream.write(reinterpret_cast<const char*>(_vertices.data()), sizeof(Vertex) * vSize);
        fileStream.write(reinterpret_cast<const char*>(_indices.data()), sizeof(int) * iSize);

        return fileStream.good();
    }
    else {
        LERROR("Error opening file '" << filename << "' for save cache file");
        return false;
    }
}

bool WavefrontGeometry::loadCachedFile(const std::string& filename) {
    std::ifstream fileStream(filename, std::ifstream::binary);
    if (fileStream.good()) {
        int8_t version = 0;
        fileStream.read(reinterpret_cast<char*>(&version), sizeof(int8_t));
        if (version != CurrentCacheVersion) {
            LINFO("The format of the cached file has changed, deleting old cache");
            fileStream.close();
            FileSys.deleteFile(filename);
            return false;
        }

        int64_t vSize, iSize;
        fileStream.read(reinterpret_cast<char*>(&vSize), sizeof(int64_t));
        fileStream.read(reinterpret_cast<char*>(&iSize), sizeof(int64_t));
        
        _vertices.resize(vSize);
        _indices.resize(iSize);

        fileStream.read(reinterpret_cast<char*>(_vertices.data()), sizeof(Vertex) * vSize);
        fileStream.read(reinterpret_cast<char*>(_indices.data()), sizeof(int) * iSize);

        return fileStream.good();
    }
    else {
        LERROR("Error opening file '" << filename << "' for loading cache file");
        return false;
    }
}

}  // namespace modelgeometry
}  // namespace openspace
