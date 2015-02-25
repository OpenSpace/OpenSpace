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

namespace {
    const std::string _loggerCat = "WavefrontGeometry";

    const int8_t CurrentCacheVersion = 1;
}

namespace openspace {
namespace modelgeometry {

WavefrontGeometry::WavefrontGeometry(const ghoul::Dictionary& dictionary)
    : ModelGeometry()
	, _isize(0)
	, _vsize(0)
	, _varray(nullptr)
	, _iarray(nullptr)
	, _tarray(nullptr)
{
	using constants::scenegraphnode::keyName;

	// The name is passed down from the SceneGraphNode
    std::string name;
    bool success = dictionary.getValue(keyName, name);
    ghoul_assert(success, "Name tag was not present");

	std::string file;
	success = dictionary.getValue(constants::modelgeometry::keyObjFile, file);
	if (!success) {
        LERROR("SimpleSphereGeometry of '" << name << "' did not provide a key '"
			                               << constants::modelgeometry::keyObjFile << "'");
	}
	const std::string filename = FileSys.absolutePath(file);

    if (FileSys.fileExists(filename))
        loadObj(filename);
    else
        LERROR("Could not load OBJ file '" << filename << "': File not found");
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




    //if (shapes[0].mesh.texcoords.size() > 0) {
    //    for (int k = 0; k < shapes[0].mesh.texcoords.size(); ++k) {

    //    }
    //}
	//
	//if (shapes[0].mesh.texcoords.size() > 0) {
	//	for (size_t k = 0; k < shapes[0].mesh.indices.size() / 3; k++) {
	//		for (int j = 0; j < 3; j++) {
	//			int idx = shapes[0].mesh.indices[3 * k + j];

	//			_varray[p].tex[0] = shapes[0].mesh.texcoords[2 * idx + 0];
	//			_varray[p].tex[1] = shapes[0].mesh.texcoords[2 * idx + 1];
	//			p++;
	//		}
	//	}
	//}
	

	//testing with one triangle - works. 
	/*
	_varray[0].location[0] = 0;
	_varray[0].location[1] = 0;
	_varray[0].location[2] = 0;
	_varray[0].location[3] = 5;

	_varray[0].normal[0] = 1;
	_varray[0].normal[1] = 1;
	_varray[0].normal[2] = 1;

	_varray[0].tex[0] = 0;
	_varray[0].tex[1] = 0;

	_varray[1].location[0] = 1;
	_varray[1].location[1] = 0;
	_varray[1].location[2] = 0;
	_varray[1].location[3] = 5;

	_varray[1].normal[0] = 1;
	_varray[1].normal[1] = 1;
	_varray[1].normal[2] = 1;

	_varray[1].tex[0] = 1;
	_varray[1].tex[1] = 0;

	_varray[2].location[0] = 0;
	_varray[2].location[1] = 1;
	_varray[2].location[2] = 0;
	_varray[2].location[3] = 5;

	_varray[2].normal[0] = 1;
	_varray[2].normal[1] = 1;
	_varray[2].normal[2] = 1;

	_varray[2].tex[0] = 0;
	_varray[2].tex[1] = 1;

	_vsize = 3;
	_isize = 3;

	_iarray[0] = 0;
	_iarray[1] = 1;
	_iarray[2] = 2;
	*/
}


WavefrontGeometry::~WavefrontGeometry(){
}

bool WavefrontGeometry::initialize(RenderableModel* parent){
	bool success = ModelGeometry::initialize(parent);
    createSphere();
	
	if (_isize == 0) return false;
	glGenVertexArrays(1, &_vaoID);
	glGenBuffers(1, &_vBufferID);
	glGenBuffers(1, &_iBufferID);

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

	GLint errorID = glGetError();
	if (errorID != GL_NO_ERROR) {
		LERROR("OpenGL error: " << glewGetErrorString(errorID));
		return false;
	}
    return success;
}

void WavefrontGeometry::deinitialize(){
	if (_varray)
		delete[] _varray;
	if (_iarray)
		delete[] _iarray;

	glDeleteBuffers(1, &_vBufferID);
	glDeleteBuffers(1, &_iBufferID);
	glDeleteVertexArrays(1, &_vaoID);
    
}

void WavefrontGeometry::render(){
	// render
	glBindVertexArray(_vaoID);  // select first VAO
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iBufferID);
	glDrawElements(GL_TRIANGLES, _isize, GL_UNSIGNED_INT, 0);
	glBindVertexArray(0);
}

void WavefrontGeometry::createSphere(){
   // create the power scaled scalar
	PowerScaledScalar ps = PowerScaledScalar(1.0, 0.0); // will set proper bounding soon.
	_parent->setBoundingSphere(ps);
}

bool WavefrontGeometry::loadModel(const std::string& filename) {
    std::string err = tinyobj::LoadObj(shapes, materials, filename.c_str(), filename.c_str());

    if (!err.empty()) {
        LERROR(err);
        return false;
    }

    LINFO("Loaded Mesh");
    LINFO("Number of Shapes: " << shapes.size());
    LINFO("Number of Materials: " << materials.size());
    for (int i = 0; i < shapes.size(); ++i) {
        LINFO("Shape #" << i << ": " <<
            "Indices   (" << shapes[i].mesh.indices.size() << ") " <<
            "Positions (" << shapes[i].mesh.positions.size() << ") " <<
            "Texture   (" << shapes[i].mesh.texcoords.size() << ") " <<
            "Normals   (" << shapes[i].mesh.normals.size() << ")");
    }

    _isize = shapes[0].mesh.indices.size();
    _vsize = shapes[0].mesh.indices.size(); // shapes[0].mesh.positions.size() + shapes[0].mesh.positions.size() / 3;
    _tsize = shapes[0].mesh.texcoords.size();

    _varray = new Vertex[_vsize];
    _iarray = new int[_isize];

    //copy indices
    for (int f = 0; f < shapes[0].mesh.indices.size(); f++) {
        _iarray[f] = f;// shapes[0].mesh.indices[f];
    }

    //shapes[0].mesh.texcoords.resize(2 * _isize);
    int p = 0;
    for (auto v : shapes[0].mesh.indices) {
        _varray[p].location[0] = shapes[0].mesh.positions[3 * v + 0];
        _varray[p].location[1] = shapes[0].mesh.positions[3 * v + 1];
        _varray[p].location[2] = shapes[0].mesh.positions[3 * v + 2];
        _varray[p].location[3] = 5;

        _varray[p].normal[0] = shapes[0].mesh.normals[3 * v + 0];
        _varray[p].normal[1] = shapes[0].mesh.normals[3 * v + 1];
        _varray[p].normal[2] = shapes[0].mesh.normals[3 * v + 2];

        // Only set the texture coordinates if they don't fall out of the value range
        _varray[p].tex[0] = (2 * v + 0) < shapes[0].mesh.texcoords.size() ? shapes[0].mesh.texcoords[2 * v + 0] : 0.f;
        _varray[p].tex[1] = (2 * v + 1) < shapes[0].mesh.texcoords.size() ? shapes[0].mesh.texcoords[2 * v + 1] : 0.f;

        p++;
    }
    p = 0;

    return true;
}

bool WavefrontGeometry::saveCachedFile(const std::string& filename) {
    std::ofstream fileStream(filename, std::ofstream::binary);
    if (fileStream.good()) {
        fileStream.write(reinterpret_cast<const char*>(&CurrentCacheVersion),
            sizeof(int8_t));

        int64_t vSize = _vsize;
        fileStream.write(reinterpret_cast<const char*>(&vSize), sizeof(int64_t));
        int64_t iSize = _isize;
        fileStream.write(reinterpret_cast<const char*>(&iSize), sizeof(int64_t));
        int64_t tSize = _tsize;
        fileStream.write(reinterpret_cast<const char*>(&tSize), sizeof(int64_t));

        fileStream.write(reinterpret_cast<const char*>(_varray), sizeof(Vertex) * _vsize);
        fileStream.write(reinterpret_cast<const char*>(_iarray), sizeof(int) * _isize);

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

        int64_t vSize, iSize, tSize;
        fileStream.read(reinterpret_cast<char*>(&vSize), sizeof(int64_t));
        fileStream.read(reinterpret_cast<char*>(&iSize), sizeof(int64_t));
        fileStream.read(reinterpret_cast<char*>(&tSize), sizeof(int64_t));
        _vsize = vSize;
        _isize = iSize;
        _tsize = tSize;

        _varray = new Vertex[vSize];
        _iarray = new int[iSize];

        fileStream.read(reinterpret_cast<char*>(_varray), sizeof(Vertex) * vSize);
        fileStream.read(reinterpret_cast<char*>(_iarray), sizeof(int) * iSize);

        return fileStream.good();
    }
    else {
        LERROR("Error opening file '" << filename << "' for loading cache file");
        return false;
    }
}

}  // namespace modelgeometry
}  // namespace openspace
