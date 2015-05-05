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

#include <openspace/rendering/model/modelgeometry.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/constants.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>

#include <fstream>
#include <openspace/rendering/model/wavefrontgeometry.h>

namespace {
    const std::string _loggerCat = "ModelGeometry";
	const std::string keyObjFile = "ObjFile";
	const int8_t CurrentCacheVersion = 3;
    const std::string keyType = "Type";
}

namespace openspace {
namespace modelgeometry {

ModelGeometry* ModelGeometry::createFromDictionary(const ghoul::Dictionary& dictionary) {
	std::string geometryType;
	const bool success = dictionary.getValue(
		keyType, geometryType);
	if (!success) {
        LERROR("ModelGeometry did not contain a correct value of the key '"
			<< keyType << "'");
        return nullptr;
	}
	ghoul::TemplateFactory<ModelGeometry>* factory
		= FactoryManager::ref().factory<ModelGeometry>();

	ModelGeometry* result = factory->create(geometryType, dictionary);
    if (result == nullptr) {
        LERROR("Failed to create a ModelGeometry object of type '" << geometryType
                                                                    << "'");
        return nullptr;
    }
    return result;
}

ModelGeometry::ModelGeometry(const ghoul::Dictionary& dictionary)
    : _parent(nullptr)
	, _mode(GL_TRIANGLES)
{
	setName("ModelGeometry");
	using constants::scenegraphnode::keyName;

	std::string name;
	bool success = dictionary.getValue(keyName, name);
	ghoul_assert(success, "Name tag was not present");

	success = dictionary.getValue(keyObjFile, _file);
	if (!success) {
		LERROR("WaveFrontGeometry of '" << name << "' did not provide a key '"
			<< keyObjFile << "'");
	}
	_file = FileSys.absolutePath(_file);

	if (!FileSys.fileExists(_file, true))
		LERROR("Could not load OBJ file '" << _file << "': File not found");
	
}

ModelGeometry::~ModelGeometry() {
}

void ModelGeometry::render() {
	glBindVertexArray(_vaoID);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _ibo);
	glDrawElements(_mode, _indices.size(), GL_UNSIGNED_INT, 0);
	glBindVertexArray(0);
}

void ModelGeometry::changeRenderMode(const GLenum mode){
	_mode = mode;
}

bool ModelGeometry::initialize(RenderableModel* parent) {
    _parent = parent;
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

    return true;
}

void ModelGeometry::deinitialize() {
	glDeleteBuffers(1, &_vbo);
	glDeleteVertexArrays(1, &_vaoID);
	glDeleteBuffers(1, &_ibo);
}

bool ModelGeometry::loadObj(const std::string& filename){
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
		LINFO("Loading Model file '" << filename << "'");
		bool success = loadModel(filename);
		if (!success)
			//return false;

		LINFO("Saving cache");
		success = saveCachedFile(cachedFile);

		return success;
}

bool ModelGeometry::saveCachedFile(const std::string& filename) {
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

bool ModelGeometry::loadCachedFile(const std::string& filename) {
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
