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

#include <modules/base/rendering/modelgeometry.h>

#include <openspace/documentation/verifier.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/dictionary.h>

#include <fstream>

namespace {
    const char* _loggerCat = "ModelGeometry";
    const char* keyGeomModelFile = "GeometryFile";
    const int8_t CurrentCacheVersion = 3;
    const char* keyType = "Type";
    const char* keyName = "Name";
}

namespace openspace {
namespace modelgeometry {

documentation:: Documentation ModelGeometry::Documentation() {
    using namespace documentation;
    return {
        "Model Geometry",
        "base_geometry_model",
        {
            {
                keyType,
                new StringVerifier,
                "The type of the Model Geometry that should be generated",
                Optional::No
            },
            {
                keyGeomModelFile,
                new StringVerifier,
                "The file that should be loaded in this ModelGeometry. The file can "
                "contain filesystem tokens or can be specified relatively to the "
                "location of the .mod file.",
                Optional::No
            }
        }
    };
}


std::unique_ptr<ModelGeometry> ModelGeometry::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    if (!dictionary.hasKeyAndValue<std::string>(keyType)) {
        throw ghoul::RuntimeError("Dictionary did not contain a key 'Type'");
    }

    std::string geometryType = dictionary.value<std::string>(keyType);
    auto factory = FactoryManager::ref().factory<ModelGeometry>();

    std::unique_ptr<ModelGeometry> result = factory->create(geometryType, dictionary);
    if (result == nullptr) {
        throw ghoul::RuntimeError(
            "Failed to create a ModelGeometry object of type '" + geometryType + "'"
        );
    }
    return result;
}

ModelGeometry::ModelGeometry(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner("ModelGeometry")
    , _parent(nullptr)
    , _mode(GL_TRIANGLES)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "ModelGeometry"
    );

    std::string name;
    bool success = dictionary.getValue(keyName, name);
    ghoul_assert(success, "Name tag was not present");

    _file = absPath(dictionary.value<std::string>(keyGeomModelFile));
}

double ModelGeometry::boundingRadius() const {
    double maxDistSquared = 0;
    double distSquared;
    for (const Vertex& v : _vertices) {
        distSquared = // x*x + y*y + z*z
            v.location[0] * v.location[0] +
            v.location[1] * v.location[1] +
            v.location[2] * v.location[2];
        maxDistSquared = glm::max(maxDistSquared, distSquared);
    }
    double maxDist = std::sqrt(maxDistSquared);
    return maxDist;
}


ModelGeometry::~ModelGeometry() {
}

void ModelGeometry::render() {
    glBindVertexArray(_vaoID);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _ibo);
    glDrawElements(_mode, static_cast<GLsizei>(_indices.size()), GL_UNSIGNED_INT, 0);
    glBindVertexArray(0);
}

void ModelGeometry::changeRenderMode(const GLenum mode) {
    _mode = mode;
}

bool ModelGeometry::initialize(Renderable* parent) {
    _parent = parent;
    float maximumDistanceSquared = 0;
    for (auto v: _vertices)
    {
        maximumDistanceSquared = glm::max(
            glm::pow(v.location[0], 2.f) +
            glm::pow(v.location[1], 2.f) +
            glm::pow(v.location[2], 2.f), maximumDistanceSquared);
    }
    _parent->setBoundingSphere(PowerScaledScalar(glm::sqrt(maximumDistanceSquared), 0.0));

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

bool ModelGeometry::loadObj(const std::string& filename) {
    std::string cachedFile = FileSys.cacheManager()->cachedFilename(
        filename,
        ghoul::filesystem::CacheManager::Persistent::Yes
        );

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
        LINFO("Cached file '" << cachedFile << "' used for Model file '" << filename << "' not found");
    }

    LINFO("Loading Model file '" << filename << "'");
    bool success = loadModel(filename);

    if (!success)
        return false;

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

        if (vSize == 0 || iSize == 0) {
            LERROR("Error opening file '" << filename << "' for loading cache file");
            return false;
        }

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

bool ModelGeometry::getVertices(std::vector<Vertex>* vertexList) {
    vertexList->clear();
    for (auto v : _vertices)
        vertexList->push_back(v);

    return !(vertexList->empty());
}

bool ModelGeometry::getIndices(std::vector<int>* indexList) {
    indexList->clear();
    for (auto i : _indices)
        indexList->push_back(i);

    return !(indexList->empty());
}

void ModelGeometry::setUniforms(ghoul::opengl::ProgramObject& program) {
}

}  // namespace modelgeometry
}  // namespace openspace
