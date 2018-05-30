/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
#include <openspace/rendering/renderable.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/invariants.h>
#include <ghoul/misc/templatefactory.h>
#include <fstream>

namespace {
    constexpr const char* _loggerCat = "ModelGeometry";

    constexpr const char* KeyType = "Type";
    constexpr const char* KeyGeomModelFile = "GeometryFile";
    constexpr const int8_t CurrentCacheVersion = 3;
} // namespace

namespace openspace::modelgeometry {

documentation:: Documentation ModelGeometry::Documentation() {
    using namespace documentation;
    return {
        "Model Geometry",
        "base_geometry_model",
        {
            {
                KeyType,
                new StringVerifier,
                Optional::No,
                "The type of the Model Geometry that should be generated"
            },
            {
                KeyGeomModelFile,
                new StringVerifier,
                Optional::No,
                "The file that should be loaded in this ModelGeometry. The file can "
                "contain filesystem tokens or can be specified relatively to the "
                "location of the .mod file."
            }
        }
    };
}


std::unique_ptr<ModelGeometry> ModelGeometry::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    if (!dictionary.hasKeyAndValue<std::string>(KeyType)) {
        throw ghoul::RuntimeError("Dictionary did not contain a key 'Type'");
    }

    const std::string& geometryType = dictionary.value<std::string>(KeyType);

    auto factory = FactoryManager::ref().factory<ModelGeometry>();
    return factory->create(geometryType, dictionary);;
}

ModelGeometry::ModelGeometry(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "ModelGeometry" })
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "ModelGeometry"
    );

    _file = absPath(dictionary.value<std::string>(KeyGeomModelFile));
}

double ModelGeometry::boundingRadius() const {
    return _boundingRadius;
}

void ModelGeometry::render() {
    glBindVertexArray(_vaoID);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _ibo);
    glDrawElements(
        _mode,
        static_cast<GLsizei>(_indices.size()),
        GL_UNSIGNED_INT,
        nullptr
    );
    glBindVertexArray(0);
}

void ModelGeometry::changeRenderMode(GLenum mode) {
    _mode = mode;
}

bool ModelGeometry::initialize(Renderable* parent) {
    float maximumDistanceSquared = 0;
    for (const Vertex& v : _vertices) {
        maximumDistanceSquared = glm::max(
            glm::pow(v.location[0], 2.f) +
            glm::pow(v.location[1], 2.f) +
            glm::pow(v.location[2], 2.f), maximumDistanceSquared);
    }
    _boundingRadius = maximumDistanceSquared;
    parent->setBoundingSphere(glm::sqrt(maximumDistanceSquared));

    if (_vertices.empty()) {
        return false;
    }

    glGenVertexArrays(1, &_vaoID);
    glGenBuffers(1, &_vbo);
    glGenBuffers(1, &_ibo);

    glBindVertexArray(_vaoID);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        _vertices.size() * sizeof(Vertex),
        _vertices.data(),
        GL_STATIC_DRAW
    );

    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(Vertex), nullptr);
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(Vertex),
        reinterpret_cast<const GLvoid*>(offsetof(Vertex, tex)) // NOLINT
    );
    glVertexAttribPointer(
        2,
        3,
        GL_FLOAT,
        GL_FALSE,
        sizeof(Vertex),
        reinterpret_cast<const GLvoid*>(offsetof(Vertex, normal)) // NOLINT
    );

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _ibo);
    glBufferData(
        GL_ELEMENT_ARRAY_BUFFER,
        _indices.size() * sizeof(int),
        _indices.data(),
        GL_STATIC_DRAW
    );

    glBindVertexArray(0);

    return true;
}

void ModelGeometry::deinitialize() {
    glDeleteBuffers(1, &_vbo);
    glDeleteVertexArrays(1, &_vaoID);
    glDeleteBuffers(1, &_ibo);
}

bool ModelGeometry::loadObj(const std::string& filename) {
    const std::string& cachedFile = FileSys.cacheManager()->cachedFilename(
        filename,
        ghoul::filesystem::CacheManager::Persistent::Yes
    );

    const bool hasCachedFile = FileSys.fileExists(cachedFile);
    if (hasCachedFile) {
        LINFO(fmt::format("Cached file '{}' used for file '{}", cachedFile, filename));

        const bool success = loadCachedFile(cachedFile);
        if (success) {
            return true;
        }
        else {
            FileSys.cacheManager()->removeCacheFile(filename);
        }
    }
    else {
        LINFO(fmt::format(
            "Cached file '{}' for file '{}' not found",
            cachedFile,
            filename
        ));
    }

    LINFO(fmt::format("Loading Model file '{}'", filename));
    const bool modelSuccess = loadModel(filename);

    if (!modelSuccess) {
        return false;
    }

    LINFO("Saving cache");
    const bool cacheSuccess = saveCachedFile(cachedFile);

    return cacheSuccess;
}

bool ModelGeometry::saveCachedFile(const std::string& filename) {
    std::ofstream fileStream(filename, std::ofstream::binary);
    if (fileStream.good()) {
        fileStream.write(
            reinterpret_cast<const char*>(&CurrentCacheVersion),
            sizeof(int8_t)
        );

        const int64_t vSize = _vertices.size();
        fileStream.write(reinterpret_cast<const char*>(&vSize), sizeof(int64_t));
        const int64_t iSize = _indices.size();
        fileStream.write(reinterpret_cast<const char*>(&iSize), sizeof(int64_t));

        fileStream.write(
            reinterpret_cast<const char*>(_vertices.data()),
            sizeof(Vertex) * vSize
        );
        fileStream.write(
            reinterpret_cast<const char*>(_indices.data()),
            sizeof(int) * iSize
        );

        return fileStream.good();
    }
    else {
        LERROR(fmt::format("Error opening file '{}' for save cache file", filename));
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

        int64_t vSize;
        fileStream.read(reinterpret_cast<char*>(&vSize), sizeof(int64_t));
        int64_t iSize;
        fileStream.read(reinterpret_cast<char*>(&iSize), sizeof(int64_t));

        if (vSize == 0 || iSize == 0) {
            LERROR(
                fmt::format("Error opening file '{}' for loading cache file", filename)
            );
            return false;
        }

        _vertices.resize(vSize);
        _indices.resize(iSize);

        fileStream.read(
            reinterpret_cast<char*>(_vertices.data()), sizeof(Vertex) * vSize
        );
        fileStream.read(reinterpret_cast<char*>(_indices.data()), sizeof(int) * iSize);

        return fileStream.good();
    }
    else {
        LERROR(fmt::format(
            "Error opening file '{}' for loading cache file",
            filename
        ));
        return false;
    }
}

void ModelGeometry::setUniforms(ghoul::opengl::ProgramObject&) {}

}  // namespace openspace::modelgeometry
