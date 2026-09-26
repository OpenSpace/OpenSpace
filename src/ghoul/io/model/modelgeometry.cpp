/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#include <ghoul/io/model/modelgeometry.h>

#include <ghoul/format.h>
#include <ghoul/io/model/modelmesh.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/profiling.h>
#include <glm/gtc/type_ptr.hpp>
#include <algorithm>
#include <array>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <fstream>
#include <limits>
#include <string_view>
#include <utility>

namespace {
    using namespace ghoul;

    constexpr std::string_view _loggerCat = "ModelGeometry";
    constexpr int8_t CurrentCacheVersion = 12;
    constexpr int FormatStringSize = 4;
    constexpr int8_t ShouldSkipMarker = -1;
    constexpr int8_t NoSkipMarker = 1;

    opengl::Texture::Format stringToFormat(std::string_view format) {
        using Format = opengl::Texture::Format;
        if (format == "Red ")      { return Format::Red; }
        else if (format == "RG  ") { return Format::RG; }
        else if (format == "RGB ") { return Format::RGB; }
        else if (format == "BGR ") { return Format::BGR; }
        else if (format == "RGBA") { return Format::RGBA; }
        else if (format == "BGRA") { return Format::BGRA; }
        else if (format == "Dept") { return Format::DepthComponent; }
        else                       { throw MissingCaseException(); }
    }

    std::string formatToString(opengl::Texture::Format format) {
        using Format = opengl::Texture::Format;
        switch (format) {
            case Format::Red:            return "Red ";
            case Format::RG:             return "RG  ";
            case Format::RGB:            return "RGB ";
            case Format::BGR:            return "BGR ";
            case Format::RGBA:           return "RGBA";
            case Format::BGRA:           return "BGRA";
            case Format::DepthComponent: return "Dept";
            default:                     throw MissingCaseException();
        }
    }

    GLenum stringToDataType(std::string_view dataType) {
        if (dataType == "byte")      { return GL_BYTE; }
        else if (dataType == "ubyt") { return GL_UNSIGNED_BYTE; }
        else if (dataType == "shor") { return GL_SHORT; }
        else if (dataType == "usho") { return GL_UNSIGNED_SHORT; }
        else if (dataType == "int ") { return GL_INT; }
        else if (dataType == "uint") { return GL_UNSIGNED_INT; }
        else if (dataType == "floa") { return GL_FLOAT; }
        else if (dataType == "doub") { return GL_DOUBLE; }
        else                         { throw MissingCaseException(); }
    }

    std::string dataTypeToString(GLenum dataType) {
        switch (dataType) {
            case GL_BYTE:           return "byte";
            case GL_UNSIGNED_BYTE:  return "ubyt";
            case GL_SHORT:          return "shor";
            case GL_UNSIGNED_SHORT: return "usho";
            case GL_INT:            return "int ";
            case GL_UNSIGNED_INT:   return "uint";
            case GL_FLOAT:          return "floa";
            case GL_DOUBLE:         return "doub";
            default:                throw MissingCaseException();
        }
    }

    void calculateBoundingRadiusRecursive(const std::vector<io::ModelNode>& nodes,
                                          const io::ModelNode* node,
                                          const glm::mat4& parentTransform,
                                          float& maximumDistanceSquared)
    {
        if (!node) {
            LERROR("Cannot calculate bounding radius for empty node");
            return;
        }

        // NOTE: The bounding radius will not change along with an animation
        glm::mat4 globalTransform = parentTransform * node->transform();

        for (const io::ModelMesh& mesh : node->meshes()) {
            const float d = mesh.calculateBoundingRadius(globalTransform);
            maximumDistanceSquared = std::max(d, maximumDistanceSquared);
        }

        for (const int child : node->children()) {
            calculateBoundingRadiusRecursive(
                nodes,
                &nodes[child],
                globalTransform,
                maximumDistanceSquared
            );
        }
    }

    void renderRecursive(const std::vector<io::ModelNode>& nodes,
                         const io::ModelNode* node, opengl::ProgramObject& program,
                         const glm::mat4& parentTransform, bool isFullyTexturedModel,
                         bool isProjection, bool replaceWithCustomTransforms)
    {
        if (!node) {
            LERROR("Cannot render empty node");
            return;
        }

        glm::mat4 animationTransform = glm::mat4(1.f);
        glm::mat4 customTransform = glm::mat4(1.f);
        glm::mat4 nodeTransform = node->transform();

        if (node->hasAnimation()) {
            // Animation is given by Assimp in absolute format, i.e. animation replaces
            // old transform
            animationTransform = node->animationTransform();
            nodeTransform = glm::mat4(1.f);
        }

        if (node->hasCustomTransform()) {
            if (replaceWithCustomTransforms) {
                nodeTransform = glm::mat4(1.f);
            }

            customTransform = node->customTransform();
        }

        glm::mat4 globalTransform =
            parentTransform * animationTransform * customTransform * nodeTransform;

        for (const io::ModelMesh& mesh : node->meshes()) {
            mesh.render(program, globalTransform, isFullyTexturedModel, isProjection);
        }

        for (const int child : node->children()) {
            renderRecursive(
                nodes,
                &nodes[child],
                program,
                globalTransform,
                isFullyTexturedModel,
                isProjection,
                replaceWithCustomTransforms
            );
        }
    }
} // namespace

namespace ghoul::modelgeometry {

ModelGeometry::ModelCacheException::ModelCacheException(std::filesystem::path file,
                                                        std::string msg)
    : RuntimeError(std::format("Error '{}' with cache file '{}'", msg, file))
    , filename(std::move(file))
    , errorMessage(std::move(msg))
{}

ModelGeometry::ModelGeometry(std::vector<io::ModelNode> nodes,
                             std::vector<TextureEntry> textureStorage,
                             std::unique_ptr<io::ModelAnimation> animation,
                             bool isTransparent, bool hasCalcTransparency)
    : _nodes(std::move(nodes))
    , _textureStorage(std::move(textureStorage))
    , _animation(std::move(animation))
    , _hasCalcTransparency(hasCalcTransparency)
    , _isTransparent(isTransparent)
{
    if (!_hasCalcTransparency) {
        calculateTransparency();
    }
}

std::unique_ptr<modelgeometry::ModelGeometry> ModelGeometry::loadCacheFile(
                                                  const std::filesystem::path& cachedFile,
                                                                bool forceRenderInvisible,
                                                              bool notifyInvisibleDropped)
{
    ZoneScoped;

    std::ifstream fileStream = std::ifstream(cachedFile, std::ifstream::binary);
    if (!fileStream.good()) {
        throw ModelCacheException(cachedFile, "Could not open file to load cache");
    }

    // Check the caching version
    int8_t version = 0;
    fileStream.read(reinterpret_cast<char*>(&version), sizeof(int8_t));
    if (version != CurrentCacheVersion) {
        throw ModelCacheException(
            cachedFile,
            "The format of the cached file has changed"
        );
    }

    // First read the textureEntries
    int32_t nTextureEntries = 0;
    fileStream.read(reinterpret_cast<char*>(&nTextureEntries), sizeof(int32_t));
    if (nTextureEntries == 0) {
        LINFO("No TextureEntries were loaded while loading cache");
    }
    else if (nTextureEntries < 0) {
        std::string message = std::format(
            "Model cannot have negative number of texture entries while loading "
            "cache: {}", nTextureEntries
        );
        throw ModelCacheException(cachedFile, message);
    }
    std::vector<modelgeometry::ModelGeometry::TextureEntry> textureStorageArray;
    textureStorageArray.reserve(nTextureEntries);

    for (int32_t te = 0; te < nTextureEntries; te++) {
        modelgeometry::ModelGeometry::TextureEntry textureEntry;

        // Name
        int32_t nameSize = 0;
        fileStream.read(reinterpret_cast<char*>(&nameSize), sizeof(int32_t));
        if (nameSize <= 0) {
            throw ModelCacheException(
                cachedFile,
                "No texture name was found while loading cache"
            );
        }
        textureEntry.name.resize(nameSize);
        fileStream.read(textureEntry.name.data(), nameSize * sizeof(char));

        // Texture
        // Dimensions
        std::array<int32_t, 3> dimensionStorage = {};
        fileStream.read(
            reinterpret_cast<char*>(dimensionStorage.data()),
            3 * sizeof(int32_t)
        );
        const glm::uvec3 dimensions = glm::uvec3(
            static_cast<unsigned int>(dimensionStorage[0]),
            static_cast<unsigned int>(dimensionStorage[1]),
            static_cast<unsigned int>(dimensionStorage[2])
        );

        // Format
        std::string formatString;
        formatString.resize(FormatStringSize);
        fileStream.read(formatString.data(), FormatStringSize * sizeof(char));
        const opengl::Texture::Format format = stringToFormat(formatString);

        // Internal format
        uint32_t rawInternalFormat = 0;
        fileStream.read(reinterpret_cast<char*>(&rawInternalFormat), sizeof(uint32_t));
        const GLenum internalFormat = static_cast<GLenum>(rawInternalFormat);

        // Data type
        std::string dataTypeString;
        dataTypeString.resize(FormatStringSize);
        fileStream.read(dataTypeString.data(), FormatStringSize * sizeof(char));
        const GLenum dataType = stringToDataType(dataTypeString);

        // Data
        int32_t textureSize = 0;
        fileStream.read(reinterpret_cast<char*>(&textureSize), sizeof(int32_t));
        if (textureSize <= 0) {
            throw ModelCacheException(
                cachedFile,
                "No texture size was found while loading cache"
            );
        }
        std::byte* data = new std::byte[textureSize];
        fileStream.read(reinterpret_cast<char*>(data), textureSize);

        textureEntry.texture = std::make_unique<opengl::Texture>(
            opengl::Texture::FormatInit {
                .dimensions = dimensions,
                .type = GL_TEXTURE_2D,
                .format = format,
                .dataType = dataType,
                .internalFormat = internalFormat
            },
            opengl::Texture::SamplerInit {
                .filter = opengl::Texture::FilterMode::AnisotropicMipMap
            },
            data
        );

        textureStorageArray.push_back(std::move(textureEntry));
    }

    // Read how many nodes to read
    int32_t nNodes = 0;
    fileStream.read(reinterpret_cast<char*>(&nNodes), sizeof(int32_t));
    if (nNodes <= 0) {
        throw ModelCacheException(cachedFile, "No nodes were found while loading cache");
    }

    // Nodes
    std::vector<io::ModelNode> nodeArray;
    nodeArray.reserve(nNodes);
    for (int32_t n = 0; n < nNodes; n++) {
        // Read how many meshes to read
        int32_t nMeshes = 0;
        fileStream.read(reinterpret_cast<char*>(&nMeshes), sizeof(int32_t));
        if (nMeshes < 0) {
            std::string message = std::format(
                "Model cannot have negative number of meshes while loading cache: {}",
                nMeshes
            );
            throw ModelCacheException(cachedFile, message);
        }

        // Meshes
        std::vector<io::ModelMesh> meshArray;
        meshArray.reserve(nMeshes);
        for (int32_t m = 0; m < nMeshes; m++) {
            // HasVertexColors
            uint8_t col = 0;
            fileStream.read(reinterpret_cast<char*>(&col), sizeof(uint8_t));
            const bool hasVertexColors = (col == 1);

            // Vertices
            int32_t nVertices = 0;
            fileStream.read(reinterpret_cast<char*>(&nVertices), sizeof(int32_t));
            if (nVertices <= 0) {
                throw ModelCacheException(
                    cachedFile,
                    "No vertices were found while loading cache"
                );
            }
            std::vector<io::ModelMesh::Vertex> vertexArray;
            vertexArray.reserve(nVertices);

            for (int32_t v = 0; v < nVertices; v++) {
                io::ModelMesh::Vertex vertex;
                fileStream.read(
                    reinterpret_cast<char*>(&vertex),
                    sizeof(io::ModelMesh::Vertex)
                );
                vertexArray.push_back(std::move(vertex));
            }

            // Indices
            int32_t nIndices = 0;
            fileStream.read(reinterpret_cast<char*>(&nIndices), sizeof(int32_t));
            if (nIndices <= 0) {
                throw ModelCacheException(
                    cachedFile,
                    "No indices were found while loading cache"
                );
            }
            std::vector<uint32_t> indexArray;
            indexArray.resize(nIndices);
            fileStream.read(
                reinterpret_cast<char*>(indexArray.data()),
                nIndices * sizeof(uint32_t)
            );

            // IsInvisible
            uint8_t inv = 0;
            fileStream.read(reinterpret_cast<char*>(&inv), sizeof(uint8_t));
            const bool isInvisible = (inv == 1);

            // Textures
            int32_t nTextures = 0;
            fileStream.read(reinterpret_cast<char*>(&nTextures), sizeof(int32_t));
            if (nTextures <= 0 && !isInvisible) {
                throw ModelCacheException(
                    cachedFile,
                    "No materials were found while loading cache"
                );
            }
            std::vector<io::ModelMesh::Texture> textureArray;
            textureArray.reserve(nTextures);

            for (int32_t t = 0; t < nTextures; t++) {
                io::ModelMesh::Texture texture;

                // Skip marker
                int8_t skip = 0;
                fileStream.read(reinterpret_cast<char*>(&skip), sizeof(int8_t));
                if (skip == ShouldSkipMarker) {
                    continue;
                }

                // Type
                fileStream.read(reinterpret_cast<char*>(&texture.type), sizeof(uint8_t));

                // HasTexture
                uint8_t h = 0;
                fileStream.read(reinterpret_cast<char*>(&h), sizeof(uint8_t));
                texture.hasTexture = (h == 1);

                // Color
                fileStream.read(
                    reinterpret_cast<char*>(&texture.color.r),
                    4 * sizeof(float)
                );

                // IsTransparent
                uint8_t isT = 0;
                fileStream.read(reinterpret_cast<char*>(&isT), sizeof(uint8_t));
                texture.isTransparent = (isT == 1);

                // Texture
                if (texture.hasTexture) {
                    // Read which index in the textureStorageArray that this texture
                    // should point to
                    uint32_t index = 0;
                    fileStream.read(reinterpret_cast<char*>(&index), sizeof(uint32_t));

                    if (index >= textureStorageArray.size()) {
                        throw ModelCacheException(
                            cachedFile,
                            "Texture index is outside textureStorage during cache loading"
                        );
                    }

                    texture.texture = textureStorageArray[index].texture.get();
                }
                textureArray.push_back(std::move(texture));
            }

            // If mesh is invisible then check if it should be forced to render with
            // flashy colors and/or there should ba a notification
            if (isInvisible) {
                if (forceRenderInvisible) {
                    // Force invisible mesh to render with flashy colors
                    io::ModelMesh::Texture texture;
                    io::ModelMesh::generateDebugTexture(texture);
                    textureArray.push_back(std::move(texture));
                }
                else if (notifyInvisibleDropped) {
                    LINFO("An invisible mesh has been dropped while loading from cache");
                }
            }

            // Make mesh
            meshArray.emplace_back(
                std::move(vertexArray),
                std::move(indexArray),
                std::move(textureArray),
                isInvisible,
                hasVertexColors
            );
        }

        // Transform
        float rawTransform[16];
        fileStream.read(reinterpret_cast<char*>(rawTransform), 16 * sizeof(float));
        glm::mat4 transform = glm::make_mat4(rawTransform);

        // AnimationTransform
        float rawAnimTransform[16];
        fileStream.read(reinterpret_cast<char*>(&rawAnimTransform), 16 * sizeof(float));
        const glm::mat4 animationTransform = glm::make_mat4(rawAnimTransform);

        // Parent
        int32_t parent = 0;
        fileStream.read(reinterpret_cast<char*>(&parent), sizeof(int32_t));

        // Read how many children to read
        int32_t nChildren = 0;
        fileStream.read(reinterpret_cast<char*>(&nChildren), sizeof(int32_t));
        if (nChildren < 0) {
            std::string message = std::format(
                "Model cannot have negative number of children while loading cache: {}",
                nChildren
            );
            throw ModelCacheException(cachedFile, message);
        }

        // Children
        std::vector<int32_t> childrenArray;
        childrenArray.resize(nChildren);
        fileStream.read(
            reinterpret_cast<char*>(childrenArray.data()),
            nChildren * sizeof(int32_t)
        );

        // HasAnimation
        uint8_t a = 0;
        fileStream.read(reinterpret_cast<char*>(&a), sizeof(uint8_t));
        const bool hasAnimation = (a == 1);

        // Name
        int32_t nChars = 0;
        fileStream.read(reinterpret_cast<char*>(&nChars), sizeof(int32_t));
        if (nChars < 0) {
            std::string message = std::format(
                "Model node name cannot have negative number of characters while loading "
                "cache: {}", nChars
            );
            throw ModelCacheException(cachedFile, message);
        }
        std::string name;
        name.resize(nChars);
        fileStream.read(reinterpret_cast<char*>(name.data()), nChars * sizeof(char));

        // Create Node
        io::ModelNode node = io::ModelNode(
            name,
            std::move(transform),
            std::move(meshArray)
        );
        node.setChildren(std::move(childrenArray));
        node.setParent(parent);
        if (hasAnimation) {
            node.setAnimation(animationTransform);
        }

        nodeArray.push_back(std::move(node));
    }

    // Animation
    uint8_t anim = 0;
    fileStream.read(reinterpret_cast<char*>(&anim), sizeof(uint8_t));
    const bool hasAnimation = (anim == 1);

    if (hasAnimation) {
        // Name
        uint8_t nameSize = 0;
        fileStream.read(reinterpret_cast<char*>(&nameSize), sizeof(uint8_t));
        std::string name;
        name.resize(nameSize);
        fileStream.read(name.data(), nameSize * sizeof(char));

        // Duration
        double duration = 0.0;
        fileStream.read(reinterpret_cast<char*>(&duration), sizeof(double));

        // Read how many NodeAnimations to read
        int32_t nNodeAnimations = 0;
        fileStream.read(reinterpret_cast<char*>(&nNodeAnimations), sizeof(int32_t));
        if (nNodeAnimations <= 0) {
            throw ModelCacheException(
                cachedFile,
                "No node animations were found while loading cache"
            );
        }

        // NodeAnimations
        auto animation = std::make_unique<io::ModelAnimation>(name, duration);
        animation->nodeAnimations().reserve(nNodeAnimations);
        for (int32_t na = 0; na < nNodeAnimations; na++) {
            io::ModelAnimation::NodeAnimation nodeAnimation;

            // Node index
            int32_t nodeIndex = 0;
            fileStream.read(reinterpret_cast<char*>(&nodeIndex), sizeof(int32_t));
            nodeAnimation.node = nodeIndex;

            // Positions
            uint32_t nPos = 0;
            fileStream.read(reinterpret_cast<char*>(&nPos), sizeof(uint32_t));
            nodeAnimation.positions.reserve(nPos);
            for (uint32_t p = 0; p < nPos; p++) {
                io::ModelAnimation::PositionKeyframe posKeyframe;

                // Position
                fileStream.read(
                    reinterpret_cast<char*>(glm::value_ptr(posKeyframe.position)),
                    3 * sizeof(float)
                );

                // Time
                double time = 0.0;
                fileStream.read(reinterpret_cast<char*>(&time), sizeof(double));
                posKeyframe.time = time;

                nodeAnimation.positions.push_back(std::move(posKeyframe));
            }

            // Rotations
            uint32_t nRot = 0;
            fileStream.read(reinterpret_cast<char*>(&nRot), sizeof(uint32_t));
            nodeAnimation.rotations.reserve(nRot);
            for (uint32_t r = 0; r < nRot; r++) {
                io::ModelAnimation::RotationKeyframe rotKeyframe;

                // Rotation
                float rot[4];
                fileStream.read(reinterpret_cast<char*>(rot), 4 * sizeof(float));
                rotKeyframe.rotation = glm::quat(rot[0], rot[1], rot[2], rot[3]);

                // Time
                double time = 0.0;
                fileStream.read(reinterpret_cast<char*>(&time), sizeof(double));
                rotKeyframe.time = time;

                nodeAnimation.rotations.push_back(std::move(rotKeyframe));
            }

            // Scales
            uint32_t nScale = 0;
            fileStream.read(reinterpret_cast<char*>(&nScale), sizeof(uint32_t));
            nodeAnimation.scales.reserve(nScale);
            for (uint32_t s = 0; s < nScale; s++) {
                io::ModelAnimation::ScaleKeyframe scaleKeyframe;

                // Scale
                fileStream.read(
                    reinterpret_cast<char*>(glm::value_ptr(scaleKeyframe.scale)),
                    3 * sizeof(float)
                );

                // Time
                double time = 0.0;
                fileStream.read(reinterpret_cast<char*>(&time), sizeof(double));
                scaleKeyframe.time = time;

                nodeAnimation.scales.push_back(std::move(scaleKeyframe));
            }

            animation->nodeAnimations().push_back(nodeAnimation);
        }

        // IsTransparent
        uint8_t isT = 0;
        fileStream.read(reinterpret_cast<char*>(&isT), sizeof(uint8_t));
        const bool isTransparent = (isT == 1);

        // HasCalcTransparency
        uint8_t hasCalcT = 0;
        fileStream.read(reinterpret_cast<char*>(&hasCalcT), sizeof(uint8_t));
        const bool hasCalcTransparency = (hasCalcT == 1);

        // Create the ModelGeometry
        return std::make_unique<modelgeometry::ModelGeometry>(
            std::move(nodeArray),
            std::move(textureStorageArray),
            std::move(animation),
            isTransparent,
            hasCalcTransparency
        );
    }
    else {
        // IsTransparent
        uint8_t isT = 0;
        fileStream.read(reinterpret_cast<char*>(&isT), sizeof(uint8_t));
        const bool isTransparent = (isT == 1);

        // HasCalcTransparency
        uint8_t hasCalcT = 0;
        fileStream.read(reinterpret_cast<char*>(&hasCalcT), sizeof(uint8_t));
        const bool hasCalcTransparency = (hasCalcT == 1);

        // Create the ModelGeometry
        return std::make_unique<modelgeometry::ModelGeometry>(
            std::move(nodeArray),
            std::move(textureStorageArray),
            nullptr,
            isTransparent,
            hasCalcTransparency
        );
    }
}

bool ModelGeometry::saveToCacheFile(const std::filesystem::path& cachedFile) const {
    std::ofstream fileStream = std::ofstream(cachedFile, std::ofstream::binary);
    if (!fileStream.good()) {
        throw ModelCacheException(cachedFile, "Could not open file to save cache");
    }

    // Write which version of caching that is used
    fileStream.write(reinterpret_cast<const char*>(&CurrentCacheVersion), sizeof(int8_t));

    // First cache the textureStorage
    int32_t nTextureEntries = static_cast<int32_t>(_textureStorage.size());
    if (nTextureEntries == 0) {
        LINFO("No TextureEntries were loaded while saving cache");
    }
    fileStream.write(reinterpret_cast<const char*>(&nTextureEntries), sizeof(int32_t));

    for (int32_t te = 0; te < nTextureEntries; te++) {
        // Name
        int32_t nameSize = static_cast<int32_t>(_textureStorage[te].name.size());
        if (nameSize == 0) {
            throw ModelCacheException(
                cachedFile,
                "No texture name was found while saving cache"
            );
        }
        fileStream.write(reinterpret_cast<const char*>(&nameSize), sizeof(int32_t));
        fileStream.write(_textureStorage[te].name.data(), nameSize);

        // Texture
        // Dimensions
        std::array<int32_t, 3> dimensionStorage;
        dimensionStorage[0] = _textureStorage[te].texture->dimensions().x;
        dimensionStorage[1] = _textureStorage[te].texture->dimensions().y;
        dimensionStorage[2] = _textureStorage[te].texture->dimensions().z;

        fileStream.write(
            reinterpret_cast<const char*>(dimensionStorage.data()),
            3 * sizeof(int32_t)
        );

        // Format
        std::string format = formatToString(_textureStorage[te].texture->format());
        fileStream.write(format.data(), FormatStringSize * sizeof(char));

        // Internal format
        uint32_t internalFormat = static_cast<uint32_t>(
            _textureStorage[te].texture->internalFormat()
        );
        fileStream.write(
            reinterpret_cast<const char*>(&internalFormat),
            sizeof(uint32_t)
        );

        // Data type
        std::string dataType =
            dataTypeToString(_textureStorage[te].texture->dataType());
        fileStream.write(dataType.data(), FormatStringSize * sizeof(char));

        // Data
        std::vector<std::byte> pixels = _textureStorage[te].texture->pixelData();
        int32_t nPixels = static_cast<int32_t>(pixels.size());
        fileStream.write(reinterpret_cast<const char*>(&nPixels), sizeof(int32_t));
        fileStream.write(reinterpret_cast<const char*>(pixels.data()), nPixels);
    }

    // Write how many nodes are to be written
    int32_t nNodes = static_cast<int32_t>(_nodes.size());
    if (nNodes == 0) {
        throw ModelCacheException(cachedFile, "No nodes were found while saving cache");
    }
    fileStream.write(reinterpret_cast<const char*>(&nNodes), sizeof(int32_t));

    // Nodes
    for (const io::ModelNode& node : _nodes) {
        // Write how many meshes are to be written
        int32_t nMeshes = static_cast<int32_t>(node.meshes().size());
        fileStream.write(reinterpret_cast<const char*>(&nMeshes), sizeof(int32_t));

        // Meshes
        for (const io::ModelMesh& mesh : node.meshes()) {
            // HasVertexColors
            uint8_t col = mesh.hasVertexColors() ? 1 : 0;
            fileStream.write(reinterpret_cast<const char*>(&col), sizeof(uint8_t));

            // Vertices
            int32_t nVertices = static_cast<int32_t>(mesh.vertices().size());
            if (nVertices == 0) {
                throw ModelCacheException(
                    cachedFile,
                    "No vertices were found while saving cache"
                );
            }
            fileStream.write(reinterpret_cast<const char*>(&nVertices), sizeof(int32_t));

            for (int32_t v = 0; v < nVertices; v++) {
                fileStream.write(
                    reinterpret_cast<const char*>(&mesh.vertices()[v]),
                    sizeof(io::ModelMesh::Vertex)
                );
            }

            // Indices
            int32_t nIndices = static_cast<int32_t>(mesh.indices().size());
            if (nIndices == 0) {
                throw ModelCacheException(
                    cachedFile,
                    "No indices were found while saving cache"
                );
            }
            fileStream.write(reinterpret_cast<const char*>(&nIndices), sizeof(int32_t));

            for (int32_t i = 0; i < nIndices; i++) {
                uint32_t index = static_cast<uint32_t>(mesh.indices()[i]);
                fileStream.write(reinterpret_cast<const char*>(&index), sizeof(uint32_t));
            }

            // IsInvisible
            uint8_t inv = mesh.isInvisible() ? 1 : 0;
            fileStream.write(reinterpret_cast<const char*>(&inv), sizeof(uint8_t));

            // Textures
            int32_t nTextures = static_cast<int32_t>(mesh.textures().size());
            if (nTextures == 0 && !mesh.isInvisible()) {
                throw ModelCacheException(
                    cachedFile,
                    "No materials were found while saving cache"
                );
            }
            fileStream.write(reinterpret_cast<const char*>(&nTextures), sizeof(int32_t));

            for (int32_t t = 0; t < nTextures; t++) {
                // Don't save the debug texture to the cache. Write matching skip marker
                if (mesh.textures()[t].useForcedColor) {
                    fileStream.write(
                        reinterpret_cast<const char*>(&ShouldSkipMarker),
                        sizeof(int8_t)
                    );
                    continue;
                }
                else {
                    fileStream.write(
                        reinterpret_cast<const char*>(&NoSkipMarker),
                        sizeof(int8_t)
                    );
                }

                // Type
                fileStream.write(
                    reinterpret_cast<const char*>(&mesh.textures()[t].type),
                    sizeof(uint8_t)
                );

                // HasTexture
                uint8_t h = (mesh.textures()[t].hasTexture) ? 1 : 0;
                fileStream.write(reinterpret_cast<const char*>(&h), sizeof(uint8_t));

                // Color
                fileStream.write(
                    reinterpret_cast<const char*>(&mesh.textures()[t].color.r),
                     4 * sizeof(float)
                );

                // IsTransparent
                uint8_t isT = (mesh.textures()[t].isTransparent) ? 1 : 0;
                fileStream.write(reinterpret_cast<const char*>(&isT), sizeof(uint8_t));

                // Texture
                if (mesh.textures()[t].hasTexture) {
                    // Search the textureStorage to find the texture entry
                    bool wasFound = false;
                    for (size_t te = 0; te < _textureStorage.size(); te++) {
                        if (_textureStorage[te].name ==
                            mesh.textures()[t].texture->name())
                        {
                            uint32_t index = static_cast<uint32_t>(te);
                            fileStream.write(
                                reinterpret_cast<const char*>(&index),
                                sizeof(uint32_t)
                            );
                            wasFound = true;
                            break;
                        }
                    }

                    if (!wasFound) {
                        throw ModelCacheException(
                            cachedFile,
                            "Could not find texture in textureStorage while saving cache"
                        );
                    }
                }
            }
        }

        // Transform
        glm::mat4 transform = node.transform();
        fileStream.write(reinterpret_cast<const char*>(&transform), 16 * sizeof(GLfloat));

        // AnimationTransform
        glm::mat4 animationTransform = node.animationTransform();
        fileStream.write(
            reinterpret_cast<const char*>(&animationTransform),
            16 * sizeof(GLfloat)
        );

        // Parent
        int32_t parent = node.parent();
        fileStream.write(reinterpret_cast<const char*>(&parent), sizeof(int32_t));

        // Write how many children are to be written
        int32_t nChildren = static_cast<int32_t>(node.children().size());

        fileStream.write(reinterpret_cast<const char*>(&nChildren), sizeof(int32_t));

        // Children
        for (int32_t child : node.children()) {
            fileStream.write(reinterpret_cast<const char*>(&child), sizeof(int32_t));
        }

        // HasAnimation
        uint8_t a = node.hasAnimation() ? 1 : 0;
        fileStream.write(reinterpret_cast<const char*>(&a), sizeof(uint8_t));

        // Name
        std::string name = node.name();
        int32_t nChars = static_cast<int32_t>(name.size());
        fileStream.write(reinterpret_cast<const char*>(&nChars), sizeof(int32_t));
        fileStream.write(
            reinterpret_cast<const char*>(name.data()),
            nChars * sizeof(char)
        );
    }

    // Animation
    uint8_t a = _animation ? 1 : 0;
    fileStream.write(reinterpret_cast<const char*>(&a), sizeof(uint8_t));

    if (_animation) {
        // Name
        if (_animation->name().size() >= std::numeric_limits<uint8_t>::max()) {
            LWARNING(std::format(
                "A maximum animaion name length of {} is supported",
                std::numeric_limits<uint8_t>::max()
            ));
        }
        uint8_t nameSize = static_cast<uint8_t>(_animation->name().size());
        if (nameSize == 0) {
            LINFO("No name was found for animation while saving cache");
        }
        fileStream.write(reinterpret_cast<const char*>(&nameSize), sizeof(uint8_t));
        fileStream.write(
            reinterpret_cast<const char*>(_animation->name().data()),
            nameSize * sizeof(char)
        );

        // Duration
        double duration = _animation->duration();
        fileStream.write(reinterpret_cast<const char*>(&duration), sizeof(double));

        // Write how many NodeAnimations are to be written
        int32_t nAnimations = static_cast<int32_t>(_animation->nodeAnimations().size());
        if (nAnimations == 0) {
            throw ModelCacheException(
                cachedFile,
                "No node animations were found while saving cache"
            );
        }
        fileStream.write(reinterpret_cast<const char*>(&nAnimations), sizeof(int32_t));

        // NodeAnimations
        for (const io::ModelAnimation::NodeAnimation& nodeAnimation :
            _animation->nodeAnimations())
        {
            // Node index
            int32_t nodeIndex = static_cast<int32_t>(nodeAnimation.node);
            fileStream.write(reinterpret_cast<const char*>(&nodeIndex), sizeof(int32_t));

            // Positions
            if (nodeAnimation.positions.size() >= std::numeric_limits<uint32_t>::max()) {
                LWARNING(std::format(
                    "A maximum number of '{}' position keyframes are supported",
                    std::numeric_limits<uint32_t>::max())
                );
            }
            uint32_t nPos = static_cast<uint32_t>(nodeAnimation.positions.size());
            fileStream.write(reinterpret_cast<const char*>(&nPos), sizeof(uint32_t));
            for (const io::ModelAnimation::PositionKeyframe& posKeyframe :
                nodeAnimation.positions)
            {
                // Position
                fileStream.write(
                    reinterpret_cast<const char*>(glm::value_ptr(posKeyframe.position)),
                    3 * sizeof(float)
                );

                // Time
                fileStream.write(
                    reinterpret_cast<const char*>(&posKeyframe.time),
                    sizeof(double)
                );
            }

            // Rotations
            uint32_t nRot = static_cast<uint32_t>(nodeAnimation.rotations.size());
            if (nodeAnimation.rotations.size() >= std::numeric_limits<uint32_t>::max()) {
                LWARNING(std::format(
                    "A maximum number of '{}' rotation keyframes are supported",
                    std::numeric_limits<uint32_t>::max())
                );
            }
            fileStream.write(reinterpret_cast<const char*>(&nRot), sizeof(uint32_t));
            for (const io::ModelAnimation::RotationKeyframe& rotKeyframe :
                nodeAnimation.rotations)
            {
                // Rotation
                fileStream.write(
                    reinterpret_cast<const char*>(&rotKeyframe.rotation.w),
                    sizeof(float)
                );
                fileStream.write(
                    reinterpret_cast<const char*>(&rotKeyframe.rotation.x),
                    3 * sizeof(float)
                );

                // Time
                fileStream.write(
                    reinterpret_cast<const char*>(&rotKeyframe.time),
                    sizeof(double)
                );
            }

            // Scales
            uint32_t nScale = static_cast<uint32_t>(nodeAnimation.scales.size());
            if (nodeAnimation.scales.size() >= std::numeric_limits<uint32_t>::max()) {
                LWARNING(std::format(
                    "A maximum number of '{}' scale keyframes are supported",
                    std::numeric_limits<uint32_t>::max())
                );
            }
            fileStream.write(reinterpret_cast<const char*>(&nScale), sizeof(uint32_t));
            for (const io::ModelAnimation::ScaleKeyframe& scaleKeyframe :
                nodeAnimation.scales)
            {
                // Scale
                fileStream.write(
                    reinterpret_cast<const char*>(glm::value_ptr(scaleKeyframe.scale)),
                    3 * sizeof(float)
                );

                // Time
                fileStream.write(
                    reinterpret_cast<const char*>(&scaleKeyframe.time),
                    sizeof(double)
                );
            }
        }
    }

    // IsTransparent
    uint8_t isT = _isTransparent ? 1 : 0;
    fileStream.write(reinterpret_cast<const char*>(&isT), sizeof(uint8_t));

    // HasCalcTransparency
    uint8_t hasCalcT = _hasCalcTransparency ? 1 : 0;
    fileStream.write(reinterpret_cast<const char*>(&hasCalcT), sizeof(uint8_t));

    return fileStream.good();
}

double ModelGeometry::boundingRadius() const {
    return _boundingRadius;
}

void ModelGeometry::calculateBoundingRadius() {
    ZoneScoped;

    if (_nodes.empty()) {
        LERROR("Cannot calculate bounding radius for empty geometry");
        return;
    }

    const glm::mat4 parentTransform = glm::mat4(1.f);
    float maximumDistanceSquared = 0.f;
    calculateBoundingRadiusRecursive(
        _nodes,
        _nodes.data(),
        parentTransform,
        maximumDistanceSquared
    );

    _boundingRadius = std::sqrt(maximumDistanceSquared);
}

bool ModelGeometry::hasAnimation() const {
    return _animation != nullptr;
}

double ModelGeometry::animationDuration() const {
    if (_animation == nullptr) {
        LERROR("Model does not have any animation");
        return -1.0;
    }

    return _animation->duration();
}

void ModelGeometry::calculateTransparency() {
    ZoneScoped;

    if (_hasCalcTransparency) {
        return;
    }

    bool isTransparent = false;
    for (const io::ModelNode& n : _nodes) {
        if (isTransparent) {
            break;
        }

        for (const io::ModelMesh& m : n.meshes()) {
            if (m.isTransparent()) {
                isTransparent = true;
                break;
            }
        }
    }

    _isTransparent = isTransparent;
    _hasCalcTransparency = true;
}

void ModelGeometry::recalculateTransparency() {
    _hasCalcTransparency = false;
    return calculateTransparency();
}

bool ModelGeometry::isTransparent() const {
    if (!_hasCalcTransparency) {
        LWARNING(
            "Transparency has not been calculated for this model, value may be invalid"
        );
    }
    return _isTransparent;
}

std::vector<io::ModelNode>& ModelGeometry::nodes() {
    return _nodes;
}

const std::vector<io::ModelNode>& ModelGeometry::nodes() const {
    return _nodes;
}

std::vector<ModelGeometry::TextureEntry>& ModelGeometry::textureStorage() {
    return _textureStorage;
}

const std::vector<ModelGeometry::TextureEntry>& ModelGeometry::textureStorage() const {
    return _textureStorage;
}

void ModelGeometry::render(opengl::ProgramObject& program, bool isFullyTexturedModel,
                           bool isProjection) const
{
    if (_nodes.empty()) {
        LERROR("Cannot render empty geometry");
        return;
    }

    const glm::mat4 parentTransform = glm::mat4(1.f);
    renderRecursive(
        _nodes,
        _nodes.data(),
        program,
        parentTransform,
        isFullyTexturedModel,
        isProjection,
        _replaceWithCustomTransforms
    );
}

void ModelGeometry::update(double now) {
    if (_animation == nullptr) {
        LERROR("Cannot update empty animation");
        return;
    }

    _animation->animate(_nodes, now, _animationEnabled);
}

void ModelGeometry::setTimeScale(float timeScale) {
    if (_animation == nullptr) {
        LERROR("Cannot set time scale of empty animation");
        return;
    }

    _animation->setTimeScale(timeScale);
}

void ModelGeometry::enableAnimation(bool value) {
    _animationEnabled = value;

    if (!value) {
        _animation->reset(_nodes);
    }
}

void ModelGeometry::updateCustomNodeTransform(std::string_view nodeName,
                                              const glm::dmat4& customTransform)
{
    for (io::ModelNode& node : _nodes) {
        if (node.name() == nodeName) {
            node.updateCustomTransform(customTransform);
            return;
        }
    }

    LERROR(std::format(
        "Could not find node with name '{}' to update custom transform", nodeName
    ));
}

void ModelGeometry::setReplaceWithCustomTransforms(bool replaceWithCustomTransforms) {
    _replaceWithCustomTransforms = replaceWithCustomTransforms;
}

void ModelGeometry::initialize() {
    ZoneScoped;

    for (io::ModelNode& node : _nodes) {
        for (io::ModelMesh& mesh : node.meshes()) {
            mesh.initialize();
        }
    }

    calculateBoundingRadius();
    calculateTransparency();
}

void ModelGeometry::deinitialize() {
    for (io::ModelNode& node : _nodes) {
        for (io::ModelMesh& mesh : node.meshes()) {
            mesh.deinitialize();
        }
    }
}

} // namespace ghoul::modelgeometry
