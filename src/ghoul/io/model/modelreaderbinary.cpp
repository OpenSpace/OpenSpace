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
 * permit persons to whom the Software is furnished to do so, subject to the llowing   *
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

#include <ghoul/io/model/modelreaderbinary.h>

#include <ghoul/io/model/modelanimation.h>
#include <ghoul/io/model/modelgeometry.h>
#include <ghoul/io/model/modelmesh.h>
#include <ghoul/io/model/modelnode.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/texture.h>
#include <cstdint>
#include <fstream>
#include <string_view>
#include <cstddef>

namespace {
    using namespace ghoul;

    constexpr std::string_view _loggerCat = "ModelReaderBinary";
    constexpr int8_t CurrentModelVersion = 12;
    constexpr int FormatStringSize = 4;
    constexpr int8_t ShouldSkipMarker = -1;

    // Backward compatible versions
    constexpr int8_t AnimationUpdateVersion = 7;
    constexpr int8_t OpacityUpdateVersion = 8;
    constexpr int8_t VertexColorUpdateVersion = 9;
    constexpr int8_t SkipMarkerUpdateVersion = 10;
    constexpr int8_t ImmutableTexturesVersion = 11; // No file format difference from 10
    constexpr int8_t NodeNameUpdateVersion = 12;

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
} // namespace

namespace ghoul::io {

std::unique_ptr<modelgeometry::ModelGeometry> ModelReaderBinary::loadModel(
                                                    const std::filesystem::path& filename,
                                                                bool forceRenderInvisible,
                                                        bool notifyInvisibleDropped) const
{
    ghoul_assert(!filename.empty(), "Filename must not be empty");

    std::ifstream fileStream = std::ifstream(filename, std::ifstream::binary);
    if (!fileStream.good()) {
        throw ModelLoadException(filename, "Could not open binary model file", this);
    }

    // Check the file format version
    int8_t version = 0;
    fileStream.read(reinterpret_cast<char*>(&version), sizeof(int8_t));
    if (version != CurrentModelVersion &&
        // Backward compatible versions that are ok
        version != AnimationUpdateVersion && version != OpacityUpdateVersion &&
        version != VertexColorUpdateVersion && version != SkipMarkerUpdateVersion &&
        version != ImmutableTexturesVersion && version != NodeNameUpdateVersion)
    {
        throw ModelLoadException(
            filename,
            std::format("OS-model format {} is not supported", version),
            this
        );
    }

    // First read the textureEntries
    int32_t nTextureEntries = 0;
    fileStream.read(reinterpret_cast<char*>(&nTextureEntries), sizeof(int32_t));
    if (nTextureEntries == 0) {
        LINFO("No TextureEntries were found while loading binary model");
    }
    else if (nTextureEntries < 0) {
        std::string message = std::format(
            "Model cannot have negative number of texture entries while loading binary "
            "model: {}", nTextureEntries
        );
        throw ModelLoadException(filename, message, this);
    }
    std::vector<modelgeometry::ModelGeometry::TextureEntry> textureStorageArray;
    textureStorageArray.reserve(nTextureEntries);

    for (int32_t te = 0; te < nTextureEntries; te++) {
        modelgeometry::ModelGeometry::TextureEntry textureEntry;

        // Name
        int32_t nameSize = 0;
        fileStream.read(reinterpret_cast<char*>(&nameSize), sizeof(int32_t));
        if (nameSize <= 0) {
            throw ModelLoadException(
                filename,
                "No texture name was found while loading binary model",
                this
            );
        }
        textureEntry.name.resize(nameSize);
        fileStream.read(textureEntry.name.data(), nameSize * sizeof(char));

        // Texture
        // Dimensions
        std::array<int32_t, 3> dimensionStorage;
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
            throw ModelLoadException(
                filename,
                "No texture size was found while loading binary model",
                this
            );
        }
        std::vector<std::byte> data = std::vector<std::byte>(textureSize);
        fileStream.read(reinterpret_cast<char*>(data.data()), textureSize);

        textureEntry.texture = std::make_unique<opengl::Texture>(
            opengl::Texture::FormatInit {
                .dimensions = dimensions,
                .type = GL_TEXTURE_2D,
                .format = format,
                .dataType = dataType,
                .internalFormat = internalFormat
            },
            opengl::Texture::SamplerInit{
                .filter = opengl::Texture::FilterMode::AnisotropicMipMap
            },
            data.data()
        );
        textureStorageArray.push_back(std::move(textureEntry));
    }

    // Read how many nodes to read
    int32_t nNodes = 0;
    fileStream.read(reinterpret_cast<char*>(&nNodes), sizeof(int32_t));
    if (nNodes <= 0) {
        throw ModelLoadException(
            filename,
            "No nodes were found while loading binary model",
            this
        );
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
                "Model cannot have negative number of meshes while loading binary "
                "model: {}", nMeshes
            );
            throw ModelLoadException(filename, message, this);
        }

        // Meshes
        std::vector<io::ModelMesh> meshArray;
        meshArray.reserve(nMeshes);
        for (int32_t m = 0; m < nMeshes; m++) {
            bool hasVertexColors = false;
            if (version >= VertexColorUpdateVersion) {
                // HasVertexColors
                uint8_t col = 0;
                fileStream.read(reinterpret_cast<char*>(&col), sizeof(uint8_t));
                hasVertexColors = (col == 1);
            }

            // Vertices
            int32_t nVertices = 0;
            fileStream.read(reinterpret_cast<char*>(&nVertices), sizeof(int32_t));
            if (nVertices <= 0) {
                throw ModelLoadException(
                    filename,
                    "No vertices were found while loading binary model",
                    this
                );
            }
            std::vector<io::ModelMesh::Vertex> vertexArray;
            vertexArray.reserve(nVertices);

            for (int32_t v = 0; v < nVertices; v++) {
                io::ModelMesh::Vertex vertex;

                // Set the vertex size based on version
                auto vertexSize = sizeof(io::ModelMesh::Vertex);
                if (version < VertexColorUpdateVersion) {
                    // Three extra floats (rgb color) were added to the Vertex struct
                    // since version VertexColorUpdateVersion, previous versions does not
                    // have them
                    vertexSize -= sizeof(GLfloat[3]);
                }

                fileStream.read(reinterpret_cast<char*>(&vertex), vertexSize);
                vertexArray.push_back(std::move(vertex));
            }

            // Indices
            int32_t nIndices = 0;
            fileStream.read(reinterpret_cast<char*>(&nIndices), sizeof(int32_t));
            if (nIndices <= 0) {
                throw ModelLoadException(
                    filename,
                    "No indices were found while loading binary model",
                    this
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
            if (nTextures == 0 && !isInvisible) {
                throw ModelLoadException(
                    filename,
                    "No textures were found while loading binary model",
                    this
                );
            }
            std::vector<io::ModelMesh::Texture> textureArray;
            textureArray.reserve(nTextures);

            for (int32_t t = 0; t < nTextures; t++) {
                io::ModelMesh::Texture texture;

                if (version >= SkipMarkerUpdateVersion) {
                    // Skip marker
                    int8_t skip = 0;
                    fileStream.read(reinterpret_cast<char*>(&skip), sizeof(int8_t));
                    if (skip == ShouldSkipMarker) {
                        continue;
                    }
                }

                // Type
                fileStream.read(reinterpret_cast<char*>(&texture.type), sizeof(uint8_t));

                // HasTexture
                uint8_t h = 0;
                fileStream.read(reinterpret_cast<char*>(&h), sizeof(uint8_t));
                texture.hasTexture = (h == 1);

                // Color
                fileStream.read(
                    reinterpret_cast<char*>(glm::value_ptr(texture.color)),
                    3 * sizeof(float)
                );
                if (version >= OpacityUpdateVersion) {
                    fileStream.read(
                        reinterpret_cast<char*>(&texture.color.a),
                        sizeof(float)
                    );

                    // IsTransparent
                    uint8_t isT = 0;
                    fileStream.read(reinterpret_cast<char*>(&isT), sizeof(uint8_t));
                    texture.isTransparent = (isT == 1);
                }

                // Texture
                if (texture.hasTexture) {
                    // Read which index in the textureStorageArray that this texture
                    // should point to
                    uint32_t index = 0;
                    fileStream.read(reinterpret_cast<char*>(&index), sizeof(uint32_t));

                    if (index >= textureStorageArray.size()) {
                        std::string message =
                            "Texture index is outside of textureStorage during loading "
                            "of binary model";
                        throw ModelLoadException(filename, message, this);
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
                    LINFO(
                        "An invisible mesh has been dropped while loading binary model"
                    );
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
        GLfloat rawTransform[16];
        fileStream.read(reinterpret_cast<char*>(rawTransform), 16 * sizeof(GLfloat));
        glm::mat4 transform = glm::make_mat4(rawTransform);

        // AnimationTransform
        GLfloat rawAnimTransform[16];
        fileStream.read(reinterpret_cast<char*>(&rawAnimTransform), 16 * sizeof(GLfloat));
        const glm::mat4 animationTransform = glm::make_mat4(rawAnimTransform);

        // Parent
        int32_t parent = 0;
        fileStream.read(reinterpret_cast<char*>(&parent), sizeof(int32_t));

        // Read how many children to read
        int32_t nChildren = 0;
        fileStream.read(reinterpret_cast<char*>(&nChildren), sizeof(int32_t));
        if (nChildren < 0) {
            std::string message = std::format(
                "Binary model cannot have negative number of children: {}",
                nChildren
            );
            throw ModelLoadException(filename, message, this);
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
        std::string name;
        if (version >= NodeNameUpdateVersion) {
            int32_t nChars = 0;
            fileStream.read(reinterpret_cast<char*>(&nChars), sizeof(int32_t));
            if (nChars < 0) {
                std::string message = std::format(
                    "Model node name cannot have negative number of characters: {}",
                    nChars
                );
                throw ModelLoadException(filename, message, this);
            }
            name.resize(nChars);
            fileStream.read(reinterpret_cast<char*>(name.data()), nChars * sizeof(char));
        }

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
            throw ModelLoadException(
                filename,
                "No node animations were found while loading binary model",
                this
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
                std::array<float, 4> rot = {};
                fileStream.read(reinterpret_cast<char*>(rot.data()), 4 * sizeof(float));
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

        bool isTransparent = false;
        bool hasCalcTransparency = false;
        if (version >= OpacityUpdateVersion) {
            // IsTransparent
            uint8_t isT = 0;
            fileStream.read(reinterpret_cast<char*>(&isT), sizeof(uint8_t));
            isTransparent = (isT == 1);

            // HasCalcTransparency
            uint8_t hasCalcT = 0;
            fileStream.read(reinterpret_cast<char*>(&hasCalcT), sizeof(uint8_t));
            hasCalcTransparency = (hasCalcT == 1);
        }

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
        bool isTransparent = false;
        bool hasCalcTransparency = false;
        if (version >= OpacityUpdateVersion) {
            // IsTransparent
            uint8_t isT = 0;
            fileStream.read(reinterpret_cast<char*>(&isT), sizeof(uint8_t));
            isTransparent = (isT == 1);

            // HasCalcTransparency
            uint8_t hasCalcT = 0;
            fileStream.read(reinterpret_cast<char*>(&hasCalcT), sizeof(uint8_t));
            hasCalcTransparency = (hasCalcT == 1);
        }

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

bool ModelReaderBinary::needsCache() const {
    return false;
}

std::vector<std::string> ModelReaderBinary::supportedExtensions() const {
    return {
        "osmodel" // Custom OpenSpace model format
    };
}

} // namespace ghoul::io
