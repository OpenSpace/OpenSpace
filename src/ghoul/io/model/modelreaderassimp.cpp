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

#include <ghoul/io/model/modelreaderassimp.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/glm.h>
#include <ghoul/io/model/modelanimation.h>
#include <ghoul/io/model/modelgeometry.h>
#include <ghoul/io/model/modelmesh.h>
#include <ghoul/io/model/modelnode.h>
#include <ghoul/io/model/modelreaderbase.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/defer.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <assimp/anim.h>
#include <assimp/color4.h>
#include <assimp/Importer.hpp>
#include <assimp/material.h>
#include <assimp/mesh.h>
#include <assimp/postprocess.h>
#include <assimp/scene.h>
#include <assimp/texture.h>
#include <assimp/types.h>
#include <algorithm>
#include <cstdlib>
#include <limits>
#include <string_view>
#include <utility>

namespace {
    using namespace ghoul;
    using namespace ghoul::io;

    constexpr std::string_view _loggerCat = "ModelReaderAssimp";
    constexpr int TreeTabLength = 2;

    bool isTextureTransparent(const ModelMesh::Texture& texture) {
        const int nChannels = texture.texture->numberOfChannels();

        if (nChannels < 4) {
            return false;
        }

        texture.texture->downloadTexture();
        defer { texture.texture->clearDownloadedTexture(); };

        // Check if there is at least one pixel that is somewhat transparent
        for (unsigned int j = 0; j < texture.texture->dimensions().x; j++) {
            for (unsigned int k = 0; k < texture.texture->dimensions().y; k++) {
                const float alpha = texture.texture->texelAsFloat({ j, k, 0 }).a;
                if (alpha < 1.f) {
                    return true;
                }
            }
        }
        return false;
    }

    bool loadMaterialTextures(const aiScene& scene, const aiMaterial& material,
                              const aiTextureType& type,
                              const ModelMesh::TextureType& enumType,
                              std::vector<ModelMesh::Texture>& textureArray,
                  std::vector<modelgeometry::ModelGeometry::TextureEntry>& textureStorage,
                                                    std::filesystem::path& modelDirectory)
    {
        for (unsigned int i = 0; i < material.GetTextureCount(type); i++) {
            ModelMesh::Texture meshTexture = {
                .type = enumType
            };
            aiString path;
            material.GetTexture(type, i, &path);

            // Check for duplicates
            bool shouldSkip = false;
            for (const ModelMesh::Texture& texture : textureArray) {
                if (texture.hasTexture &&
                    texture.texture->name() == std::string_view(path.C_Str()))
                {
                    // Texture has already been loaded for this mesh, continue to next one
                    shouldSkip = true;
                    break;
                }
            }

            // Texture already loaded, do not load it again
            if (shouldSkip) {
                continue;
            }

            // Check if texture has already been loaded by other meshes
            for (const modelgeometry::ModelGeometry::TextureEntry& tex : textureStorage) {
                if (tex.texture->name() == std::string_view(path.C_Str())) {
                    // Texture has already been loaded. Point to that texture instead
                    meshTexture.texture = tex.texture.get();
                    meshTexture.hasTexture = true;
                    textureArray.push_back(meshTexture);
                    shouldSkip = true;
                    break;
                }
            }

            // Texture already loaded, point to it and do not load it again
            if (shouldSkip) {
                continue;
            }

            // Load texture
            const aiTexture* texture = scene.GetEmbeddedTexture(path.C_Str());
            modelgeometry::ModelGeometry::TextureEntry textureEntry;
            textureEntry.name = path.C_Str();
            // Check if the texture is an embedded texture or a local texture
            if (texture) {
                // Embedded texture
                if (texture->mHeight == 0) {
                    // Load compressed embedded texture
                    try {
                        textureEntry.texture = texture::loadTexture(
                            static_cast<void*>(texture->pcData),
                            texture->mWidth,
                            2,
                            opengl::Texture::SamplerInit{
                                .filter = opengl::Texture::FilterMode::AnisotropicMipMap
                            },
                            texture->achFormatHint
                        );
                        textureEntry.texture->setName(path.C_Str());
                        meshTexture.texture = textureEntry.texture.get();
                    }
                    catch (const texture::InvalidLoadException& e) {
                        LWARNING(std::format(
                            "Could not load unsupported texture from '{}' with size "
                            "'{}': Replacing with flashy color", e._memory, e._size
                        ));
                        ModelMesh::generateDebugTexture(meshTexture);
                        textureArray.push_back(std::move(meshTexture));
                        return false;
                    }
                    catch (const texture::TextureLoadException& e) {
                        LWARNING(std::format(
                            "Failed to load texture from '{}' with error '{}': "
                            "Replacing with flashy color", e.filename, e.message
                        ));
                        ModelMesh::generateDebugTexture(meshTexture);
                        textureArray.push_back(std::move(meshTexture));
                        return false;
                    }
                }
                else {
                    // Load uncompressed embedded texture
                    LWARNING("Uncompressed embedded texture detected: Not supported! "
                        "Replacing with flashy color"
                    );
                    ModelMesh::generateDebugTexture(meshTexture);
                    textureArray.push_back(std::move(meshTexture));
                    return false;
                }
            }
            else {
                // Local texture
                try {
                    const std::string pathString(path.C_Str());
                    const std::string absolutePath = std::format(
                        "{}/{}", modelDirectory, pathString
                    );

                    textureEntry.texture = texture::loadTexture(
                        absPath(absolutePath),
                        2,
                        opengl::Texture::SamplerInit{
                            .filter = opengl::Texture::FilterMode::AnisotropicMipMap
                        }
                    );
                    textureEntry.texture->setName(path.C_Str());
                    meshTexture.texture = textureEntry.texture.get();
                }
                catch (const texture::MissingReaderException& e) {
                    LWARNING(std::format(
                        "Could not load unsupported texture from '{}' with extension "
                        "'{}': Replacing with flashy color", e.file, e.fileExtension
                    ));
                    ModelMesh::generateDebugTexture(meshTexture);
                    textureArray.push_back(std::move(meshTexture));
                    return false;
                }
                catch (const texture::TextureLoadException& e) {
                    LWARNING(std::format(
                        "Failed to load texture from '{}' with error '{}': Replacing "
                        "with flashy color", e.filename, e.message
                    ));
                    ModelMesh::generateDebugTexture(meshTexture);
                    textureArray.push_back(std::move(meshTexture));
                    return false;
                }
            }

            meshTexture.texture->downloadTexture();

            // Check if the entire texture is transparent
            bool isOpaque = false;
            for (unsigned int j = 0; j < meshTexture.texture->dimensions().x; j++) {
                if (isOpaque) {
                    break;
                }

                for (unsigned int k = 0; k < meshTexture.texture->dimensions().y; k++) {
                    const float alpha = meshTexture.texture->texelAsFloat({ j, k, 0 }).a;
                    if (alpha > 0.f) {
                        isOpaque = true;
                        break;
                    }
                }
            }

            meshTexture.texture->clearDownloadedTexture();

            // If entire texture is transparent, do not add it
            if (!isOpaque) {
                continue;
            }

            // Check if the diffuse texture is somewhat transparent
            if (enumType == ModelMesh::TextureType::TextureDiffuse) {
                meshTexture.isTransparent = isTextureTransparent(meshTexture);
            }

            // Add new Texture to the textureStorage and point to it in the texture array
            meshTexture.hasTexture = true;
            textureArray.push_back(std::move(meshTexture));
            textureStorage.push_back(std::move(textureEntry));
        }
        return true;
    }


    ModelMesh processMesh(const aiMesh& mesh, const aiScene& scene,
                  std::vector<modelgeometry::ModelGeometry::TextureEntry>& textureStorage,
                                                    std::filesystem::path& modelDirectory,
                                                                bool forceRenderInvisible,
                                                              bool notifyInvisibleDropped)
    {
        std::vector<ModelMesh::Vertex> vertexArray;
        std::vector<unsigned int> indexArray;
        std::vector<ModelMesh::Texture> textureArray;
        bool hasVertexColors = false;

        // Vertices
        vertexArray.reserve(mesh.mNumVertices);
        for (unsigned int i = 0; i < mesh.mNumVertices; i++) {
            ModelMesh::Vertex vertex;

            // Position
            vertex.position[0] = mesh.mVertices[i].x;
            vertex.position[1] = mesh.mVertices[i].y;
            vertex.position[2] = mesh.mVertices[i].z;

            // Normal
            if (mesh.HasNormals()) {
                vertex.normal[0] = mesh.mNormals[i].x;
                vertex.normal[1] = mesh.mNormals[i].y;
                vertex.normal[2] = mesh.mNormals[i].z;
            }
            else {
                vertex.normal[0] = 0.f;
                vertex.normal[1] = 0.f;
                vertex.normal[2] = 0.f;
            }

            // Texture Coordinates
            if (mesh.HasTextureCoords(0)) {
                // Each vertex can have at most 8 different texture coordinates. We are
                // using only the first one provided
                vertex.tex[0] = mesh.mTextureCoords[0][i].x;
                vertex.tex[1] = mesh.mTextureCoords[0][i].y;
            }
            else {
                vertex.tex[0] = 0.f;
                vertex.tex[1] = 0.f;
            }

            // Tangent, used for normal mapping
            // Bitangent is calculated in shader
            if (mesh.HasTangentsAndBitangents()) {
                vertex.tangent[0] = mesh.mTangents[i].x;
                vertex.tangent[1] = mesh.mTangents[i].y;
                vertex.tangent[2] = mesh.mTangents[i].z;
            }
            else {
                vertex.tangent[0] = 0.f;
                vertex.tangent[1] = 0.f;
                vertex.tangent[2] = 0.f;
            }

            // Color, only check the first color set
            // @TODO: (malej 2024-07-08) Handle the other colors in the set up to
            // AI_MAX_NUMBER_OF_COLOR_SETS.
            //
            // @TODO: (malej 2024-07-08) Handle alpha channel as well
            if (mesh.HasVertexColors(0)) {
                aiColor4D* color = &mesh.mColors[0][i];
                vertex.color[0] = color->r;
                vertex.color[1] = color->g;
                vertex.color[2] = color->b;
                hasVertexColors = true;
            }

            vertexArray.push_back(std::move(vertex));
        }

        // Indices
        // Reserve space, every face has usually three indices
        const unsigned int nIndices = mesh.mNumFaces * 3u;
        indexArray.reserve(nIndices);
        for (unsigned int i = 0; i < mesh.mNumFaces; i++) {
            const aiFace face = mesh.mFaces[i];

            for (unsigned int j = 0; j < face.mNumIndices; j++) {
                indexArray.push_back(face.mIndices[j]);
            }
        }

        // Process materials and textures
        aiMaterial* material = scene.mMaterials[mesh.mMaterialIndex];

        // If there is no material and no color then do not add the mesh
        if (!material && !hasVertexColors) {
            return ModelMesh(
                std::move(vertexArray),
                std::move(indexArray),
                std::move(textureArray)
            );
        }

        // We assume a convention for sampler names in the shaders. Each diffuse texture
        // should be named as 'texture_diffuse', at the moment only one texture per type
        // is supported. Same applies to other textures as the following list summarizes:
        //  - diffuse: texture_diffuse or color_diffuse if embedded simple material
        //  - specular: texture_specular or color_specular if embedded simple material
        //  - normal: texture_normal

        // Opacity
        float opacity = 0.f;
        const aiReturn result = material->Get(AI_MATKEY_OPACITY, opacity);
        const bool hasOpacity = result == AI_SUCCESS && opacity < 1.f;
        if (hasOpacity && opacity < std::numeric_limits<float>::epsilon()) {
            // If the material is transparent then do not add it
            return ModelMesh(
                std::move(vertexArray),
                std::move(indexArray),
                std::move(textureArray)
            );
        }

        // Diffuse
        if (material->GetTextureCount(aiTextureType_DIFFUSE) > 0) {
            const bool success = loadMaterialTextures(
                scene,
                *material,
                aiTextureType_DIFFUSE,
                ModelMesh::TextureType::TextureDiffuse,
                textureArray,
                textureStorage,
                modelDirectory
            );

            if (!success) {
                return ModelMesh(
                    std::move(vertexArray),
                    std::move(indexArray),
                    std::move(textureArray)
                );
            }
            if (hasOpacity) {
                LWARNING("Unsupported opacity + diffuse texture found");
            }
        }
        else {
            // Load embedded simple material instead of textures
            aiColor4D color4 = aiColor4D(0.f, 0.f, 0.f, 0.f);
            const aiReturn hasColor4 = material->Get(AI_MATKEY_COLOR_DIFFUSE, color4);
            if (hasColor4 == AI_SUCCESS) {
                // Only add the color if it is not transparent
                if (color4.a != 0.f) {
                    ModelMesh::Texture texture = {
                        .type = ModelMesh::TextureType::ColorDiffuse,
                        .hasTexture = false,
                        .color = glm::vec4(color4.r, color4.g, color4.b, color4.a),
                        .isTransparent = texture.color.a < 1.f
                    };
                    textureArray.push_back(std::move(texture));
                }
            }
            else {
                aiColor3D color3 = aiColor3D(0.f, 0.f, 0.f);
                const aiReturn hasColor3 = material->Get(AI_MATKEY_COLOR_DIFFUSE, color3);
                if (hasColor3 == AI_SUCCESS) {
                    ModelMesh::Texture texture = {
                        .type = ModelMesh::TextureType::ColorDiffuse,
                        .hasTexture = false,
                        .color = glm::vec4(color3.r, color3.g, color3.b, 1.f)
                    };
                    textureArray.push_back(std::move(texture));
                }
            }
            if (hasOpacity) {
                ModelMesh::Texture& tex = textureArray.back();
                tex.isTransparent = true;
                tex.color.a = std::min(opacity, tex.color.a);
            }
        }

        // Specular
        if (material->GetTextureCount(aiTextureType_SPECULAR) > 0) {
            const bool success = loadMaterialTextures(
                scene,
                *material,
                aiTextureType_SPECULAR,
                ModelMesh::TextureType::TextureSpecular,
                textureArray,
                textureStorage,
                modelDirectory
            );

            if (!success) {
                return ModelMesh(
                    std::move(vertexArray),
                    std::move(indexArray),
                    std::move(textureArray)
                );
            }
        }
        else {
            // Load embedded simple material instead of textures
            aiColor4D color4 = aiColor4D(0.f, 0.f, 0.f, 0.f);
            const aiReturn hasColor4 = material->Get(AI_MATKEY_COLOR_SPECULAR, color4);
            if (hasColor4 == AI_SUCCESS && !color4.IsBlack()) {
                // Only add the color if it is not transparent
                if (color4.a != 0.f) {
                    ModelMesh::Texture texture = {
                        .type = ModelMesh::TextureType::ColorSpecular,
                        .hasTexture = false,
                        .color = glm::vec4(color4.r, color4.g, color4.b, 1.f)
                    };
                    textureArray.push_back(std::move(texture));
                }
            }
            else {
                aiColor3D color3 = aiColor3D(0.f, 0.f, 0.f);
                const aiReturn hasColor = material->Get(AI_MATKEY_COLOR_SPECULAR, color3);
                if (hasColor == AI_SUCCESS && !color3.IsBlack()) {
                    ModelMesh::Texture texture = {
                        .type = ModelMesh::TextureType::ColorSpecular,
                        .hasTexture = false,
                        .color = glm::vec4(color3.r, color3.g, color3.b, 1.f)
                    };
                    textureArray.push_back(std::move(texture));
                }
            }
        }

        // Normal
        if (material->GetTextureCount(aiTextureType_NORMALS) > 0) {
            const bool success = loadMaterialTextures(
                scene,
                *material,
                aiTextureType_NORMALS,
                ModelMesh::TextureType::TextureNormal,
                textureArray,
                textureStorage,
                modelDirectory
            );

            if (!success) {
                return ModelMesh(
                    std::move(vertexArray),
                    std::move(indexArray),
                    std::move(textureArray)
                );
            }
        }

        // If mesh is invisible (no materials or textures) drop it unless forced to render
        // Notify unless suppresed
        if (textureArray.empty()) {
            if (forceRenderInvisible) {
                // Force invisible mesh to render with flashy colors
                ModelMesh::Texture texture;
                ModelMesh::generateDebugTexture(texture);
                textureArray.push_back(std::move(texture));
            }
            // If not forced to render, drop invisible mesh
            else if (notifyInvisibleDropped) {
                LINFO(std::format("Invisible mesh '{}' dropped", mesh.mName.C_Str()));
            }
        }

        textureArray.shrink_to_fit();
        return ModelMesh(
            std::move(vertexArray),
            std::move(indexArray),
            std::move(textureArray),
            false,
            hasVertexColors
        );
    }

    /**
     * Process a node in a recursive fashion.Process each individual mesh located at the
     * node and repeats this process on its children nodes (if any).
     */
    void processNode(const aiNode& node, const aiScene& scene,
                     std::vector<ModelNode>& nodes, int parent,
                     std::unique_ptr<ModelAnimation>& modelAnimation,
                  std::vector<modelgeometry::ModelGeometry::TextureEntry>& textureStorage,
                                                                bool forceRenderInvisible,
                                                              bool notifyInvisibleDropped,
                                                    std::filesystem::path& modelDirectory)
    {
        // Convert transform matrix of the node
        // Assimp stores matrixes in row major and glm stores matrixes in column major
        const glm::mat4 nodeTransform = glm::mat4(
            node.mTransformation.a1, node.mTransformation.b1,
            node.mTransformation.c1, node.mTransformation.d1,

            node.mTransformation.a2, node.mTransformation.b2,
            node.mTransformation.c2, node.mTransformation.d2,

            node.mTransformation.a3, node.mTransformation.b3,
            node.mTransformation.c3, node.mTransformation.d3,

            node.mTransformation.a4, node.mTransformation.b4,
            node.mTransformation.c4, node.mTransformation.d4
        );

        // Process each mesh for the current node
        std::vector<ModelMesh> meshArray;
        meshArray.reserve(node.mNumMeshes);
        for (unsigned int i = 0; i < node.mNumMeshes; i++) {
            // The node object only contains indices to the actual objects in the scene.
            // The scene contains all the data, node is just to keep stuff organized (like
            // relations between nodes)
            aiMesh* mesh = scene.mMeshes[node.mMeshes[i]];

            if (mesh->HasBones()) {
                LWARNING(
                    "Detected unsupported animation type: 'Bones', currently only "
                    "keyframe animations are supported"
                );
            }
            if (mesh->mNumAnimMeshes > 0) {
                LWARNING(
                    "Detected unsupported animation type: 'Mesh', currently only "
                    "keyframe animations are supported"
                );
            }

            ModelMesh loadedMesh = processMesh(
                *mesh,
                scene,
                textureStorage,
                modelDirectory,
                forceRenderInvisible,
                notifyInvisibleDropped
            );

            // Don't render invisible meshes
            if (loadedMesh.textures().empty()) {
                loadedMesh.setInvisible(true);
            }

            meshArray.push_back(std::move(loadedMesh));
        }

        // When the name field of a node is missing, it is replaced by Assimp with the
        // string "nodes[{i}]". Blender, for example, will automatically replace that
        // auto-generated name with the name of the contained mesh, if there is only one.
        // For consistency with Blender, we do the same here.
        std::string nodeName = node.mName.C_Str();
        const bool hasSyntheticName =
            nodeName.starts_with("nodes[") && nodeName.ends_with(']');
        if (hasSyntheticName && node.mNumMeshes == 1) {
            const aiMesh* nodeMesh = scene.mMeshes[node.mMeshes[0]];
            if (nodeMesh && nodeMesh->mName.length > 0) {
                nodeName = nodeMesh->mName.C_Str();
            }
        }

        ModelNode modelNode(nodeName.c_str(), nodeTransform, std::move(meshArray));
        modelNode.setParent(parent);
        nodes.push_back(std::move(modelNode));
        const int newNode = static_cast<int>(nodes.size() - 1);
        if (parent != -1) {
            nodes[parent].addChild(newNode);
        }

        // Check animations
        if (scene.HasAnimations()) {
            for (unsigned int a = 0; a < scene.mNumAnimations; a++) {
                aiAnimation* animation = scene.mAnimations[a];
                if (modelAnimation->name() != animation->mName.C_Str()) {
                    continue;
                }

                for (unsigned int c = 0; c < animation->mNumChannels; c++) {
                    aiNodeAnim* nodeAnim = animation->mChannels[c];

                    if (nodeAnim->mNodeName != node.mName) {
                        continue;
                    }

                    ModelAnimation::NodeAnimation nodeAnimation;
                    nodeAnimation.node = newNode;

                    for (unsigned int p = 0; p < nodeAnim->mNumPositionKeys; p++) {
                        const aiVectorKey posKey = nodeAnim->mPositionKeys[p];

                        ModelAnimation::PositionKeyframe positionKf;
                        positionKf.time =
                            std::abs(animation->mTicksPerSecond) <
                            std::numeric_limits<double>::epsilon() ? posKey.mTime :
                            posKey.mTime / animation->mTicksPerSecond;
                        positionKf.position = glm::vec3(
                            posKey.mValue.x,
                            posKey.mValue.y,
                            posKey.mValue.z
                        );

                        nodeAnimation.positions.push_back(std::move(positionKf));
                    }

                    for (unsigned int r = 0; r < nodeAnim->mNumRotationKeys; r++) {
                        const aiQuatKey rotKey = nodeAnim->mRotationKeys[r];

                        ModelAnimation::RotationKeyframe rotationKf;
                        rotationKf.time =
                            std::abs(animation->mTicksPerSecond) <
                            std::numeric_limits<double>::epsilon() ? rotKey.mTime :
                            rotKey.mTime / animation->mTicksPerSecond;
                        rotationKf.rotation = glm::quat(
                            rotKey.mValue.w,
                            rotKey.mValue.x,
                            rotKey.mValue.y,
                            rotKey.mValue.z
                        );

                        nodeAnimation.rotations.push_back(std::move(rotationKf));
                    }

                    for (unsigned int s = 0; s < nodeAnim->mNumScalingKeys; s++) {
                        const aiVectorKey scaleKey = nodeAnim->mScalingKeys[s];

                        ModelAnimation::ScaleKeyframe scaleKeyframe;
                        scaleKeyframe.time =
                            std::abs(animation->mTicksPerSecond) <
                            std::numeric_limits<double>::epsilon() ? scaleKey.mTime :
                            scaleKey.mTime / animation->mTicksPerSecond;
                        scaleKeyframe.scale = glm::vec3(
                            scaleKey.mValue.x,
                            scaleKey.mValue.y,
                            scaleKey.mValue.z
                        );

                        nodeAnimation.scales.push_back(std::move(scaleKeyframe));
                    }

                    modelAnimation->nodeAnimations().push_back(
                        std::move(nodeAnimation)
                    );
                    break;
                }
            }
        }

        // After we've processed all of the meshes (if any) we then recursively process
        // each of the children nodes (if any)
        for (unsigned int i = 0; i < node.mNumChildren; i++) {
            processNode(
                *(node.mChildren[i]),
                scene,
                nodes,
                newNode,
                modelAnimation,
                textureStorage,
                forceRenderInvisible,
                notifyInvisibleDropped,
                modelDirectory
            );
        }
    }

    void printModelTreeRecursive(const aiNode& node, const aiScene& scene,
                                 const std::string& prefix = "")
    {
        // When the name field of a node is missing, it is replaced by Assimp with the
        // string "nodes[{i}]". Blender, for example, will automatically replace that
        // auto-generated name with the name of the contained mesh, if there is only one.
        // For consistency with Blender, we do the same here.
        std::string nodeName = node.mName.C_Str();
        const bool hasSyntheticName =
            nodeName.starts_with("nodes[") && nodeName.ends_with(']');
        if (hasSyntheticName && node.mNumMeshes == 1) {
            const aiMesh* nodeMesh = scene.mMeshes[node.mMeshes[0]];
            if (nodeMesh && nodeMesh->mName.length > 0) {
                nodeName = nodeMesh->mName.C_Str();
            }
        }

        // Print the name of this node, the parent
        LINFO(std::format("{}{}", prefix, nodeName));

        // Then print the name of the children nodes with increased prefix
        for (unsigned int i = 0; i < node.mNumChildren; i++) {
            printModelTreeRecursive(
                *(node.mChildren[i]),
                scene,
                std::string(prefix.size() + TreeTabLength, ' ')
            );
        }
    }

} // namespace

namespace ghoul::io {

std::unique_ptr<modelgeometry::ModelGeometry> ModelReaderAssimp::loadModel(
                                                    const std::filesystem::path& filename,
                                                                bool forceRenderInvisible,
                                                        bool notifyInvisibleDropped) const
{
    ghoul_assert(!filename.empty(), "Filename must not be empty");

    std::filesystem::path modelDirectory = filename.parent_path();

    Assimp::Importer importer;
    const aiScene* scene = importer.ReadFile(
        filename.string(),
        aiProcess_Triangulate |       // Only triangles
        aiProcess_GenSmoothNormals |  // Generate smooth normals
        aiProcess_CalcTangentSpace    // Generate tangents and bitangents
    );

    if (!scene || scene->mFlags & AI_SCENE_FLAGS_INCOMPLETE || !scene->mRootNode) {
        throw ModelLoadException(filename, importer.GetErrorString(), this);
    }

    // Check animations
    std::unique_ptr<ModelAnimation> modelAnimation = nullptr;
    if (scene->HasAnimations()) {
        // Do not support more than one animation
        if (scene->mNumAnimations > 1) {
            LWARNING(
                "Detected more than one animation but currently only one is supported"
            );
        }
        aiAnimation* animation = scene->mAnimations[0];

        // Do not support morph animation
        if (animation->mNumMorphMeshChannels > 0) {
            LWARNING(
                "Detected unsupported animation type: 'Morph', currently only keyframe "
                "animations are supported"
            );
        }
        // Do not support animation that replaces the mesh for every frame
        if (animation->mNumMeshChannels > 0) {
            LWARNING(
                "Detected unsupported animation type: 'Mesh', currently only keyframe "
                "animations are supported"
            );
        }
        // Only support keyframe animation
        if (animation->mNumChannels > 0) {
            modelAnimation = std::make_unique<ModelAnimation>(
                animation->mName.C_Str(),
                std::abs(animation->mTicksPerSecond) <
                std::numeric_limits<double>::epsilon() ? // Not all formats have this
                animation->mDuration :
                animation->mDuration / animation->mTicksPerSecond
            );
        }
    }

    // Get info from all models in the scene
    std::vector<ModelNode> nodeArray;
    std::vector<modelgeometry::ModelGeometry::TextureEntry> textureStorage;
    textureStorage.reserve(scene->mNumTextures);
    processNode(
        *(scene->mRootNode),
        *scene,
        nodeArray,
        -1,
        modelAnimation,
        textureStorage,
        forceRenderInvisible,
        notifyInvisibleDropped,
        modelDirectory
    );

    // Return the ModelGeometry from the meshArray
    return std::make_unique<modelgeometry::ModelGeometry>(
        std::move(nodeArray),
        std::move(textureStorage),
        std::move(modelAnimation)
    );
}

void ModelReaderAssimp::printModelTree(const std::filesystem::path& filepath) const {
    Assimp::Importer importer;
    const aiScene* scene = importer.ReadFile(
        filepath.string(),
        aiProcess_Triangulate |       // Only triangles
        aiProcess_GenSmoothNormals |  // Generate smooth normals
        aiProcess_CalcTangentSpace    // Generate tangents and bitangents
    );

    if (!scene || scene->mFlags & AI_SCENE_FLAGS_INCOMPLETE || !scene->mRootNode) {
        throw ModelLoadException(filepath, importer.GetErrorString(), this);
    }

    LINFO(std::format("Model tree for '{}':", filepath));
    printModelTreeRecursive(
        *(scene->mRootNode),
        *scene
    );
}

bool ModelReaderAssimp::needsCache() const {
    return true;
}

std::vector<std::string> ModelReaderAssimp::supportedExtensions() const {
    // Taken from https://github.com/assimp/assimp/blob/master/doc/Fileformats.md
    // (An asterisk * indicates limited support)
    return {
        "obj",           // * Wavefront Object
        "fbx",           // Autodesk
        "gltf", "glb",   // glTF
        "dae", "zae",    // Collada
        "blend",         // * Blender 3D
        "3ds", "prj",    // * 3ds Max 3DS
        "ase", "ask",    // 3ds Max ASE
        "x",             // DirectX X
        "stl",           // Stereolithography
        "ifc", "ifczip", // Industry Foundation Classes (IFC / Step)
        "xgl", "zgl",    // XGL
        "ply",           // Stanford Polygon Library
        "dxf",           // AutoCAD DXF
        "lwo",           // LightWave
        "lws", "mot",    // LightWave Scene
        "lxo",           // Modo
        "ac", "ac3d",    // AC3D
        "acc",           // AC3D
        "ms3d",          // Milkshape 3D
        "cob", "scn",    // * TrueSpace
        "amf",           // * Additive manufacturing file format
        "md3",           // Quake III Mesh
        "mdl",           // Quake Mesh / 3D GameStudio Mesh
        "md2",           // Quake II Mesh
        "smd", "vta",    // Valve SMD
        "mdc",           // Return To Castle Wolfenstein Mesh
        "md5anim",       // Doom 3 / MD5 Mesh
        "md5mesh",       // Doom 3 / MD5 Mesh
        "md5camera",     // Doom 3 / MD5 Mesh
        "nff", "enff",   // Neutral File Format
        "raw",           // Raw
        "sib",           // * Silo SIB
        "off",           // OFF
        "irr",           // Irrlicht Scene
        "irrmesh",       // Irrlicht Mesh
        "q3o", "q3s",    // Quick3D
        "b3d",           // BlitzBasic 3D
        "3d", "uc",      // Unreal Mesh
        "mesh",          // Ogre3D Mesh
        "mesh.xml",      // Ogre3D Mesh
        "ogex",          // Open Game Engine Exchange
        "pk3", "bsp",    // Quake III BSP
        "ndo",           // Nendo Mesh
        "assbin",        // Assimp Binary
        "3mf",           // 3D Manufacturing Format
        "x3d", "x3db",   // * Extensible 3D
        "m3d"            // Model 3D
    };
}

} // namespace ghoul::io
