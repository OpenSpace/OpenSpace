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

#ifndef __GHOUL___MODELGEOMETRY___H__
#define __GHOUL___MODELGEOMETRY___H__

#include <ghoul/misc/exception.h>
#include <ghoul/io/model/modelanimation.h>
#include <ghoul/io/model/modelnode.h>
#include <ghoul/opengl/texture.h>
#include <filesystem>
#include <memory>
#include <string>
#include <vector>

namespace ghoul::opengl { class ProgramObject; }

namespace ghoul::modelgeometry {

class ModelGeometry {
public:
    /**
     * The exception that gets thrown if there was an error loading the cache file or
     * saving this model to a cache file.
     */
    struct ModelCacheException final : public RuntimeError {
        explicit ModelCacheException(std::filesystem::path file, std::string msg);

        /// The file that caused the exception to be thrown
        const std::filesystem::path filename;

        /// The error message that occurred
        const std::string errorMessage;
    };

    struct TextureEntry {
        std::string name;
        std::unique_ptr<opengl::Texture> texture;
    };

    ModelGeometry(std::vector<io::ModelNode> nodes,
        std::vector<TextureEntry> textureStorage,
        std::unique_ptr<io::ModelAnimation> animation,
        bool isTransparent = false, bool hasCalcTransparency = false);
    ModelGeometry(ModelGeometry&&) noexcept = default;
    ~ModelGeometry() noexcept = default;

    static std::unique_ptr<modelgeometry::ModelGeometry> loadCacheFile(
        const std::filesystem::path& cachedFile, bool forceRenderInvisible,
        bool notifyInvisibleDropped);
    bool saveToCacheFile(const std::filesystem::path& cachedFile) const;

    void setTimeScale(float timeScale);
    void enableAnimation(bool value);
    void updateCustomNodeTransform(std::string_view nodeName,
        const glm::dmat4& customTransform);
    void setReplaceWithCustomTransforms(bool replaceWithCustomTransforms);

    void initialize();
    void deinitialize();
    void render(opengl::ProgramObject& program, bool isFullyTexturedModel = true,
        bool isProjection = false) const;
    void update(double now);

    double boundingRadius() const;
    void calculateBoundingRadius();
    bool hasAnimation() const;
    double animationDuration() const;
    void calculateTransparency();
    void recalculateTransparency();
    bool isTransparent() const;

    std::vector<io::ModelNode>& nodes();
    const std::vector<io::ModelNode>& nodes() const;
    std::vector<TextureEntry>& textureStorage();
    const std::vector<TextureEntry>& textureStorage() const;

protected:
    double _boundingRadius = 0.0;
    bool _animationEnabled = false;
    std::vector<io::ModelNode> _nodes;
    std::vector<TextureEntry> _textureStorage;
    std::unique_ptr<io::ModelAnimation> _animation;
    bool _hasCalcTransparency = false;
    bool _isTransparent = false;
    bool _replaceWithCustomTransforms = false;
};

} // namespace ghoul::modelgeometry

#endif // __GHOUL___MODELGEOMETRY___H__
