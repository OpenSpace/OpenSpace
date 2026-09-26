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

#ifndef __GHOUL___MODELREADERASSIMP___H__
#define __GHOUL___MODELREADERASSIMP___H__

#include <ghoul/io/model/modelreaderbase.h>

#include <filesystem>
#include <memory>
#include <string>
#include <vector>

struct aiMesh;
struct aiScene;

namespace ghoul::io {

/**
 * This model reader loads the provided file using the Assimp library. This simple method
 * loads multiple shapes, from different 3D file formats. All supported formats are listed
 * in ModelReaderAssimp::supportedExtensions().
 *
 * \see https://github.com/assimp/assimp
 */
class ModelReaderAssimp : public ModelReaderBase {
public:
    /**
     * Loads the 3D model (anyone from the previous list) file pointed to by \p filename
     * and returns a constructed ModelGeometry from it.
     *
     * \param filename The geometric model file to be loaded
     * \param forceRenderInvisible Force invisible meshes to render or not
     * \param notifyInvisibleDropped Notify in log if invisible meshses were dropped
     * \return The ModelGeometry containing the model
     *
     * \throw ModelLoadException If there was an error loading the model from \p filename
     * \pre \p filename must not be empty
     */
    std::unique_ptr<modelgeometry::ModelGeometry> loadModel(
        const std::filesystem::path& filename, bool forceRenderInvisible = false,
        bool notifyInvisibleDropped = true) const override;

    /**
     * Prints the model tree of the model file pointed to by \p filepath to the info log.
     * This is useful for identifying the structure of the model and the names of the
     * nodes, which can be used for custom node transformations and animations.
     *
     * \param filepath The geometric model file to print the model tree for
     */
    void printModelTree(const std::filesystem::path& filepath) const;

    /**
     * Returns if this reader needs a cache file or not.
     *
     * \return A boolean for if this reader needs a cache file or not
     */
    bool needsCache() const override;

    /**
     * Returns a list of all extensions that this ModelReaderAssimp supports.
     *
     * \return A list of all extensions that this ModelReaderAssimp supports
     */
    std::vector<std::string> supportedExtensions() const override;
};

} // namespace ghoul::io

#endif // __GHOUL___MODELREADERASSIMP___H__
