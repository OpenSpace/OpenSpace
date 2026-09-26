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

#ifndef __GHOUL___MODELREADERBINARY___H__
#define __GHOUL___MODELREADERBINARY___H__

#include <ghoul/io/model/modelreaderbase.h>

namespace ghoul::modelgeometry { class ModelGeometry; }

namespace ghoul::io {

/**
 * This model reader loads a custom OpenSpace model from the provided file.
 */
class ModelReaderBinary : public ModelReaderBase {
public:
    /**
     * Loads the 3D OpenSpace model file pointed to by \p filename and returns a
     * constructed ModelGeometry from it.
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
     * Returns if this reader needs a cache file or not.
     *
     * \return A boolean for if this reader needs a cache file or not
     */
    bool needsCache() const override;

    /**
     * Returns a list of all extensions that this ModelReaderBinary supports.
     *
     * \return A list of all extensions that this ModelReaderBinary supports
     */
    std::vector<std::string> supportedExtensions() const override;
};

} // namespace ghoul::io

#endif // __GHOUL___MODELREADERBINARY___H__
