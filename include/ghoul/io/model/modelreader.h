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

#ifndef __GHOUL___MODELREADER___H__
#define __GHOUL___MODELREADER___H__

#include <ghoul/misc/exception.h>

namespace ghoul::modelgeometry { class ModelGeometry; }

namespace ghoul::io {

class ModelReaderBase;

/**
 * This class manages multiple ModelReaderBase and makes them available through one method
 * #loadModel. ModelReaderBases are added through the method addReader. The class provides
 * a static member, but also allows users to create local variants. ModelReaderBases can
 * be reused between multiple ModelReaders.
 */
class ModelReader {
public:
    BooleanType(ForceRenderInvisible);
    BooleanType(NotifyInvisibleDropped);

    /**
     * Exception that gets thrown when there is no reader for the provided \p extension.
     */
    struct MissingReaderException final : public RuntimeError {
        explicit MissingReaderException(std::string extension,
            std::filesystem::path file_);

        const std::string fileExtension;
        const std::filesystem::path file;
    };

    /**
     * Returns the static variant of the ModelReader.
     *
     * \return The static variant of the ModelReader
     */
    static ModelReader& ref();

    /**
     * Loads the provided \p filename into a ModelGeometry and returns it. The correct
     * ModelReaderBase is determined by the extension of the \p filename. If a part of the
     * model is invisible (has no texture or color) it will by default be dropped and not
     * rendered at all. If the provided \p forceRenderInvisible is enabled the invisible
     * parts will instead be forced to render with a colorful pink and green chessboard
     * pattern. This material will also be forced if there is any error reading the
     * texture or material.
     *
     * \param filename The name of the file which should be loaded into a ModelGeometry
     * \param forceRenderInvisible Force invisible meshes to render or not
     * \param notifyInvisibleDropped Notify in log if invisible meshes were dropped
     *
     * \throw ModelLoadException If there was an error reading the \p filename
     * \throw MissingReaderException If there was no reader for the specified \p filename
     * \pre \p filename must not be empty
     * \pre \p filename must have an extension
     * \pre At least one ModelReaderBase must have been added to the ModelReader before
     *      (addReader)
     */
    std::unique_ptr<modelgeometry::ModelGeometry> loadModel(
        const std::filesystem::path& filename,
        ForceRenderInvisible forceRenderInvisible = ForceRenderInvisible::No,
        NotifyInvisibleDropped notifyInvisibleDropped = NotifyInvisibleDropped::Yes);

    /**
     * Returns a list of all the extensions that are supported by registered readers. If a
     * file with an extension included in this list is passed to the loadModel method and
     * the file is not corrupted, it will be successfully loaded.
     *
     * \return A list of all supported extensions
     */
    std::vector<std::string> supportedExtensions() const;

    /**
     * Adds the \p reader to this ModelReader and makes it available through subsequent
     * calls to loadModel. If an extension is supported by multiple ModelReaderBases, the
     * ModelReaderBase that was added first will be used.
     *
     * \param reader The reader that is to be added to this ModelReader
     *
     * \pre \p reader must not have been added to this ModelReader before
     */
    void addReader(std::unique_ptr<ModelReaderBase> reader);

    /**
     * Returns the ModelReaderBase that is responsible for the provided extension.
     *
     * \param extension The extension for which the ModelReaderBase should be returned
     * \return The first match of a reader that can read the provided \p extension, or
     *         `nullptr` if no such reader exists
     */
    ModelReaderBase* readerForExtension(const std::string& extension);

private:
    /// The list of all registered readers
    std::vector<std::unique_ptr<ModelReaderBase>> _readers;
};

} // namespace ghoul::io

#endif // __GHOUL___MODELREADER___H__
