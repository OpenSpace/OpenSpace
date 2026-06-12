/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <ghoul/io/model/modelreader.h>
#include <ghoul/io/model/modelreaderassimp.h>
#include <ghoul/io/model/modelreaderbase.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>

namespace {

    /**
     * Prints the model tree of the given filepath model file. The node names detected
     * during reading and the hierarchy of the nodes will be printed. This is useful for
     * debugging and finding correct node names for applying custom transformations to
     * internal model nodes.
     *
     * The given filepath must not be empty, must contain an extension, and must be a
     * valid model file that can be read with the `ModelReaderAssimp` reader. The
     * `ModelReaderAssimp` reader must have been added to the `ModelReader` before calling
     * this function. Will throw a `ModelLoadException` if there was an error reading the
     * file, or a `MissingReaderException` if there was no reader for the specified
     * filepath.
     *
     * \param filepath The model file on disk whose model tree should be printed.
     */
[[codegen::luawrap]] void printModelTree(std::filesystem::path filepath) {
    ghoul_assert(!filepath.empty(), "Filepath must not be empty");

    std::string extension = filepath.extension().string();
    if (!extension.empty()) {
        extension = extension.substr(1);
    }
    ghoul_assert(!extension.empty(), "Filepath must have an extension");

    ghoul::io::ModelReaderBase* reader =
        ghoul::io::ModelReader::ref().readerForExtension(extension);

    if (!reader) {
        throw ghoul::io::ModelReader::MissingReaderException(extension, filepath);
    }

    // (malej 2026-05-29) Only the ModelReaderAssimp can print model trees
    ghoul::io::ModelReaderAssimp* typedReader;
    try {
        typedReader = &dynamic_cast<ghoul::io::ModelReaderAssimp&>(*reader);
    }
    catch (std::exception& e) {
        LERRORC("RenderableModel", std::format("Cannot find a suitable reader to print "
            "the model tree for model {}", filepath)
        );
        return;
    }

    typedReader->printModelTree(filepath);
}

} // namespace

#include "renderablemodel_lua_codegen.cpp"
