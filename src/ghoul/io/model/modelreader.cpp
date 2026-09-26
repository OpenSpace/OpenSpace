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

#include <ghoul/io/model/modelreader.h>

#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/io/model/modelreaderbase.h>
#include <ghoul/io/model/modelgeometry.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/stringhelper.h>
#include <algorithm>
#include <string_view>
#include <utility>
#include <vector>

namespace {
    constexpr std::string_view _loggerCat = "ModelReader";
} // namespace

namespace ghoul::io {

ModelReader::MissingReaderException::MissingReaderException(std::string extension,
                                                            std::filesystem::path file_)
    : RuntimeError(std::format(
        "No reader was found for extension '{}' with file '{}'", extension, file_
    ))
    , fileExtension(std::move(extension))
    , file(std::move(file_))
{}

ModelReader& ModelReader::ref() {
    static ModelReader modelReader;
    return modelReader;
}

std::unique_ptr<modelgeometry::ModelGeometry> ModelReader::loadModel(
                                                    const std::filesystem::path& filename,
                                                ForceRenderInvisible forceRenderInvisible,
                                            NotifyInvisibleDropped notifyInvisibleDropped)
{
    ZoneScoped;

    ghoul_assert(!_readers.empty(), "No readers were registered before");
    ghoul_assert(!filename.empty(), "Filename must not be empty");

    std::string extension = filename.extension().string();
    if (!extension.empty()) {
        extension = extension.substr(1);
    }
    ghoul_assert(!extension.empty(), "Filename must have an extension");

    ModelReaderBase* reader = readerForExtension(extension);

    if (!reader) {
        throw MissingReaderException(extension, filename);
    }

    if (!reader->needsCache()) {
        LINFO(std::format("Loading ModelGeometry file '{}'", filename));
        return reader->loadModel(filename, forceRenderInvisible, notifyInvisibleDropped);
    }

    std::filesystem::path cachedFile = FileSys.cacheManager()->cachedFilename(filename);
    const bool hasCachedFile = std::filesystem::is_regular_file(cachedFile);
    if (hasCachedFile) {
        LINFO(std::format(
            "Cached file '{}' used for ModelGeometry file '{}'", cachedFile, filename
        ));

        try {
            std::unique_ptr<modelgeometry::ModelGeometry> model =
                modelgeometry::ModelGeometry::loadCacheFile(
                    cachedFile,
                    forceRenderInvisible,
                    notifyInvisibleDropped
                );
            return model;
        }
        catch (const modelgeometry::ModelGeometry::ModelCacheException& e) {
            LINFO(std::format(
                "Encountered problem '{}' while loading model from cache file '{}'. "
                "Deleting cache", e.errorMessage, cachedFile
            ));
            FileSys.cacheManager()->removeCacheFile(filename);
            // Intentional fall-through to the 'else' computation to generate the cache
            // file for the next run
        }

    }
    else {
        LINFO(std::format("Cache for ModelGeometry file '{}' not found", filename));
    }

    LINFO(std::format("Loading ModelGeometry file '{}'", filename));

    std::unique_ptr<modelgeometry::ModelGeometry> model =
        reader->loadModel(filename, forceRenderInvisible, notifyInvisibleDropped);

    LINFO("Saving cache");
    try {
        model->saveToCacheFile(cachedFile);
    }
    catch (const modelgeometry::ModelGeometry::ModelCacheException& e) {
        LINFO(std::format(
            "Encountered problem '{}' while saving model to cache file '{}'. Deleting "
            "cache", e.errorMessage, e.filename
        ));

        FileSys.cacheManager()->removeCacheFile(filename);
    }
    return model;
}

std::vector<std::string> ModelReader::supportedExtensions() const {
    std::vector<std::string> result;
    for (const std::unique_ptr<ModelReaderBase>& i : _readers) {
        std::vector<std::string> extensions = i->supportedExtensions();
        result.insert(result.end(), extensions.begin(), extensions.end());
    }
    return result;
}

void ModelReader::addReader(std::unique_ptr<ModelReaderBase> reader) {
    _readers.push_back(std::move(reader));
}

ModelReaderBase* ModelReader::readerForExtension(const std::string& extension) {
    const std::string lowerExtension = toLowerCase(extension);

    for (const std::unique_ptr<ModelReaderBase>& reader : _readers) {
        std::vector<std::string> extensions = reader->supportedExtensions();
        auto it = std::find(extensions.cbegin(), extensions.cend(), lowerExtension);
        if (it != extensions.end()) {
            return reader.get();
        }
    }

    return nullptr;
}

} // namespace ghoul::io
