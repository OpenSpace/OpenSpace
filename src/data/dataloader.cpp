/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/data/dataloader.h>

#include <openspace/data/csvloader.h>
#include <openspace/data/speckloader.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/stringhelper.h>
#include <cctype>
#include <fstream>
#include <functional>
#include <string_view>

namespace {
    constexpr int8_t DataCacheFileVersion = 13;
    constexpr int8_t LabelCacheFileVersion = 11;
    constexpr int8_t ColorCacheFileVersion = 11;

    template <typename T, typename U>
    void checkSize(U value, std::string_view message) {
        if (value > std::numeric_limits<U>::max()) {
            throw ghoul::RuntimeError(std::format("Error saving file '{}'", message));
        }
    }

    template <typename T>
    using LoadCacheFunc = std::function<std::optional<T>(std::filesystem::path)>;

    template <typename T>
    using SaveCacheFunc = std::function<void(const T&, std::filesystem::path)>;

    template <typename T>
    using LoadDataFunc = std::function<T(
        std::filesystem::path, std::optional<openspace::dataloader::DataMapping> specs
    )>;

    template <typename T>
    T internalLoadFileWithCache(std::filesystem::path filePath,
                                std::optional<openspace::dataloader::DataMapping> specs,
                                LoadDataFunc<T> loadFunction,
                                LoadCacheFunc<T> loadCacheFunction,
                                SaveCacheFunc<T> saveCacheFunction)
    {
        static_assert(
            std::is_same_v<T, openspace::dataloader::Dataset> ||
            std::is_same_v<T, openspace::dataloader::Labelset> ||
            std::is_same_v<T, openspace::dataloader::ColorMap>
        );

        ZoneScoped;

        std::string info;
        if (specs.has_value()) {
            info = openspace::dataloader::generateHashString(*specs);
        }
        std::filesystem::path cached = FileSys.cacheManager()->cachedFilename(
            filePath,
            info
        );

        if (std::filesystem::exists(cached)) {
            LINFOC(
                "DataLoader",
                std::format("Cached file {} used for file {}", cached, filePath)
            );

            std::optional<T> dataset = loadCacheFunction(cached);
            if (dataset.has_value()) {
                // We could load the cache file and we are now done with this
                return std::move(*dataset);
            }
            else {
                FileSys.cacheManager()->removeCacheFile(cached);
            }
        }

        LINFOC("DataLoader", std::format("Loading file '{}'", filePath));
        T dataset = loadFunction(filePath, specs);

        if (!dataset.entries.empty()) {
            LINFOC("DataLoader", "Saving cache");
            saveCacheFunction(dataset, cached);
        }

        return dataset;
    }
} // namespace

namespace openspace::dataloader {

namespace data {

Dataset loadFile(std::filesystem::path path, std::optional<DataMapping> specs) {
    ZoneScoped;

    ghoul_assert(std::filesystem::exists(path), "File must exist");

    const std::ifstream file = std::ifstream(path);
    if (!file.good()) {
        throw ghoul::RuntimeError(std::format("Failed to open data file '{}'", path));
    }

    const std::string extension = ghoul::toLowerCase(path.extension().string());

    Dataset res;
    if (extension == ".csv") {
        res = csv::loadCsvFile(path, std::move(specs));
    }
    else if (extension == ".speck") {
        res = speck::loadSpeckFile(path, std::move(specs));
    }
    else {
        LERRORC("DataLoader", std::format(
            "Could not read data file '{}'. File format '{}' is not supported",
            path, path.extension()
        ));
    }

    return res;
}

std::optional<Dataset> loadCachedFile(const std::filesystem::path& path) {
    ZoneScoped;

    std::ifstream file = std::ifstream(path, std::ios::binary);
    if (!file.good()) {
        return std::nullopt;
    }

    Dataset result;

    int8_t fileVersion = 0;
    file.read(reinterpret_cast<char*>(&fileVersion), sizeof(int8_t));
    if (fileVersion != DataCacheFileVersion) {
        // Incompatible version and we won't be able to read the file
        return std::nullopt;
    }

    //
    // Read variables
    uint16_t nVariables = 0;
    file.read(reinterpret_cast<char*>(&nVariables), sizeof(uint16_t));
    result.variables.resize(nVariables);
    for (int i = 0; i < nVariables; i += 1) {
        ZoneScopedN("Variable");

        Dataset::Variable var;

        int16_t idx = 0;
        file.read(reinterpret_cast<char*>(&idx), sizeof(int16_t));
        var.index = idx;

        uint16_t len = 0;
        file.read(reinterpret_cast<char*>(&len), sizeof(uint16_t));
        var.name.resize(len);
        file.read(var.name.data(), len);

        result.variables[i] = std::move(var);
    }

    //
    // Read textures
    uint16_t nTextures = 0;
    file.read(reinterpret_cast<char*>(&nTextures), sizeof(uint16_t));
    result.textures.resize(nTextures);
    for (int i = 0; i < nTextures; i += 1) {
        ZoneScopedN("Texture");

        Dataset::Texture tex;

        int16_t idx = 0;
        file.read(reinterpret_cast<char*>(&idx), sizeof(int16_t));
        tex.index = idx;

        uint16_t len = 0;
        file.read(reinterpret_cast<char*>(&len), sizeof(uint16_t));
        tex.file.resize(len);
        file.read(tex.file.data(), len);

        result.textures[i] = std::move(tex);
    }

    //
    // Read indices
    int16_t texDataIdx = 0;
    file.read(reinterpret_cast<char*>(&texDataIdx), sizeof(int16_t));
    result.textureDataIndex = texDataIdx;

    int16_t oriDataIdx = 0;
    file.read(reinterpret_cast<char*>(&oriDataIdx), sizeof(int16_t));
    result.orientationDataIndex = oriDataIdx;

    //
    // Read entries
    uint64_t nEntries = 0;
    file.read(reinterpret_cast<char*>(&nEntries), sizeof(uint64_t));
    result.entries.reserve(nEntries);
    for (uint64_t i = 0; i < nEntries; i += 1) {
        Dataset::Entry e;
        file.read(reinterpret_cast<char*>(&e.position.x), 3 * sizeof(float));

        // For now we just store the length of the comment. Since the comments are stored
        // in one block after the data entries, we can use the length later to extract the
        // contents of this entries comment out of the big block
        uint16_t len = 0;
        file.read(reinterpret_cast<char*>(&len), sizeof(uint16_t));
        if (len > 0) {
            // If there is a comment, we already allocate the space for it here. This way
            // we don't need to separately store the length of it, but can use the size of
            // the vector instead
            std::string comment;
            comment.resize(len);
            e.comment = std::move(comment);
        }

        result.entries.push_back(std::move(e));
    }

    //
    // Read the data values next
    uint16_t nValues = 0;
    file.read(reinterpret_cast<char*>(&nValues), sizeof(uint16_t));
    std::vector<float> entriesBuffer;
    entriesBuffer.resize(nEntries * nValues);
    file.read(
        reinterpret_cast<char*>(entriesBuffer.data()),
        nEntries * nValues * sizeof(float)
    );

    //
    // Read comments in one block and then assign them to the data entries
    uint64_t totalCommentLength = 0;
    file.read(reinterpret_cast<char*>(&totalCommentLength), sizeof(uint64_t));
    std::vector<char> commentBuffer;
    commentBuffer.resize(totalCommentLength);
    file.read(commentBuffer.data(), totalCommentLength);

    //
    // Now we have the comments and the data values, we need to implant them into the
    // data entries

    // commentIdx is the running index into the total comment buffer
    int commentIdx = 0;
    int valuesIdx = 0;
    for (Dataset::Entry& e : result.entries) {
        e.data.resize(nValues);
        std::memcpy(
            e.data.data(),
            entriesBuffer.data() + valuesIdx,
            nValues * sizeof(float)
        );
        valuesIdx += nValues;

        if (e.comment.has_value()) {
            ghoul_assert(commentIdx < commentBuffer.size(), "Index too large");

            // If we have a comment, we need to extract its length's worth of characters
            // from the buffer
            std::memcpy(e.comment->data(), &commentBuffer[commentIdx], e.comment->size());

            // and then advance the index
            commentIdx += static_cast<int>(e.comment->size());
        }
    }

    //
    // Read max data point variable
    float max = 0.f;
    file.read(reinterpret_cast<char*>(&max), sizeof(float));
    result.maxPositionComponent = max;

    return result;
}

void saveCachedFile(const Dataset& dataset, const std::filesystem::path& path) {
    ZoneScoped;

    std::ofstream file = std::ofstream(path, std::ofstream::binary);

    file.write(reinterpret_cast<const char*>(&DataCacheFileVersion), sizeof(int8_t));

    //
    // Store variables
    checkSize<uint16_t>(dataset.variables.size(), "Too many variables");
    uint16_t nVariables = static_cast<uint16_t>(dataset.variables.size());
    file.write(reinterpret_cast<const char*>(&nVariables), sizeof(uint16_t));
    for (const Dataset::Variable& var : dataset.variables) {
        checkSize<int16_t>(var.index, "Variable index too large");
        int16_t idx = static_cast<int16_t>(var.index);
        file.write(reinterpret_cast<const char*>(&idx), sizeof(int16_t));

        checkSize<uint16_t>(var.name.size(), "Variable name too long");
        uint16_t len = static_cast<uint16_t>(var.name.size());
        file.write(reinterpret_cast<const char*>(&len), sizeof(uint16_t));
        file.write(var.name.data(), len);
    }

    //
    // Store textures
    checkSize<uint16_t>(dataset.textures.size(), "Too many textures");
    uint16_t nTextures = static_cast<uint16_t>(dataset.textures.size());
    file.write(reinterpret_cast<const char*>(&nTextures), sizeof(uint16_t));
    for (const Dataset::Texture& tex : dataset.textures) {
        checkSize<int16_t>(tex.index, "Texture index too large");
        int16_t idx = static_cast<int16_t>(tex.index);
        file.write(reinterpret_cast<const char*>(&idx), sizeof(int16_t));


        checkSize<uint16_t>(tex.file.size(), "Texture file too long");
        uint16_t len = static_cast<uint16_t>(tex.file.size());
        file.write(reinterpret_cast<const char*>(&len), sizeof(uint16_t));
        file.write(tex.file.data(), len);
    }

    //
    // Store indices
    checkSize<int16_t>(dataset.textureDataIndex, "Texture index too large");
    int16_t texIdx = static_cast<int16_t>(dataset.textureDataIndex);
    file.write(reinterpret_cast<const char*>(&texIdx), sizeof(int16_t));

    checkSize<int16_t>(dataset.orientationDataIndex, "Orientation index too large");
    int16_t orientationIdx = static_cast<int16_t>(dataset.orientationDataIndex);
    file.write(reinterpret_cast<const char*>(&orientationIdx), sizeof(int16_t));

    //
    // Store entries
    checkSize<uint64_t>(dataset.entries.size(), "Too many entries");
    uint64_t nEntries = static_cast<uint64_t>(dataset.entries.size());
    file.write(reinterpret_cast<const char*>(&nEntries), sizeof(uint64_t));

    // We assume the number of values for each dataset to be the same, so we can store
    // them upfront
    size_t nValuesF = dataset.entries.empty() ? 0 : dataset.entries[0].data.size();
    checkSize<uint16_t>(nValuesF, "Too many data variables");
    uint16_t nValues = static_cast<uint16_t>(nValuesF);
    std::vector<float> valuesBuffer;
    valuesBuffer.reserve(dataset.entries.size() * nValues);

    uint64_t totalCommentLength = 0;
    for (const Dataset::Entry& e : dataset.entries) {
        file.write(reinterpret_cast<const char*>(&e.position.x), 3 * sizeof(float));

        valuesBuffer.insert(valuesBuffer.end(), e.data.begin(), e.data.end());

        if (e.comment.has_value()) {
            checkSize<uint16_t>(e.comment->size(), "Comment too long");
        }
        uint16_t commentLen = e.comment.has_value() ?
            static_cast<uint16_t>(e.comment->size()) :
            0;
        file.write(reinterpret_cast<const char*>(&commentLen), sizeof(uint16_t));
        totalCommentLength += commentLen;
    }

    // Write all of the datavalues next
    file.write(reinterpret_cast<const char*>(&nValues), sizeof(uint16_t));
    file.write(
        reinterpret_cast<const char*>(valuesBuffer.data()),
        valuesBuffer.size() * sizeof(float)
    );

    //
    // Write all of the comments next. We don't have to store the individual comment
    // lengths as the data values written before already have those stored. And since we
    // are reading the comments in the same order as the dataset entries, we're good
    file.write(reinterpret_cast<const char*>(&totalCommentLength), sizeof(uint64_t));
    for (const Dataset::Entry& e : dataset.entries) {
        if (e.comment.has_value()) {
            file.write(e.comment->data(), e.comment->size());
        }
    }

    //
    // Store max data point variable
    file.write(
        reinterpret_cast<const char*>(&dataset.maxPositionComponent),
        sizeof(float)
    );
}

Dataset loadFileWithCache(std::filesystem::path path, std::optional<DataMapping> specs) {
    return internalLoadFileWithCache<Dataset>(
        std::move(path),
        std::move(specs),
        &loadFile,
        &loadCachedFile,
        &saveCachedFile
    );
}

} // namespace data

namespace label {

Labelset loadFile(std::filesystem::path path, std::optional<DataMapping>) {
    ZoneScoped;

    ghoul_assert(std::filesystem::exists(path), "File must exist");

    const std::ifstream file = std::ifstream(path);
    if (!file.good()) {
        throw ghoul::RuntimeError(std::format("Failed to open dataset file '{}'", path));
    }

    const std::string extension = ghoul::toLowerCase(path.extension().string());

    Labelset res;
    if (extension == ".label") {
        res = speck::loadLabelFile(path);
    }
    else {
        LERRORC("DataLoader", std::format(
            "Could not read label data file '{}'. File format '{}' is not supported",
            path, path.extension()
        ));
    }

    return res;
}

std::optional<Labelset> loadCachedFile(const std::filesystem::path& path) {
    std::ifstream file(path, std::ios::binary);
    if (!file.good()) {
        return std::nullopt;
    }

    int8_t fileVersion = 0;
    file.read(reinterpret_cast<char*>(&fileVersion), sizeof(int8_t));
    if (fileVersion != LabelCacheFileVersion) {
        // Incompatible version and we won't be able to read the file
        return std::nullopt;
    }

    Labelset result;

    int16_t textColorIdx = 0;
    file.read(reinterpret_cast<char*>(&textColorIdx), sizeof(int16_t));
    result.textColorIndex = textColorIdx;

    uint32_t nEntries = 0;
    file.read(reinterpret_cast<char*>(&nEntries), sizeof(uint32_t));
    result.entries.reserve(nEntries);
    for (unsigned int i = 0; i < nEntries; i += 1) {
        Labelset::Entry e;
        file.read(reinterpret_cast<char*>(&e.position.x), sizeof(float));
        file.read(reinterpret_cast<char*>(&e.position.y), sizeof(float));
        file.read(reinterpret_cast<char*>(&e.position.z), sizeof(float));

        // Identifier
        uint8_t idLen = 0;
        file.read(reinterpret_cast<char*>(&idLen), sizeof(uint8_t));
        e.identifier.resize(idLen);
        file.read(e.identifier.data(), idLen);

        // Text
        uint16_t len = 0;
        file.read(reinterpret_cast<char*>(&len), sizeof(uint16_t));
        e.text.resize(len);
        file.read(e.text.data(), len);

        result.entries.push_back(e);
    }

    return result;
}

void saveCachedFile(const Labelset& labelset, const std::filesystem::path& path) {
    std::ofstream file = std::ofstream(path, std::ofstream::binary);

    file.write(reinterpret_cast<const char*>(&LabelCacheFileVersion), sizeof(int8_t));

    //
    // Storage text color
    checkSize<int16_t>(labelset.textColorIndex, "Too high text color");
    int16_t textColorIdx = static_cast<int16_t>(labelset.textColorIndex);
    file.write(reinterpret_cast<const char*>(&textColorIdx), sizeof(int16_t));

    //
    // Storage text lines
    checkSize<uint32_t>(labelset.entries.size(), "Too many entries");
    uint32_t nEntries = static_cast<uint32_t>(labelset.entries.size());
    file.write(reinterpret_cast<const char*>(&nEntries), sizeof(uint32_t));
    for (const Labelset::Entry& e : labelset.entries) {
        file.write(reinterpret_cast<const char*>(&e.position.x), sizeof(float));
        file.write(reinterpret_cast<const char*>(&e.position.y), sizeof(float));
        file.write(reinterpret_cast<const char*>(&e.position.z), sizeof(float));

        // Identifier
        checkSize<uint8_t>(e.identifier.size(), "Identifier too long");
        uint8_t idLen = static_cast<uint8_t>(e.identifier.size());
        file.write(reinterpret_cast<const char*>(&idLen), sizeof(uint8_t));
        file.write(e.identifier.data(), idLen);

        // Text
        checkSize<uint16_t>(e.text.size(), "Text too long");
        uint16_t len = static_cast<uint16_t>(e.text.size());
        file.write(reinterpret_cast<const char*>(&len), sizeof(uint16_t));
        file.write(e.text.data(), len);
    }
}

Labelset loadFileWithCache(std::filesystem::path path) {
    return internalLoadFileWithCache<Labelset>(
        std::move(path),
        std::nullopt,
        &loadFile,
        &loadCachedFile,
        &saveCachedFile
    );
}

Labelset loadFromDataset(const Dataset& dataset) {
    Labelset res;
    res.entries.reserve(dataset.entries.size());

    int count = 0;
    for (const Dataset::Entry& entry : dataset.entries) {
        Labelset::Entry label;
        label.position = entry.position;
        label.text = entry.comment.value_or("MISSING LABEL");
        // @TODO: make is possible to configure this identifier?
        label.identifier = std::format("Point-{}", count);
        res.entries.push_back(std::move(label));
    }

    return res;
}

} // namespace label

namespace color {

ColorMap loadFile(std::filesystem::path path, std::optional<DataMapping>) {
    ghoul_assert(std::filesystem::exists(path), "File must exist");

    const std::ifstream file = std::ifstream(path);
    if (!file.good()) {
        throw ghoul::RuntimeError(std::format("Failed to open colormap file '{}'", path));
    }

    const std::string extension = ghoul::toLowerCase(path.extension().string());

    ColorMap res;
    if (extension == ".cmap") {
        res = speck::loadCmapFile(path);
    }
    else {
        LERRORC("DataLoader", std::format(
            "Could not read color map file '{}'. File format '{}' is not supported",
            path, path.extension()
        ));
    }

    return res;
}

std::optional<ColorMap> loadCachedFile(const std::filesystem::path& path) {
    std::ifstream file(path, std::ios::binary);
    if (!file.good()) {
        return std::nullopt;
    }

    int8_t fileVersion = 0;
    file.read(reinterpret_cast<char*>(&fileVersion), sizeof(int8_t));
    if (fileVersion != ColorCacheFileVersion) {
        // Incompatible version and we won't be able to read the file
        return std::nullopt;
    }

    ColorMap result;

    uint32_t nColors = 0;
    file.read(reinterpret_cast<char*>(&nColors), sizeof(uint32_t));
    result.entries.reserve(nColors);
    for (unsigned int i = 0; i < nColors; i += 1) {
        glm::vec4 color;
        file.read(reinterpret_cast<char*>(&color.x), sizeof(float));
        file.read(reinterpret_cast<char*>(&color.y), sizeof(float));
        file.read(reinterpret_cast<char*>(&color.z), sizeof(float));
        file.read(reinterpret_cast<char*>(&color.w), sizeof(float));
        result.entries.push_back(color);
    }

    glm::vec4 color;

    bool hasBelowColor = false;
    file.read(reinterpret_cast<char*>(&hasBelowColor), sizeof(bool));
    file.read(reinterpret_cast<char*>(&color.x), sizeof(float));
    file.read(reinterpret_cast<char*>(&color.y), sizeof(float));
    file.read(reinterpret_cast<char*>(&color.z), sizeof(float));
    file.read(reinterpret_cast<char*>(&color.w), sizeof(float));

    if (hasBelowColor) {
        result.belowRangeColor = color;
    }

    bool hasAboveColor = false;
    file.read(reinterpret_cast<char*>(&hasAboveColor), sizeof(bool));
    file.read(reinterpret_cast<char*>(&color.x), sizeof(float));
    file.read(reinterpret_cast<char*>(&color.y), sizeof(float));
    file.read(reinterpret_cast<char*>(&color.z), sizeof(float));
    file.read(reinterpret_cast<char*>(&color.w), sizeof(float));

    if (hasAboveColor) {
        result.aboveRangeColor = color;
    }

    bool hasNanColor = false;
    file.read(reinterpret_cast<char*>(&hasNanColor), sizeof(bool));
    file.read(reinterpret_cast<char*>(&color.x), sizeof(float));
    file.read(reinterpret_cast<char*>(&color.y), sizeof(float));
    file.read(reinterpret_cast<char*>(&color.z), sizeof(float));
    file.read(reinterpret_cast<char*>(&color.w), sizeof(float));

    if (hasNanColor) {
        result.nanColor = color;
    }

    return result;
}

void saveCachedFile(const ColorMap& colorMap, const std::filesystem::path& path) {
    std::ofstream file = std::ofstream(path, std::ofstream::binary);

    file.write(reinterpret_cast<const char*>(&ColorCacheFileVersion), sizeof(int8_t));

    uint32_t nColors = static_cast<uint32_t>(colorMap.entries.size());
    file.write(reinterpret_cast<const char*>(&nColors), sizeof(uint32_t));
    for (const glm::vec4& color : colorMap.entries) {
        file.write(reinterpret_cast<const char*>(&color.x), sizeof(float));
        file.write(reinterpret_cast<const char*>(&color.y), sizeof(float));
        file.write(reinterpret_cast<const char*>(&color.z), sizeof(float));
        file.write(reinterpret_cast<const char*>(&color.w), sizeof(float));
    }

    bool hasBelowColor = colorMap.belowRangeColor.has_value();
    const glm::vec4 belowColor = colorMap.belowRangeColor.value_or(glm::vec4(0.f));
    file.write(reinterpret_cast<const char*>(&hasBelowColor), sizeof(bool));
    file.write(reinterpret_cast<const char*>(&belowColor.x), sizeof(float));
    file.write(reinterpret_cast<const char*>(&belowColor.y), sizeof(float));
    file.write(reinterpret_cast<const char*>(&belowColor.z), sizeof(float));
    file.write(reinterpret_cast<const char*>(&belowColor.w), sizeof(float));

    bool hasAboveColor = colorMap.aboveRangeColor.has_value();
    const glm::vec4 aboveColor = colorMap.aboveRangeColor.value_or(glm::vec4(0.f));
    file.write(reinterpret_cast<const char*>(&hasAboveColor), sizeof(bool));
    file.write(reinterpret_cast<const char*>(&aboveColor.x), sizeof(float));
    file.write(reinterpret_cast<const char*>(&aboveColor.y), sizeof(float));
    file.write(reinterpret_cast<const char*>(&aboveColor.z), sizeof(float));
    file.write(reinterpret_cast<const char*>(&aboveColor.w), sizeof(float));

    bool hasNanColor = colorMap.nanColor.has_value();
    const glm::vec4 nanColor = colorMap.nanColor.value_or(glm::vec4(0.f));
    file.write(reinterpret_cast<const char*>(&hasNanColor), sizeof(bool));
    file.write(reinterpret_cast<const char*>(&nanColor.x), sizeof(float));
    file.write(reinterpret_cast<const char*>(&nanColor.y), sizeof(float));
    file.write(reinterpret_cast<const char*>(&nanColor.z), sizeof(float));
    file.write(reinterpret_cast<const char*>(&nanColor.w), sizeof(float));
}

ColorMap loadFileWithCache(std::filesystem::path path) {
    return internalLoadFileWithCache<ColorMap>(
        std::move(path),
        std::nullopt,
        &loadFile,
        &loadCachedFile,
        &saveCachedFile
    );
}

} // namespace color

bool Dataset::isEmpty() const {
    return variables.empty() || entries.empty();
}

int Dataset::index(std::string_view variableName) const {
    for (const Dataset::Variable& v : variables) {
        if (v.name == variableName) {
            return v.index;
        }
    }
    return -1;
}

bool Dataset::normalizeVariable(std::string_view variableName) {
    const int idx = index(variableName);

    if (idx == -1) {
        // We didn't find the variable that was specified
        return false;
    }

    float minValue = std::numeric_limits<float>::max();
    float maxValue = -std::numeric_limits<float>::max();
    for (Dataset::Entry& e : entries) {
        const float value = e.data[idx];
        if (std::isnan(value)) {
            continue;
        }
        minValue = std::min(minValue, value);
        maxValue = std::max(maxValue, value);
    }

    for (Dataset::Entry& e : entries) {
        const float value = e.data[idx];
        if (std::isnan(value)) {
            continue;
        }
        e.data[idx] = (value - minValue) / (maxValue - minValue);
    }

    return true;
}

glm::vec2 Dataset::findValueRange(int variableIndex) const {
    if (entries.empty()) {
        // Can't find range if there are no entries
        return glm::vec2(0.f);
    }

    if (variableIndex >= entries[0].data.size()) {
        // The index is not a valid variable index
        return glm::vec2(0.f);
    }

    float minValue = std::numeric_limits<float>::max();
    float maxValue = -std::numeric_limits<float>::max();
    for (const Dataset::Entry& e : entries) {
        if (!e.data.empty()) {
            const float value = e.data[variableIndex];
            if (std::isnan(value)) {
                continue;
            }
            minValue = std::min(value, minValue);
            maxValue = std::max(value, maxValue);
        }
        else {
            minValue = 0.f;
            maxValue = 0.f;
        }
    }

    return glm::vec2(minValue, maxValue);
}

glm::vec2 Dataset::findValueRange(std::string_view variableName) const {
    const int idx = index(variableName);

    if (idx == -1) {
        // We didn't find the variable that was specified
        return glm::vec2(0.f);
    }

    return findValueRange(idx);
}

} // namespace openspace::dataloader
