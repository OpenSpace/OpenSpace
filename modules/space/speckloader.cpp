/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/space/speckloader.h>

#include <ghoul/fmt.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <cctype>
#include <fstream>
#include <functional>
#include <string_view>

namespace {
    constexpr int8_t DataCacheFileVersion = 10;
    constexpr int8_t LabelCacheFileVersion = 11;
    constexpr int8_t ColorCacheFileVersion = 10;

    bool startsWith(std::string lhs, std::string_view rhs) noexcept {
        for (size_t i = 0; i < lhs.size(); i++) {
            lhs[i] = static_cast<char>(tolower(lhs[i]));
        }
        return (rhs.size() <= lhs.size()) && (lhs.substr(0, rhs.size()) == rhs);
    }

    void strip(std::string& line) noexcept {
        // 1. Remove all spaces from the beginning
        // 2. Remove #
        // 3. Remove all spaces from the new beginning
        // 4. Remove all spaces from the end

        while (!line.empty() && (line[0] == ' ' || line[0] == '\t')) {
            line = line.substr(1);
        }

        if (!line.empty() && line[0] == '#') {
            line = line.substr(1);
        }

        while (!line.empty() && (line[0] == ' ' || line[0] == '\t')) {
            line = line.substr(1);
        }

        while (!line.empty() && (line.back() == ' ' || line.back() == '\t')) {
            line = line.substr(0, line.size() - 1);
        }
    }

    template <typename T, typename U>
    void checkSize(U value, std::string_view message) {
        if (value > std::numeric_limits<U>::max()) {
            throw ghoul::RuntimeError(fmt::format("Error saving file: {}", message));
        }
    }

    template <typename T>
    using LoadCacheFunc = std::function<std::optional<T>(std::filesystem::path)>;

    template <typename T>
    using SaveCacheFunc = std::function<void(const T&, std::filesystem::path)>;

    template <typename T>
    using LoadSpeckFunc = std::function<T(
        std::filesystem::path, openspace::speck::SkipAllZeroLines)>;



    template <typename T>
    T internalLoadFileWithCache(std::filesystem::path speckPath,
                                     openspace::speck::SkipAllZeroLines skipAllZeroLines,
                                     LoadSpeckFunc<T> loadSpeckFunction,
                                     LoadCacheFunc<T> loadCacheFunction,
                                     SaveCacheFunc<T> saveCacheFunction)
    {
        static_assert(
            std::is_same_v<T, openspace::speck::Dataset> ||
            std::is_same_v<T, openspace::speck::Labelset> ||
            std::is_same_v<T, openspace::speck::ColorMap>
        );

        std::filesystem::path cached = FileSys.cacheManager()->cachedFilename(speckPath);

        if (std::filesystem::exists(cached)) {
            LINFOC(
                "SpeckLoader",
                fmt::format("Cached file {} used for file {}", cached, speckPath)
            );

            std::optional<T> dataset = loadCacheFunction(cached);
            if (dataset.has_value()) {
                // We could load the cache file and we are now done with this
                return *dataset;
            }
            else {
                FileSys.cacheManager()->removeCacheFile(cached);
            }
        }
        LINFOC("SpeckLoader", fmt::format("Loading file {}", speckPath));
        T dataset = loadSpeckFunction(speckPath, skipAllZeroLines);

        if (!dataset.entries.empty()) {
            LINFOC("SpeckLoader", "Saving cache");
            saveCacheFunction(dataset, cached);
        }
        return dataset;
    }
} // namespace

namespace openspace::speck {

namespace data {

Dataset loadFile(std::filesystem::path path, SkipAllZeroLines skipAllZeroLines) {
    ghoul_assert(std::filesystem::exists(path), "File must exist");

    std::ifstream file(path);
    if (!file.good()) {
        throw ghoul::RuntimeError(fmt::format("Failed to open speck file {}", path));
    }

    Dataset res;

    int nDataValues = 0;
    int currentLineNumber = 0;

    std::string line;
    // First phase: Loading the header information
    while (std::getline(file, line)) {
        currentLineNumber++;

        // Ignore empty line or commented-out lines
        if (line.empty() || line[0] == '#') {
            continue;
        }

        // Guard against wrong line endings (copying files from Windows to Mac) causes
        // lines to have a final \r
        if (line.back() == '\r') {
            line = line.substr(0, line.length() - 1);
        }

        strip(line);

        // If the first character is a digit, we have left the preamble and are in the
        // data section of the file
        if (std::isdigit(line[0]) || line[0] == '-') {
            break;
        }


        if (startsWith(line, "datavar")) {
            // each datavar line is following the form:
            // datavar <idx> <description>
            // with <idx> being the index of the data variable

            std::stringstream str(line);
            std::string dummy;
            Dataset::Variable v;
            str >> dummy >> v.index >> v.name;

            nDataValues += 1;
            res.variables.push_back(v);
            continue;
        }

        if (startsWith(line, "texturevar")) {
            // each texturevar line is following the form:
            // texturevar <idx>
            // where <idx> is the data value index where the texture index is stored
            if (res.textureDataIndex != -1) {
                throw ghoul::RuntimeError(fmt::format(
                    "Error loading speck file {}: Texturevar defined twice", path
                ));
            }

            std::stringstream str(line);
            std::string dummy;
            str >> dummy >> res.textureDataIndex;

            continue;
        }

        if (startsWith(line, "polyorivar")) {
            // each polyorivar line is following the form:
            // texturevar <idx>
            // where <idx> is the data value index where the orientation index storage
            // starts. There are 6 values stored in total, xyz + uvw

            if (res.orientationDataIndex != -1) {
                throw ghoul::RuntimeError(fmt::format(
                    "Error loading speck file {}: Orientation index defined twice", path
                ));
            }

            std::stringstream str(line);
            std::string dummy;
            str >> dummy >> res.orientationDataIndex;

            // Ok.. this is kind of weird.  Speck unfortunately doesn't tell us in the
            // specification how many values a datavar has. Usually this is 1 value per
            // datavar, unless it is a polygon orientation thing. Now, the datavar name
            // for these can be anything (have seen 'orientation' and 'ori' before, so we
            // can't really check by name for these or we will miss some if they are
            // mispelled or whatever. So we have to go the roundabout way of adding the
            // 5 remaining values (the 6th nDataValue was already added in the
            // corresponding 'datavar' section) here
            nDataValues += 5;

            continue;
        }

        if (startsWith(line, "texture")) {
            // each texture line is following one of two forms:
            // 1:   texture -M 1 halo.sgi
            // 2:   texture 1 M1.sgi
            // The parameter in #1 is currently being ignored

            std::stringstream str(line);

            std::string dummy;
            str >> dummy;

            if (line.find('-') != std::string::npos) {
                str >> dummy;
            }

            Dataset::Texture texture;
            str >> texture.index >> texture.file;

            for (const Dataset::Texture& t : res.textures) {
                if (t.index == texture.index) {
                    throw ghoul::RuntimeError(fmt::format(
                        "Error loading speck file {}: Texture index '{}' defined twice",
                        path, texture.index
                    ));
                }
            }

            res.textures.push_back(texture);
            continue;
        }
    }

    std::sort(
        res.variables.begin(), res.variables.end(),
        [](const Dataset::Variable& lhs, const Dataset::Variable& rhs) {
            return lhs.index < rhs.index;
        }
    );

    std::sort(
        res.textures.begin(), res.textures.end(),
        [](const Dataset::Texture& lhs, const Dataset::Texture& rhs) {
            return lhs.index < rhs.index;
        }
    );

    // For the first line, we already loaded it and rejected it above, so if we do another
    // std::getline, we'd miss the first data value line
    bool isFirst = true;
    while (isFirst || std::getline(file, line)) {
        currentLineNumber++;
        isFirst = false;

        // Ignore empty line or commented-out lines
        if (line.empty() || line[0] == '#') {
            continue;
        }

        // Guard against wrong line endings (copying files from Windows to Mac) causes
        // lines to have a final \r
        if (line.back() == '\r') {
            line = line.substr(0, line.length() - 1);
        }

        strip(line);

        if (line.empty()) {
            continue;
        }

        // If the first character is a digit, we have left the preamble and are in the
        // data section of the file
        if (!std::isdigit(line[0]) && line[0] != '-') {
            throw ghoul::RuntimeError(fmt::format(
                "Error loading speck file {}: Header information and datasegment "
                "intermixed", path
            ));
        }

        bool allZero = true;

        std::stringstream str(line);
        Dataset::Entry entry;
        str >> entry.position.x >> entry.position.y >> entry.position.z;
        allZero &= (entry.position == glm::vec3(0.0));

        if (!str.good()) {
            // Need to subtract one of the line number here as we increase the current
            // line count in the beginning of the while loop we are currently in
            throw ghoul::RuntimeError(fmt::format(
                "Error loading position information out of data line {} in file {}. "
                "Value was not a number",
                currentLineNumber - 1, path
            ));
        }

        entry.data.resize(nDataValues);
        std::stringstream valueStream;
        for (int i = 0; i < nDataValues; i += 1) {
            std::string value;
            str >> value;
            if (value == "nan" || value == "NaN") {
                entry.data[i] = std::numeric_limits<float>::quiet_NaN();
            }
            else {
                valueStream.clear();
                valueStream.str(value);
                valueStream >> entry.data[i];

                allZero &= (entry.data[i] == 0.0);
                if (valueStream.fail()) {
                    // Need to subtract one of the line number here as we increase the
                    // current line count in the beginning of the while loop we are
                    // currently in
                    throw ghoul::RuntimeError(fmt::format(
                        "Error loading data value {} out of data line {} in file {}. "
                        "Value was not a number",
                        i, currentLineNumber - 1, path
                    ));
                }
            }
        }

        if (skipAllZeroLines && allZero) {
            continue;
        }

        std::string rest;
        std::getline(str, rest);
        if (!rest.empty()) {

            strip(rest);
            entry.comment = rest;
        }

        res.entries.push_back(std::move(entry));
    }

#ifdef _DEBUG
    if (!res.entries.empty()) {
        size_t nValues = res.entries[0].data.size();
        ghoul_assert(nDataValues == nValues, "nDataValues calculation went wrong");
        for (const Dataset::Entry& e : res.entries) {
            ghoul_assert(
                e.data.size() == nDataValues,
                "Row had different number of data values"
            );
        }
    }
#endif

    return res;
}

std::optional<Dataset> loadCachedFile(std::filesystem::path path) {
    std::ifstream file(path, std::ios::binary);
    if (!file.good()) {
        return std::nullopt;
    }

    Dataset result;

    int8_t fileVersion;
    file.read(reinterpret_cast<char*>(&fileVersion), sizeof(int8_t));
    if (fileVersion != DataCacheFileVersion) {
        // Incompatible version and we won't be able to read the file
        return std::nullopt;
    }

    //
    // Read variables
    uint16_t nVariables;
    file.read(reinterpret_cast<char*>(&nVariables), sizeof(uint16_t));
    result.variables.resize(nVariables);
    for (int i = 0; i < nVariables; i += 1) {
        Dataset::Variable var;

        int16_t idx;
        file.read(reinterpret_cast<char*>(&idx), sizeof(int16_t));
        var.index = idx;

        uint16_t len;
        file.read(reinterpret_cast<char*>(&len), sizeof(uint16_t));
        var.name.resize(len);
        file.read(var.name.data(), len);

        result.variables[i] = std::move(var);
    }

    //
    // Read textures
    uint16_t nTextures;
    file.read(reinterpret_cast<char*>(&nTextures), sizeof(uint16_t));
    result.textures.resize(nTextures);
    for (int i = 0; i < nTextures; i += 1) {
        Dataset::Texture tex;

        int16_t idx;
        file.read(reinterpret_cast<char*>(&idx), sizeof(int16_t));
        tex.index = idx;

        uint16_t len;
        file.read(reinterpret_cast<char*>(&len), sizeof(uint16_t));
        tex.file.resize(len);
        file.read(tex.file.data(), len);

        result.textures[i] = std::move(tex);
    }

    //
    // Read indices
    int16_t texDataIdx;
    file.read(reinterpret_cast<char*>(&texDataIdx), sizeof(int16_t));
    result.textureDataIndex = texDataIdx;

    int16_t oriDataIdx;
    file.read(reinterpret_cast<char*>(&oriDataIdx), sizeof(int16_t));
    result.orientationDataIndex = oriDataIdx;

    //
    // Read entries
    uint64_t nEntries;
    file.read(reinterpret_cast<char*>(&nEntries), sizeof(uint64_t));
    result.entries.reserve(nEntries);
    for (uint64_t i = 0; i < nEntries; i += 1) {
        Dataset::Entry e;
        file.read(reinterpret_cast<char*>(&e.position.x), sizeof(float));
        file.read(reinterpret_cast<char*>(&e.position.y), sizeof(float));
        file.read(reinterpret_cast<char*>(&e.position.z), sizeof(float));

        uint16_t nValues;
        file.read(reinterpret_cast<char*>(&nValues), sizeof(uint16_t));
        e.data.resize(nValues);
        file.read(reinterpret_cast<char*>(e.data.data()), nValues * sizeof(float));

        uint16_t len;
        file.read(reinterpret_cast<char*>(&len), sizeof(uint16_t));
        if (len > 0) {
            std::string comment;
            comment.resize(len);
            file.read(comment.data(), len);
            e.comment = std::move(comment);
        }

        result.entries.push_back(std::move(e));
    }

    return result;
}

void saveCachedFile(const Dataset& dataset, std::filesystem::path path) {
    std::ofstream file(path, std::ofstream::binary);

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
    for (const Dataset::Entry& e : dataset.entries) {
        file.write(reinterpret_cast<const char*>(&e.position.x), sizeof(float));
        file.write(reinterpret_cast<const char*>(&e.position.y), sizeof(float));
        file.write(reinterpret_cast<const char*>(&e.position.z), sizeof(float));

        checkSize<uint16_t>(e.data.size(), "Too many data variables");
        uint16_t nValues = static_cast<uint16_t>(e.data.size());
        file.write(reinterpret_cast<const char*>(&nValues), sizeof(uint16_t));
        file.write(
            reinterpret_cast<const char*>(e.data.data()),
            e.data.size() * sizeof(float)
        );

        if (e.comment.has_value()) {
            checkSize<uint16_t>(e.comment->size(), "Comment too long");
        }
        uint16_t commentLen = e.comment.has_value() ?
            static_cast<uint16_t>(e.comment->size()) :
            0;
        file.write(reinterpret_cast<const char*>(&commentLen), sizeof(uint16_t));
        if (e.comment.has_value()) {
            file.write(e.comment->data(), e.comment->size());
        }
    }
}

Dataset loadFileWithCache(std::filesystem::path speckPath,
                               SkipAllZeroLines skipAllZeroLines)
{
    return internalLoadFileWithCache<Dataset>(
        speckPath,
        skipAllZeroLines,
        &loadFile,
        &loadCachedFile,
        &saveCachedFile
    );
}

} // namespace data

namespace label {

Labelset loadFile(std::filesystem::path path, SkipAllZeroLines) {
    ghoul_assert(std::filesystem::exists(path), "File must exist");

    std::ifstream file(path);
    if (!file.good()) {
        throw ghoul::RuntimeError(fmt::format("Failed to open speck file {}", path));
    }

    Labelset res;

    std::string line;
    // First phase: Loading the header information
    while (std::getline(file, line)) {
        // Ignore empty line or commented-out lines
        if (line.empty() || line[0] == '#') {
            continue;
        }

        // Guard against wrong line endings (copying files from Windows to Mac) causes
        // lines to have a final \r
        if (line.back() == '\r') {
            line = line.substr(0, line.length() - 1);
        }

        strip(line);

        // If the first character is a digit, we have left the preamble and are in the
        // data section of the file
        if (std::isdigit(line[0]) || line[0] == '-') {
            break;
        }

        if (startsWith(line, "textcolor")) {
            // each textcolor line is following the form:
            // textcolor <idx>
            // with <idx> being the index of the color into some configuration file (not
            // really sure how these configuration files work, but they don't seem to be
            // included in the speck file)
            if (res.textColorIndex != -1) {
                throw ghoul::RuntimeError(fmt::format(
                    "Error loading label file {}: Textcolor defined twice", path
                ));
            }


            std::stringstream str(line);
            std::string dummy;
            str >> dummy >> res.textColorIndex;
            continue;
        }
    }

    // For the first line, we already loaded it and rejected it above, so if we do another
    // std::getline, we'd miss the first data value line
    bool isFirst = true;
    while (isFirst || std::getline(file, line)) {
        isFirst = false;

        // Ignore empty line or commented-out lines
        if (line.empty() || line[0] == '#') {
            continue;
        }

        // Guard against wrong line endings (copying files from Windows to Mac) causes
        // lines to have a final \r
        if (line.back() == '\r') {
            line = line.substr(0, line.length() - 1);
        }

        strip(line);

        if (line.empty()) {
            continue;
        }

        // If the first character is a digit, we have left the preamble and are in the
        // data section of the file
        if (!std::isdigit(line[0]) && line[0] != '-') {
            throw ghoul::RuntimeError(fmt::format(
                "Error loading label file {}: Header information and datasegment "
                "intermixed", path
            ));
        }

        // Each line looks like this:
        // <x> <y> <z> text <label> # potential comment
        // so we want to get the position, remove the 'text' text and the potential
        // comment at the end
        std::stringstream str(line);
        Labelset::Entry entry;
        str >> entry.position.x >> entry.position.y >> entry.position.z;

        std::string rest;
        std::getline(str, rest);
        strip(rest);

        if (startsWith(rest, "id")) {
            // optional arument with identifier
            // Remove the 'id' text
            rest = rest.substr(std::string_view("id ").size());
            size_t index = rest.find("text");
            entry.identifier = rest.substr(0, index - 1);

            // update the rest, remove the identifier
            rest = rest.substr(index);
        }
        if (!startsWith(rest, "text")) {
            throw ghoul::RuntimeError(fmt::format(
                "Error loading label file {}: File contains an unsupported value "
                "between positions and text label", path
            ));
        }

        // Remove the 'text' text
        rest = rest.substr(std::string_view("text ").size());

        // Remove the trailing comment
        for (size_t i = 0; i < rest.size(); i += 1) {
            if (rest[i] == '#') {
                rest = rest.substr(0, i);
                break;
            }
        }

        strip(rest);

        entry.text = rest;
        if (!rest.empty()) {
            res.entries.push_back(std::move(entry));
        }
    }

    return res;
}

std::optional<Labelset> loadCachedFile(std::filesystem::path path) {
    std::ifstream file(path, std::ios::binary);
    if (!file.good()) {
        return std::nullopt;
    }

    int8_t fileVersion;
    file.read(reinterpret_cast<char*>(&fileVersion), sizeof(int8_t));
    if (fileVersion != LabelCacheFileVersion) {
        // Incompatible version and we won't be able to read the file
        return std::nullopt;
    }

    Labelset result;

    int16_t textColorIdx;
    file.read(reinterpret_cast<char*>(&textColorIdx), sizeof(int16_t));
    result.textColorIndex = textColorIdx;

    uint32_t nEntries;
    file.read(reinterpret_cast<char*>(&nEntries), sizeof(uint32_t));
    result.entries.reserve(nEntries);
    for (unsigned int i = 0; i < nEntries; i += 1) {
        Labelset::Entry e;
        file.read(reinterpret_cast<char*>(&e.position.x), sizeof(float));
        file.read(reinterpret_cast<char*>(&e.position.y), sizeof(float));
        file.read(reinterpret_cast<char*>(&e.position.z), sizeof(float));

        // Identifier
        uint8_t idLen;
        file.read(reinterpret_cast<char*>(&idLen), sizeof(uint8_t));
        e.identifier.resize(idLen);
        file.read(e.identifier.data(), idLen);

        // Text
        uint16_t len;
        file.read(reinterpret_cast<char*>(&len), sizeof(uint16_t));
        e.text.resize(len);
        file.read(e.text.data(), len);

        result.entries.push_back(e);
    }

    return result;
}

void saveCachedFile(const Labelset& labelset, std::filesystem::path path) {
    std::ofstream file(path, std::ofstream::binary);

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

Labelset loadFileWithCache(std::filesystem::path speckPath,
                                SkipAllZeroLines skipAllZeroLines)
{
    return internalLoadFileWithCache<Labelset>(
        speckPath,
        skipAllZeroLines,
        &loadFile,
        &loadCachedFile,
        &saveCachedFile
    );
}

} // namespace label

namespace color {

ColorMap loadFile(std::filesystem::path path, SkipAllZeroLines) {
    ghoul_assert(std::filesystem::exists(path), "File must exist");

    std::ifstream file(path);
    if (!file.good()) {
        throw ghoul::RuntimeError(fmt::format("Failed to open speck file {}", path));
    }

    ColorMap res;
    int nColorLines = -1;

    // The beginning of the speck file has a header that either contains comments
    // (signaled by a preceding '#') or information about the structure of the file
    // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
    std::string line;
    while (std::getline(file, line)) {
        // Ignore empty line or commented-out lines
        if (line.empty() || line[0] == '#') {
            continue;
        }

        // Guard against wrong line endings (copying files from Windows to Mac) causes
        // lines to have a final \r
        if (line.back() == '\r') {
            line = line.substr(0, line.length() - 1);
        }

        strip(line);

        std::stringstream str(line);
        if (nColorLines == -1) {
            // This is the first time we get this far, it will have to be the first number
            // meaning that it is the number of color values

            str >> nColorLines;
            res.entries.reserve(nColorLines);
        }
        else {
            // We have already read the number of color lines, so we are in the process of
            // reading the individual value lines

            glm::vec4 color;
            str >> color.x >> color.y >> color.z >> color.w;
            res.entries.push_back(std::move(color));
        }
    }

    if (nColorLines != static_cast<int>(res.entries.size())) {
        LWARNINGC("SpeckLoader", fmt::format(
            "While loading color map, the expected number of color values '{}' was "
            "different from the actual number of color values '{}'",
            nColorLines, res.entries.size()
        ));
    }

    return res;
}

std::optional<ColorMap> loadCachedFile(std::filesystem::path path) {
    std::ifstream file(path, std::ios::binary);
    if (!file.good()) {
        return std::nullopt;
    }

    int8_t fileVersion;
    file.read(reinterpret_cast<char*>(&fileVersion), sizeof(int8_t));
    if (fileVersion != ColorCacheFileVersion) {
        // Incompatible version and we won't be able to read the file
        return std::nullopt;
    }

    ColorMap result;

    uint32_t nColors;
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
    return result;
}

void saveCachedFile(const ColorMap& colorMap, std::filesystem::path path) {
    std::ofstream file(path, std::ofstream::binary);

    file.write(reinterpret_cast<const char*>(&ColorCacheFileVersion), sizeof(int8_t));

    uint32_t nColors = static_cast<uint32_t>(colorMap.entries.size());
    file.write(reinterpret_cast<const char*>(&nColors), sizeof(uint32_t));
    for (const glm::vec4& color : colorMap.entries) {
        file.write(reinterpret_cast<const char*>(&color.x), sizeof(float));
        file.write(reinterpret_cast<const char*>(&color.y), sizeof(float));
        file.write(reinterpret_cast<const char*>(&color.z), sizeof(float));
        file.write(reinterpret_cast<const char*>(&color.w), sizeof(float));
    }
}

ColorMap loadFileWithCache(std::filesystem::path path, SkipAllZeroLines skipAllZeroLines)
{
    return internalLoadFileWithCache<ColorMap>(
        path,
        skipAllZeroLines,
        &loadFile,
        &loadCachedFile,
        &saveCachedFile
    );
}

} // namespace color

int Dataset::index(std::string_view variableName) const {
    for (const Dataset::Variable& v : variables) {
        if (v.name == variableName) {
            return v.index;
        }
    }
    return -1;
}

bool Dataset::normalizeVariable(std::string_view variableName) {
    std::optional<int> idx;
    for (const Dataset::Variable& var : variables) {
        if (var.name == variableName) {
            idx = var.index;
            break;
        }
    }

    if (!idx.has_value()) {
        // We didn't find the variable that was specified
        return false;
    }

    float minValue = std::numeric_limits<float>::max();
    float maxValue = -std::numeric_limits<float>::max();
    for (Dataset::Entry& e : entries) {
        minValue = std::min(minValue, e.data[*idx]);
        maxValue = std::max(maxValue, e.data[*idx]);
    }

    for (Dataset::Entry& e : entries) {
        e.data[*idx] = (e.data[*idx] - minValue) / (maxValue - minValue);
    }

    return true;
}

} // namespace openspace::speck
