/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <ghoul/misc/assert.h>
#include <cctype>
#include <fstream>
#include <string_view>

namespace {
    constexpr bool startsWith(std::string_view lhs, std::string_view rhs) noexcept {
        return (rhs.size() <= lhs.size()) && (lhs.substr(0, rhs.size()) == rhs);
    }

    void strip(std::string& line) noexcept {
        // 1. Remove all spaces from the beginning
        // 2. Remove #
        // 3. Remove all spaces from the new beginning
        // 4. Remove all spaces from the end

        while (!line.empty() && line[0] == ' ') {
            line = line.substr(1);
        }

        if (!line.empty() && line[0] == '#') {
            line = line.substr(1);
        }

        while (!line.empty() && line[0] == ' ') {
            line = line.substr(1);
        }

        while (!line.empty() && line.back() == ' ') {
            line = line.substr(0, line.size() - 2);
        }
    }

    template <typename T, typename U>
    void checkSize(U value, std::string_view message) {
        if (value > std::numeric_limits<U>::max()) {
            throw ghoul::RuntimeError(fmt::format("Error saving file: {}", message));
        }
    }
} // namespace

namespace openspace::speck {

Dataset loadSpeckFile(std::filesystem::path path, SkipAllZeroLines skipAllZeroLines) {
    ghoul_assert(std::filesystem::exists(path), "File must exist");

    std::ifstream file(path);
    if (!file.good()) {
        throw ghoul::RuntimeError(fmt::format("Failed to open speck file '{}'", path));
    }

    Dataset res;

    int nDataValues = 0;

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


        if (startsWith(line, "datavar")) {
            // each datavar line is following the form:
            // datavar <idx> <description>
            // with <idx> being the index of the data variable 

            std::stringstream str(line);
            std::string dummy;
            Dataset::Variable v;
            str >> dummy >> v.index >> v.name;

            //if (v.name == "orientation" || v.name == "ori" || v.name == "texture") {
            //    // The values for orientation and the texture indices are handled by
            //    // their own keywords
            //}
            //else {
                nDataValues += 1;
                res.variables.push_back(v);
            //}

            continue;
        }

        if (startsWith(line, "texturevar")) {
            // each texturevar line is following the form:
            // texturevar <idx>
            // where <idx> is the data value index where the texture index is stored
            if (res.textureDataIndex != -1) {
                throw ghoul::RuntimeError(fmt::format(
                    "Error loading speck file '{}': Texturevar defined twice",
                    path
                ));
            }

            std::stringstream str(line);
            std::string dummy;
            str >> dummy >> res.textureDataIndex;
            
            //nDataValues += 1;
            continue;
        }

        if (startsWith(line, "polyorivar")) {
            // each texturevar line is following the form:
            // texturevar <idx>
            // where <idx> is the data value index where the orientation index storage
            // starts. There are 6 values stored in total, xyz + uvw

            if (res.orientationDataIndex != -1) {
                throw ghoul::RuntimeError(fmt::format(
                    "Error loading speck file '{}': Orientation index defined twice",
                    path
                ));
            }

            std::stringstream str(line);
            std::string dummy;
            str >> dummy >> res.orientationDataIndex;
            
            //nDataValues += 6;
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
                "Error loading speck file '{}': Header information and datasegment "
                "intermixed", path
            ));
        }

        bool allZero = true;

        std::stringstream str(line);
        Dataset::Entry entry;
        str >> entry.position.x >> entry.position.y >> entry.position.z;
        allZero &= (entry.position == glm::dvec3(0.0));

        entry.data.resize(nDataValues);
        for (int i = 0; i < nDataValues; i += 1) {
            str >> entry.data[i];
            allZero &= (entry.data[i] == 0.0);
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
        size_t nDataValues = res.entries[0].data.size();
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

Dataset loadCachedFile(std::filesystem::path path) {
    Dataset result;

    std::ifstream file(path, std::ios::binary);
    if (!file.good()) {
        throw ghoul::RuntimeError(fmt::format("Error opening cache file '{}', path"));
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
    result.entries.resize(nEntries);
    for (int i = 0; i < nEntries; i += 1) {
        Dataset::Entry e;
        file.read(reinterpret_cast<char*>(&e.position.x), sizeof(double));
        file.read(reinterpret_cast<char*>(&e.position.y), sizeof(double));
        file.read(reinterpret_cast<char*>(&e.position.z), sizeof(double));

        uint16_t nValues;
        file.read(reinterpret_cast<char*>(&nValues), sizeof(uint16_t));
        e.data.resize(nValues);
        file.read(reinterpret_cast<char*>(e.data.data()), nValues * sizeof(double));

        uint16_t len;
        file.read(reinterpret_cast<char*>(&len), sizeof(uint16_t));
        if (len > 0) {
            std::string comment;
            comment.resize(len);
            file.read(comment.data(), len);
            e.comment = std::move(comment);
        }

        result.entries[i] = std::move(e);
    }

    return result;
}

void saveCachedFile(const Dataset& dataset, std::filesystem::path path) {
    std::ofstream file(path, std::ofstream::binary);

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
        file.write(reinterpret_cast<const char*>(&e.position.x), sizeof(double));
        file.write(reinterpret_cast<const char*>(&e.position.y), sizeof(double));
        file.write(reinterpret_cast<const char*>(&e.position.z), sizeof(double));

        checkSize<uint16_t>(e.data.size(), "Too many data variables");
        uint16_t nValues = static_cast<uint16_t>(e.data.size());
        file.write(reinterpret_cast<const char*>(&nValues), sizeof(uint16_t));
        file.write(
            reinterpret_cast<const char*>(e.data.data()),
            e.data.size() * sizeof(double)
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

bool normalizeVariable(Dataset& dataset, std::string_view variableName) {
    std::optional<int> idx;
    for (const Dataset::Variable& var : dataset.variables) {
        if (var.name == variableName) {
            idx = var.index;
            break;
        }
    }

    if (!idx.has_value()) {
        // We didn't find the variable that was specified
        return false;
    }

    double minValue = std::numeric_limits<double>::max();
    double maxValue = -std::numeric_limits<double>::max();
    for (Dataset::Entry& e : dataset.entries) {
        minValue = std::min(minValue, e.data[*idx]);
        maxValue = std::max(maxValue, e.data[*idx]);
    }

    for (Dataset::Entry& e : dataset.entries) {
        e.data[*idx] = (e.data[*idx] - minValue) / (maxValue - minValue);
    }

    return true;
}

} // namespace openspace::speck
