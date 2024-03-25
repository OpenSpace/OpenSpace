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

#include <openspace/data/speckloader.h>

#include <ghoul/fmt.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/stringhelper.h>
#include <cctype>
#include <fstream>
#include <functional>
#include <sstream>
#include <string_view>


namespace {
    bool startsWith(std::string lhs, std::string_view rhs) noexcept {
        lhs = ghoul::toLowerCase(lhs);
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

} // namespace

namespace openspace::dataloader::speck {

Dataset loadSpeckFile(std::filesystem::path path, std::optional<DataMapping> specs) {
    ghoul_assert(std::filesystem::exists(path), "File must exist");

    std::ifstream file(path);
    if (!file.good()) {
        throw ghoul::RuntimeError(std::format("Failed to open speck file '{}'", path));
    }

    Dataset res;

    int nDataValues = 0;
    int currentLineNumber = 0;

    std::string line;
    // First phase: Loading the header information
    while (std::getline(file, line)) {
        currentLineNumber++;

        // Guard against wrong line endings (copying files from Windows to Mac) causes
        // lines to have a final \r
        if (!line.empty() && line.back() == '\r') {
            line = line.substr(0, line.length() - 1);
        }

        // Ignore empty line or commented-out lines
        if (line.empty() || line[0] == '#') {
            continue;
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
                throw ghoul::RuntimeError(std::format(
                    "Error loading speck file '{}': Texturevar defined twice", path
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
                throw ghoul::RuntimeError(std::format(
                    "Error loading speck file '{}': Orientation index defined twice", path
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

            std::vector<std::string> tokens = ghoul::tokenizeString(line, ' ');
            int nNonEmptyTokens = static_cast<int>(std::count_if(
                tokens.begin(),
                tokens.end(),
                [](const std::string& t) { return !t.empty(); }
            ));

            if (nNonEmptyTokens > 4) {
                throw ghoul::RuntimeError(std::format(
                    "Error loading speck file {}: Too many arguments for texture on line {}",
                    path, currentLineNumber
                ));
            }

            bool hasExtraParameter = nNonEmptyTokens > 3;

            std::stringstream str(line);

            std::string dummy;
            str >> dummy;
            if (hasExtraParameter) {
                str >> dummy;
            }

            Dataset::Texture texture;
            str >> texture.index >> texture.file;

            for (const Dataset::Texture& t : res.textures) {
                if (t.index == texture.index) {
                    throw ghoul::RuntimeError(std::format(
                        "Error loading speck file '{}': Texture index '{}' defined twice",
                        path, texture.index
                    ));
                }
            }

            res.textures.push_back(texture);
            continue;
        }

        if (startsWith(line, "maxcomment")) {
            // ignoring this comment as we don't need it
            continue;
        }

        // If we get this far, we had an illegal header as it wasn't an empty line and
        // didn't start with either '#' denoting a comment line, and didn't start with
        // either the 'datavar', 'texturevar', 'polyorivar', or 'texture' keywords
        throw ghoul::RuntimeError(std::format(
            "Error in line {} while reading the header information of file '{}'. Line is "
            "neither a comment line, nor starts with one of the supported keywords for "
            "SPECK files",
            currentLineNumber, path
        ));
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
            throw ghoul::RuntimeError(std::format(
                "Error loading speck file '{}': Header information and datasegment "
                "intermixed", path
            ));
        }

        bool allZero = true;

        // For SPECK we know that the first 3 values are the position, so no need to
        // check agains data mapping
        std::stringstream str(line);
        Dataset::Entry entry;
        str >> entry.position.x >> entry.position.y >> entry.position.z;
        allZero &= (entry.position == glm::vec3(0.0));

        const glm::vec3 positive = glm::abs(entry.position);
        const float max = glm::compMax(positive);
        if (max > res.maxPositionComponent) {
            res.maxPositionComponent = max;
        }

        if (!str.good()) {
            // Need to subtract one of the line number here as we increase the current
            // line count in the beginning of the while loop we are currently in
            throw ghoul::RuntimeError(std::format(
                "Error loading position information out of data line {} in file '{}'. "
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

                // Check if value corresponds to a missing value
                if (specs.has_value() && specs->missingDataValue.has_value()) {
                    const float missingDataValue = specs->missingDataValue.value();
                    const float diff = std::abs(entry.data[i] - missingDataValue);
                    if (diff < std::numeric_limits<float>::epsilon()) {
                        entry.data[i] = std::numeric_limits<float>::quiet_NaN();
                    }
                }

                allZero &= (entry.data[i] == 0.0);
                if (valueStream.fail()) {
                    // Need to subtract one of the line number here as we increase the
                    // current line count in the beginning of the while loop we are
                    // currently in
                    throw ghoul::RuntimeError(std::format(
                        "Error loading data value {} out of data line {} in file '{}'. "
                        "Value was not a number",
                        i, currentLineNumber - 1, path
                    ));
                }
            }
        }

        if (allZero) {
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

Labelset loadLabelFile(std::filesystem::path path) {
    ghoul_assert(std::filesystem::exists(path), "File must exist");

    std::ifstream file(path);
    if (!file.good()) {
        throw ghoul::RuntimeError(std::format("Failed to open dataset file '{}'", path));
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
                throw ghoul::RuntimeError(std::format(
                    "Error loading label file '{}': Textcolor defined twice", path
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
            throw ghoul::RuntimeError(std::format(
                "Error loading label file '{}': Header information and datasegment "
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
            const size_t index = rest.find("text");
            entry.identifier = rest.substr(0, index - 1);

            // update the rest, remove the identifier
            rest = rest.substr(index);
        }
        if (!startsWith(rest, "text")) {
            throw ghoul::RuntimeError(std::format(
                "Error loading label file '{}': File contains an unsupported value "
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

ColorMap loadCmapFile(std::filesystem::path path) {
    ghoul_assert(std::filesystem::exists(path), "File must exist");

    std::ifstream file = std::ifstream(path);
    if (!file.good()) {
        throw ghoul::RuntimeError(std::format(
            "Failed to open color map file '{}'", path
        ));
    }

    ColorMap res;
    int nColorLines = -1;

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

        if (nColorLines == -1) {
            // This is the first time we get this far, it will have to be the first number
            // meaning that it is the number of color values

            std::stringstream str(line);
            str >> nColorLines;
            res.entries.reserve(nColorLines);
        }
        else {
            // We have already read the number of color lines, so we are in the process of
            // reading the individual value lines

            glm::vec4 color;
            std::string dummy;
            // Note that startwith converts the input string to all lowercase
            if (startsWith(line, "belowrange")) {
                std::stringstream str(line);
                str >> dummy >> color.x >> color.y >> color.z >> color.w;
                res.belowRangeColor = color;
            }
            else if (startsWith(line, "aboverange")) {
                std::stringstream str(line);
                str >> dummy >> color.x >> color.y >> color.z >> color.w;
                res.aboveRangeColor = color;
            }
            else if (startsWith(line, "nan")) {
                std::stringstream str(line);
                str >> dummy >> color.x >> color.y >> color.z >> color.w;
                res.nanColor = color;
            }
            else {
                // TODO: Catch when this is not a color!
                std::stringstream str(line);
                str >> color.x >> color.y >> color.z >> color.w;
                res.entries.push_back(std::move(color));
            }
        }
    }

    res.entries.shrink_to_fit();

    if (nColorLines != static_cast<int>(res.entries.size())) {
        LWARNINGC("SpeckLoader", std::format(
            "While loading color map '{}', the expected number of color values '{}' was "
            "different from the actual number of color values '{}'",
            path, nColorLines, res.entries.size()
        ));
    }

    return res;
}

} // namespace openspace::dataloader::speck
