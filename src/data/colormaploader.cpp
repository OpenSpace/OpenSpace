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

#include <openspace/data/colormaploader.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/stringhelper.h>
#include <format>
#include <fstream>
#include <sstream>

namespace {
    constexpr std::string_view _loggerCat = "ColorMapLoader";

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

namespace openspace::dataloader::colormap {

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
    while (ghoul::getline(file, line)) {
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
        LWARNING(std::format(
            "While loading color map '{}', the expected number of color values '{}' "
            "was different from the actual number of color values '{}'",
            path, nColorLines, res.entries.size()
        ));
    }

    return res;
}

std::unique_ptr<ghoul::opengl::Texture>
loadColorMapTexture(const std::filesystem::path& filename,
                    ghoul::opengl::Texture::SamplerInit samplerSettings)
{
    std::string extension = std::filesystem::path(filename).extension().string();
    if (!extension.empty()) {
        extension = extension.substr(1);
    }
    ghoul_assert(!extension.empty(), "Filename must have an extension");
    extension = ghoul::toLowerCase(extension);

    if (extension == "cmap") {
        const ColorMap colorMap = loadCmapFile(filename);
        return std::make_unique<ghoul::opengl::Texture>(
            ghoul::opengl::Texture::FormatInit{
                .dimensions = glm::uvec3(static_cast<unsigned int>(colorMap.entries.size()), 1, 1),
                .type = GL_TEXTURE_1D,
                .format = ghoul::opengl::Texture::Format::RGBA,
                .dataType = GL_FLOAT
            },
            samplerSettings,
            reinterpret_cast<const std::byte*>(colorMap.entries.data())
        );
        // @TODO (emmbr, 2026-04-08) Consider including nanColor, below and above range.
        // These are now ignored in the texture
    }
    else if (ghoul::io::texture::isSupportedReadExtension(extension)) {
        return ghoul::io::texture::loadTexture(filename, 1, samplerSettings);
    }
    else {
        throw ghoul::io::texture::MissingReaderException(extension, filename);
    }
}

} // namespace openspace::dataloader::colormap
