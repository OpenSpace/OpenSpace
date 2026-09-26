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

#include <ghoul/font/fontmanager.h>

#include <ghoul/font/font.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/misc/profiling.h>
#include <array>
#include <utility>
#include <vector>

namespace {
    /// The default set of glyphs that are loaded when a new Font is initialized
    constexpr std::array<wchar_t, 94> DefaultCharacterSet = {
        L' ', L'!', L'\\', L'"', L'#', L'$', L'%', L'&', L'\'', L'(',
        L')', L'*', L'+', L',', L'-', L'.', L'/', L'0', L'1', L'2',
        L'3', L'4', L'5', L'6', L'7', L'8', L'9', L':', L';', L'<',
        L'=', L'>', L'?', L'@', L'A', L'B', L'C', L'D', L'E', L'F',
        L'G', L'H', L'I', L'J', L'K', L'L', L'M', L'N', L'O', L'P',
        L'Q', L'R', L'S', L'T', L'U', L'V', L'W', L'X', L'Y', L'Z',
        L'[', L']', L'^', L'_', L'~', L'a', L'b', L'c', L'd', L'e',
        L'f', L'g', L'h', L'i', L'j', L'k', L'l', L'm', L'n', L'o',
        L'p', L'q', L'r', L's', L't', L'u', L'v', L'w', L'x', L'y',
        L'z', L'{', L'|', L'}'
    };
} // namespace

namespace ghoul::fontrendering {

FontManager::FontManager(glm::ivec3 atlasDimensions)
    : _textureAtlas(std::move(atlasDimensions))
{}

void FontManager::initialize() {
    _textureAtlas.initialize();
}

void FontManager::deinitialize() {
    _textureAtlas.deinitialize();
}

unsigned int FontManager::registerFontPath(std::string_view fontName,
                                           std::filesystem::path filePath)
{
    ghoul_assert(!fontName.empty(), "Fontname must not be empty");
    ghoul_assert(!filePath.empty(), "Filepath must not be empty");

    const unsigned int hash = hashCRC32(fontName);
    const auto it = _fontPaths.find(hash);
    if (it != _fontPaths.cend()) {
        const std::filesystem::path& registeredPath = it->second;

        if (registeredPath != filePath) {
            throw RuntimeError(
                std::format(
                    "Font '{}' was registered with path '{}' before, trying '{}' now",
                    fontName, registeredPath, filePath
                ),
                "FontManager"
            );
        }

        return hash;
    }
    _fontPaths[hash] = filePath;
    return hash;
}

std::shared_ptr<Font> FontManager::font(std::string_view name, float fontSize,
                                        Outline withOutline, LoadGlyphs loadGlyphs)
{
    ZoneScoped;
    ghoul_assert(!name.empty(), "Name must not be empty");

    unsigned int hash = hashCRC32(name);
    const auto itPath = _fontPaths.find(hash);
    if (itPath == _fontPaths.cend()) {
        // There is no hash registered for the current name, so it might be a local file
        if (std::filesystem::is_regular_file(name)) {
            hash = registerFontPath(name, name);
        }
        else {
            // The name has not neen previously registered and it is not a valid path
            throw RuntimeError(
                std::format("Name '{}' is not a valid font or file", name),
                "FontManager"
            );
        }
    }

    return font(hash, fontSize, withOutline, loadGlyphs);
}

std::shared_ptr<Font> FontManager::font(unsigned int hashName, float fontSize,
                                        Outline withOutline, LoadGlyphs loadGlyphs)
{
    ZoneScoped;

    const auto itPath = _fontPaths.find(hashName);
    if (itPath == _fontPaths.cend()) {
        throw RuntimeError(
            std::format(
                "Error retrieving font with hash '{}' for size '{}'", hashName, fontSize
            ),
            "FontManager"
        );
    }

    // First check if we already have a font of the correct size created
    auto fonts = _fonts.equal_range(hashName);
    for (auto it = fonts.first; it != fonts.second; it++) {
        const float delta = 1e-6f; // Font sizes are 1-1000, so a delta of 1/e6 is fine
        if ((glm::abs(it->second->pointSize() - fontSize) < delta) &&
            it->second->hasOutline() == withOutline)
        {
            return it->second;
        }
    }

    // If we get this far, it's a new font that has to be created
    auto f = std::make_shared<Font>(
        _fontPaths[hashName],
        fontSize,
        _textureAtlas,
        Font::Outline(withOutline)
    );

    if (loadGlyphs) {
        ZoneScopedN("Load Glyphs");
        TracyGpuZone("Load Glyphs");
        f->loadGlyphs(
            std::vector<wchar_t>(DefaultCharacterSet.begin(), DefaultCharacterSet.end())
        );
    }

    _fonts.emplace(hashName, f);
    return f;
}

} // namespace ghoul::fontrendering
