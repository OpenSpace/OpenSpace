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

#include <ghoul/font/font.h>

#include <ghoul/font/fonterrors.h>
#include <ghoul/format.h>
#include <ghoul/freetype.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/defer.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureatlas.h>
#include <algorithm>
#include <array>
#include <limits>
#include <string>
#include <utility>

namespace {
    using namespace ghoul;
    using namespace ghoul::fontrendering;

    // Sizes in FT are given in 1/64th of pt
    constexpr float PointConversionFactor = 64.f;
    constexpr int DPI = 96;

    void handleError(FT_Error err, FT_Library library, FT_Face face, FT_Stroker stroker,
                     const std::filesystem::path& name, float size)
    {
        if (!err) {
            return;
        }

        if (stroker) {
            FT_Stroker_Done(stroker);
        }
        if (face) {
            FT_Done_Face(face);
        }
        if (library) {
            FT_Done_FreeType(library);
        }
        Error e = error(err);
        throw RuntimeError(
            std::format(
                "Error loading font '{}' for size '{}': {} {}",
                name, size, e.code, e.message
            ),
            "Font"
        );
    }

    /**
     * Initializes the passed 'library' and loads the font face specified by the 'name'
     * and 'size' into the provided 'face'.
     */
    void loadFace(const std::filesystem::path& name, float size, FT_Library& library,
                  FT_Face& face)
    {
        ZoneScoped;

        const FT_Error e1 = FT_Init_FreeType(&library);
        handleError(e1, nullptr, nullptr, nullptr, name, size);

        const std::string n = name.string();
        const FT_Error e2 = FT_New_Face(library, n.c_str(), 0, &face);
        handleError(e2, library, nullptr, nullptr, name, size);

        const FT_Error e3 = FT_Select_Charmap(face, FT_ENCODING_UNICODE);
        handleError(e3, library, face, nullptr, name, size);

        const int s = static_cast<int>(size * PointConversionFactor);
        const FT_Error e4 = FT_Set_Char_Size(face, s, 0, DPI, DPI);
        handleError(e4, library, face, nullptr, name, size);
    }

    /**
     * Extracts the next line from the string view and returns it, the passed string_view
     * is modified to remove the new line *and* the \n character.
     */
    std::string_view extractLine(std::string_view& view) {
        const std::string_view::size_type p = view.find('\n');
        if (p == std::string_view::npos) {
            // No new line found
            const std::string_view res = view;
            view = std::string_view();
            return res;
        }

        const std::string_view res = view.substr(0, p);
        view = view.substr(p + 2);
        return res;
    }
} // namespace

namespace ghoul::fontrendering {

Font::Glyph::Glyph(wchar_t character_, int width_, int height_, float leftBearing_,
                   float topBearing_, float advanceX_, float advanceY_,
                   glm::vec2 texCoordTopLeft_, glm::vec2 texCoordBottomRight_,
                   glm::vec2 outlineTexCoordTopLeft_,
                   glm::vec2 outlineTexCoordBottomRight_)
    : charcode(character_)
    , width(width_)
    , height(height_)
    , leftBearing(leftBearing_)
    , topBearing(topBearing_)
    , horizontalAdvance(advanceX_)
    , verticalAdvance(advanceY_)
    , topLeft(std::move(texCoordTopLeft_))
    , bottomRight(std::move(texCoordBottomRight_))
    , outlineTopLeft(std::move(outlineTexCoordTopLeft_))
    , outlineBottomRight(std::move(outlineTexCoordBottomRight_))
{}

bool Font::Glyph::operator==(const Font::Glyph& rhs) const {
    return charcode == rhs.charcode;
}

float Font::Glyph::kerning(wchar_t character) const {
    const auto it = _kerning.find(character);
    return it != _kerning.cend() ? it->second : 0.f;
}

Font::Font(std::filesystem::path filename, float pointSize, opengl::TextureAtlas& atlas,
           Outline hasOutline, float outlineThickness)
    : _atlas(atlas)
    , _name(std::move(filename))
    , _pointSize(pointSize)
    , _hasOutline(hasOutline)
    , _outlineThickness(outlineThickness)
{
    ZoneScoped;

    ghoul_assert(!_name.empty(), "Filename must not be empty");
    ghoul_assert(_pointSize > 0.f, "Need positive point size");

    // Get font metrics at higher resolution for increased accuracy
    constexpr float HighFaceResolutionFactor = 100.f;

    FT_Library library = nullptr;
    FT_Face face = nullptr;
    defer {
        FT_Done_Face(face);
        FT_Done_FreeType(library);
    };
    loadFace(_name, _pointSize * HighFaceResolutionFactor, library, face);

    _height = (face->size->metrics.height >> 6) / HighFaceResolutionFactor;

    _glyphs.reserve(128);

    // -1 is a special glyph
    glyph(static_cast<wchar_t>(-1));
}

const std::filesystem::path& Font::name() const {
    return _name;
}

float Font::pointSize() const {
    return _pointSize;
}

float Font::height() const {
    return _height;
}

const opengl::Texture& Font::atlasTexture() const {
    return _atlas.texture();
}

bool Font::hasOutline() const {
    return _hasOutline;
}

glm::vec2 Font::boundingBox(std::string_view text) {
    const size_t lines = std::count(text.begin(), text.end(), '\n') + 1;
    float sizeX = 0.f;
    do {
        const std::string_view line = extractLine(text);

        float width = 0.f;
        for (const char c : line) {
            wchar_t character = c;
            if (character == wchar_t('\t')) {
                character = wchar_t(' ');
            }
            const Glyph* g = glyph(character);
            width += g->horizontalAdvance;
        }

        sizeX = std::max(sizeX, width);
    } while (!text.empty());

    return glm::vec2(sizeX, lines * height());
}

const Font::Glyph* Font::glyph(wchar_t character) {
    ZoneScoped;

    // Check if charcode has been already loaded
    for (const Glyph& g : _glyphs) {
        if (g.charcode == character) {
            return &g;
        }
    }

    // Charcode -1 is special: it is used for line drawing (overline, underline,
    // strikethrough) and background.
    if (character == static_cast<wchar_t>(-1)) {
        const opengl::TextureAtlas::RegionHandle handle = _atlas.newRegion(4, 4);
        // The last *4 for the depth is not a danger here as _atlas.setRegion only
        // extracts as much data as is needed for the used texture atlas
        std::array<unsigned char, 4 * 4 * 4> data {};
        data.fill(std::numeric_limits<unsigned char>::max());

        _atlas.setRegionData(handle, data.data());

        Glyph glyph(static_cast<wchar_t>(-1));
        const opengl::TextureAtlas::TextureCoordinatesResult coords =
            _atlas.textureCoordinates(handle);
        glyph.topLeft = coords.topLeft;
        glyph.bottomRight = coords.bottomRight;
        _glyphs.push_back(std::move(glyph));

        return &(_glyphs.back());
    }

    // Glyph has not been already loaded
    loadGlyphs({ character });
    return &(_glyphs.back());
}

void Font::loadGlyphs(std::vector<wchar_t> characters) {
    using namespace opengl;

    ZoneScoped;
    TracyGpuZone("loadGlyph")

    const unsigned int atlasDepth  = _atlas.size().z;

    FT_Library library = nullptr;
    FT_Face face = nullptr;
    defer {
        FT_Done_Face(face);
        FT_Done_FreeType(library);
    };
    loadFace(_name, _pointSize, library, face);

    constexpr float HighResolutionFactor = 10.f;
    FT_Library libraryHighRes = nullptr;
    FT_Face faceHighRes = nullptr;
    defer {
        if (face) {
            FT_Done_Face(faceHighRes);
        }
        if (library) {
            FT_Done_FreeType(libraryHighRes);
        }
    };
    loadFace(_name, _pointSize * HighResolutionFactor, libraryHighRes, faceHighRes);

    // Check for invalid glyph codes first
    for (wchar_t& charcode : characters) {
        const FT_UInt glyphIndex = FT_Get_Char_Index(face, charcode);
        if (glyphIndex == 0) {
            // Invalid glyph; replace with a space
            LWARNINGC(
                "Font",
                std::format(
                    "Invalid glyph '{}' found and replaced", static_cast<int>(charcode)
                )
            );

            charcode = ' ';
        }
    }

    for (const wchar_t charcode : characters) {
        ZoneScopedN("Character");

        const auto it = std::find_if(
            _glyphs.cbegin(),
            _glyphs.cend(),
            [charcode](const Glyph& glyph) { return glyph.charcode == charcode; }
        );
        if (it != _glyphs.cend()) {
            continue;
        }

        // First generate the font without outline and store it in the font atlas only if
        // an outline is request, repeat the process for the outline

        float leftBearing = 0.f;
        float topBearing = 0.f;
        glm::vec2 topLeft = glm::vec2(0.f);
        glm::vec2 bottomRight = glm::vec2(0.f);
        glm::vec2 outlineTopLeft = glm::vec2(0.f);
        glm::vec2 outlineBottomRight = glm::vec2(0.f);
        unsigned int width = 0;
        unsigned int height = 0;

        const FT_UInt glyphIndex = FT_Get_Char_Index(face, charcode);
        ghoul_assert(glyphIndex != 0, "Glyph index not found");

        const FT_Error error = FT_Load_Glyph(face, glyphIndex, FT_LOAD_FORCE_AUTOHINT);
        handleError(error, library, face, nullptr, _name, _pointSize);

        // In case the font has an outline, we load it first as we need the size of the
        // outline for loading the base layer. The reason for this is that the outline is
        // slightly bigger than the base layer for most Glyphs. Therefore, if the Font has
        // an outline, we need to increase the size of the base to match the outline so
        // that they can be rendered on top of each other
        if (_hasOutline) {
            ZoneScopedN("Outline");
            FT_Stroker stroker = nullptr;
            const FT_Error e1 = FT_Stroker_New(library, &stroker);
            handleError(e1, library, face, stroker, _name, _pointSize);

            FT_Stroker_Set(
                stroker,
                static_cast<int>(_outlineThickness * PointConversionFactor),
                FT_STROKER_LINECAP_ROUND,
                FT_STROKER_LINEJOIN_ROUND,
                0
            );

            FT_Glyph outlineGlyph = nullptr;
            const FT_Error e2 = FT_Get_Glyph(face->glyph, &outlineGlyph);
            handleError(e2, library, face, stroker, _name, _pointSize);

            const FT_Error e3 = FT_Glyph_Stroke(&outlineGlyph, stroker, 1);
            handleError(e3, library, face, stroker, _name, _pointSize);
            FT_Stroker_Done(stroker);

            const FT_Error e4 = FT_Glyph_To_Bitmap(
                &outlineGlyph,
                FT_RENDER_MODE_NORMAL,
                nullptr,
                1
            );
            handleError(e4, library, face, nullptr, _name, _pointSize);

            FT_BitmapGlyph outlineBitmap = reinterpret_cast<FT_BitmapGlyph>(outlineGlyph);
            topBearing = static_cast<float>(outlineBitmap->top);
            leftBearing = static_cast<float>(outlineBitmap->left);

            width = outlineBitmap->bitmap.width / atlasDepth;
            height = outlineBitmap->bitmap.rows;

            const TextureAtlas::RegionHandle handle = _atlas.newRegion(width, height);
            if (outlineBitmap->bitmap.buffer) {
                _atlas.setRegionData(handle, outlineBitmap->bitmap.buffer);
            }
            const TextureAtlas::TextureCoordinatesResult res =
                _atlas.textureCoordinates(handle);
            outlineTopLeft = res.topLeft;
            outlineBottomRight = res.bottomRight;
        }

        FT_Glyph insideGlyph = nullptr;
        const FT_Error e1 = FT_Get_Glyph(face->glyph, &insideGlyph);
        handleError(e1, library, face, nullptr, _name, _pointSize);

        const FT_Error e2 = FT_Glyph_To_Bitmap(
            &insideGlyph,
            FT_RENDER_MODE_NORMAL,
            nullptr,
            1
        );
        handleError(e2, library, face, nullptr, _name, _pointSize);

        const FT_BitmapGlyph insideBitmap = reinterpret_cast<FT_BitmapGlyph>(insideGlyph);
        topBearing = std::max(topBearing, static_cast<float>(insideBitmap->top));
        if (!_hasOutline) {
            leftBearing = static_cast<float>(insideBitmap->left);
        }

        // We take the maximum of the width (either 0 if there is no outline, or the
        // outline width if there is one) and the height
        width = std::max(width, insideBitmap->bitmap.width / atlasDepth);
        height = std::max(height, insideBitmap->bitmap.rows);

        const TextureAtlas::RegionHandle handle = _atlas.newRegion(width, height);

        // If we don't have an outline for this font, our current 'width' and 'height'
        // corresponds to the buffer, so we can just use it straight away. If we have an
        // outline, the buffer has a different size from the region in the atlas, so we
        // need to copy the values from the buffer into the atlas region first
        if (!_hasOutline) {
            if (insideBitmap->bitmap.buffer) {
                _atlas.setRegionData(handle, insideBitmap->bitmap.buffer);
            }
            const TextureAtlas::TextureCoordinatesResult res =
                _atlas.textureCoordinates(handle);
            topLeft = res.topLeft;
            bottomRight = res.bottomRight;
        }
        else {
            std::vector<unsigned char> buffer(width * height * sizeof(char), 0);
            const int widthOffset = (width - insideBitmap->bitmap.width) / 2;
            const int heightOffset = (height - insideBitmap->bitmap.rows) / 2;

            for (int j = 0; j < static_cast<int>(height); j++) {
                for (int i = 0; i < static_cast<int>(width); i++) {
                    const int k = i - widthOffset;
                    const int l = j - heightOffset;

                    const bool inBorder =
                        (k < 0) || (k >= static_cast<int>(insideBitmap->bitmap.width)) ||
                        (l < 0) || (l >= static_cast<int>(insideBitmap->bitmap.rows));

                    if (!inBorder) {
                        buffer[(i + j*width)] =
                            insideBitmap->bitmap.buffer[k + insideBitmap->bitmap.width*l];
                    }
                }
            }

            if (!buffer.empty()) {
                _atlas.setRegionData(handle, buffer.data());
            }

            // We need to offset the texture coordinates by half of the width and height
            // differences
            const TextureAtlas::TextureCoordinatesResult res =
                _atlas.textureCoordinates(
                    handle,
                    glm::ivec4(
                        widthOffset / 4.f, heightOffset / 4.f,
                        widthOffset / 4.f, heightOffset / 4.f
                    )
                );
            topLeft = res.topLeft;
            bottomRight = res.bottomRight;
        }

        // Discard hinting to get advance
        FT_Load_Glyph(face, glyphIndex, FT_LOAD_RENDER | FT_LOAD_NO_HINTING);
        _glyphs.emplace_back(
            charcode,
            width,
            height,
            leftBearing,
            topBearing,
            face->glyph->advance.x / PointConversionFactor,
            face->glyph->advance.y / PointConversionFactor,
            topLeft,
            bottomRight,
            outlineTopLeft,
            outlineBottomRight
        );
    }

    _atlas.upload();
    generateKerning();
}

void Font::generateKerning() {
    FT_Library library = nullptr;
    FT_Face face = nullptr;
    defer {
        FT_Done_Face(face);
        FT_Done_FreeType(library);
    };
    loadFace(_name, _pointSize, library, face);

    const bool hasKerning = FT_HAS_KERNING(face);
    if (!hasKerning) {
        return;
    }

    // For each combination of Glyphs, determine the kerning factors. The index starts at
    // 1 as 0 is reserved for the special background glyph
    for (size_t i = 1; i < _glyphs.size(); i++) {
        Glyph& glyph = _glyphs[i];
        const FT_UInt glyphIndex = FT_Get_Char_Index(face, glyph.charcode);
        glyph._kerning.clear();

        for (size_t j = 1; j < _glyphs.size(); j++) {
            const Glyph& prevGlyph = _glyphs[j];
            const FT_UInt prevIndex = FT_Get_Char_Index(face, prevGlyph.charcode);
            FT_Vector kerning;
            FT_Get_Kerning(face, prevIndex, glyphIndex, FT_KERNING_DEFAULT, &kerning);
            if (kerning.x != 0) {
                glyph._kerning[prevGlyph.charcode] = kerning.x / PointConversionFactor;
            }
        }
    }
}

} // namespace ghoul::fontrendering
