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

#ifndef __GHOUL___FONT___H__
#define __GHOUL___FONT___H__

#include <ghoul/glm.h>
#include <ghoul/misc/boolean.h>
#include <filesystem>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace ghoul::opengl {
    class TextureAtlas;
    class Texture;
} // namespace ghoul::opengl

namespace ghoul::fontrendering {

/**
 * The Font class encapsulates a single fontface for a specific font size. It contains all
 * the information that is necessary to compute display sizes and, using the FontRendering
 * render the font to the screen. Each Font consists of Glyphs, the individual characters.
 * A Font can have an outline, which is a border of varying thickness around each
 * character. Individual Glyphs can be requested using the #glyph function, which
 * automatically loads and caches missing Glyphs on the first load. The storage backend
 * for Fonts is TextureAtlas into which all Glyphs (regular and outline) are saved. Access
 * into this TextureAtlas is performed on a per-glyph basis and each Glyph stores its
 * texture coordinates in the atlas. Each Font is uniquely identified by the combination
 * of name, font size, whether it has an outline, the thickness of the outline, and the
 * TextureAtlas it uses.
 */
class Font {
public:
    BooleanType(Outline);

    /**
     * This class contains the metrics and the texture locations in the TextureAtlas for a
     * single glyph for a specific font. Each glyph supplies two pairs of coordinates:
     *
     * 1. The top left and bottom right corners of the base glyph (i.e., the regular glyph
     *    if it is rendered without an outline
     * 2. The top left and bottom right corners of the outline glyph (i.e., a filled glyph
     *    that can be rendered behind the base glyph in a different color to provide an
     *    outline to the base
     */
    class Glyph {
    public:
        friend class Font;

        /**
         * The default constructor for a Glyph.
         */
        Glyph(wchar_t character_, int width_ = 0, int height_ = 0,
            float leftBearing_ = 0.f, float topBearing_ = 0.f, float advanceX_ = 0.f,
            float advanceY_ = 0.f, glm::vec2 texCoordTopLeft_ = glm::vec2(0.f),
            glm::vec2 texCoordBottomRight_ = glm::vec2(0.f),
            glm::vec2 outlineTexCoordTopLeft_ = glm::vec2(0.f),
            glm::vec2 outlineTexCoordBottomRight_ = glm::vec2(0.f)
        );

        bool operator==(const Glyph& rhs) const;

        float kerning(wchar_t character) const;

        /// The wide character that this glyph represents
        const wchar_t charcode;

        /// Glyph's width in pixels
        const int width;

        /// Glyph's height in pixels
        const int height;

        /// Glyph's left bearing expressed in pixels
        const float leftBearing;

        /// Glyphs's top bearing expressed in pixels
        const float topBearing;

        /// This is the distance used when the glyph is drawn as part of horizontal text
        const float horizontalAdvance;

        /// This is the distance used when the glyph is drawn as part of vertical text
        const float verticalAdvance;

        /// Normalized texture coordinate of top-left corner
        glm::vec2 topLeft = glm::vec2(0.f);

        /// Normalized texture coordinate of bottom-right corner
        glm::vec2 bottomRight = glm::vec2(0.f);

        /// Normalized texture coordinates for the top left of the outline
        glm::vec2 outlineTopLeft = glm::vec2(0.f);

        /// Normalized texture coordinates for the bottom right of the outline
        glm::vec2 outlineBottomRight = glm::vec2(0.f);

    private:
        /// A vector of kerning pairs relative to this glyph
        std::unordered_map<wchar_t, float> _kerning;
    };

    /**
     * Constructor creating a new Font with the specified \p filename at the provided
     * \p pointSize. The Glyphs of this Font will be stored in the \p atlas TextureAtlas
     * if there is enough free space. If \p outline is `true` two sets of Glyphs are
     * created which are combined to provide an outline of thickness \p outlineThickness
     * to the glyphs.
     *
     * \param filename The full path to the font file
     * \param pointSize The font size in pt
     * \param atlas The TextureAtlas which holds the created Glyphs
     * \param hasOutline A flag whether Glyphs of this Font should have an outline or not
     * \param outlineThickness The thickness of the outline. This setting is ignored if
     *        the Font does not have an outline
     *
     * \throw RuntimeError If there was an error loading the basic font information
     * \pre \p filename must not be empty
     * \pre \p pointSize must be positive and bigger than 0
     */
    Font(std::filesystem::path filename, float pointSize, opengl::TextureAtlas& atlas,
         Outline hasOutline = Outline::Yes, float outlineThickness = 1.f
    );

    /**
     * Returns the name of the Font.
     *
     * \return The name of the Font
     */
    const std::filesystem::path& name() const;

    /**
     * Returns the font size of this Font.
     *
     * \return The font size of this Font
     */
    float pointSize() const;

    /**
     * Returns the line seperator for this Font. This is the vertical length that
     * separates two consecutive lines.
     *
     * \return The vertical line separation
     */
    float height() const;

    /**
     * Returns whether this Font has an outline or not.
     *
     * \return `true` if this Font has an outline, `false` otherwise
     */
    bool hasOutline() const;

    /**
     * Computes and retures the bounding box for the passed string with the settings of
     * this Font. The value returned is in pixel values and provides the width and the
     * height of the text if it were to be rendered.
     *
     * \param text The text that is rendered to the screen. The `text` can also contain
     *        `\n` to have a linebreak, which is of the correct length for the selected
     *        font
     * \return The pixel coordinates of the bounding box of the passed text
     */
    glm::vec2 boundingBox(std::string_view text);

    /**
     * Returns the Glyph that representes the passed \p character. The first call to this
     * function for each character creates and caches the Glyph before returning it.
     *
     * \param character The character for which the Glyph should be returned
     * \return A pointer to the Glyph
     *
     * \throw RuntimeError If a FreeType exception occurred while loading the glyph
     * \throw GlyphException If there was an error loading the glyph
     */
    const Glyph* glyph(wchar_t character);

    /**
     * Preloads a list of Glyphs. Glyphs that are passed as part of \p characters that
     * have been loaded previously are ignored and not loaded multiple times.
     *
     * \param characters A list of characters for which Glyphs should be created and
     *        cached
     *
     * \throw RuntimeError If a FreeType exception occurred while loading the glyphs
     * \throw GlyphException If there was an error loading the glyph
     */
    void loadGlyphs(std::vector<wchar_t> characters);

    /**
     * Returns the texture behind the TextureAtlas that stores all of the Glyphs.
     *
     * \return The TextureAtlas's texture that stores all of the Glyphs for this Font
     */
    const opengl::Texture& atlasTexture() const;

private:
    /**
     * Generates the Kerning values for all Glyph pairs that have sofar been loaded.
     */
    void generateKerning();

    /// A list of all loaded Glyphs
    std::vector<Glyph> _glyphs;

    /// The TextureAtlas backend storage for the loaded Glyphs
    opengl::TextureAtlas& _atlas;

    /// The file name of this Font
    const std::filesystem::path _name;

    /// The font size in pt
    const float _pointSize;

    /// The vertical distance between two consecutive lines
    float _height = 0.f;

    /// Whether this Font has an outline or not
    const Outline _hasOutline;

    /// The thickness of the outline
    const float _outlineThickness;
};

} // namespace ghoul::fontrendering

#endif // __GHOUL___FONT___H__
