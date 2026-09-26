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

#ifndef __GHOUL___FONTMANAGER___H__
#define __GHOUL___FONTMANAGER___H__

#include <ghoul/glm.h>
#include <ghoul/opengl/textureatlas.h>
#include <ghoul/misc/boolean.h>
#include <filesystem>
#include <map>
#include <memory>
#include <string_view>
#include <unordered_map>

namespace ghoul::fontrendering {

class Font;

/**
 * This class manages different Font objects, stores them in a TextureAtlas and provides
 * access to these fonts based a user-defined, unique name. The paths to fonts must be
 * registered (#registerFontPath) with a name before the fonts can be accessed (#font).
 * Each registered font can give rise to one Font class for each requested fontSize. When
 * each Font is created, it is, on default, initialized with a default set of glyphs and
 * has an outline with a default outline thickness.
 */
class FontManager {
public:
    BooleanType(Outline);
    BooleanType(LoadGlyphs);

    /**
     * The constructor that initializes the TextureAtlas. This means that this constructor
     * requires a valid OpenGL context.
     *
     * \param atlasDimensions The dimensions of the TextureAtlas that is used as storage
     *        for all fonts that are loaded through this FontManager. See the TextureAtlas
     *        constructor documentation for limitations on the sizes
     */
    FontManager(glm::ivec3 atlasDimensions = glm::ivec3(512, 512, 1));

    /**
     * Initializes the TextureAtlas backend and requires a valid OpenGL state.
     */
    void initialize();

    /**
     * Deinitializes the TextureAtlas backend and requires a valid OpenGL state.
     */
    void deinitialize();

    /**
     * Registers a user-defined \p fontName to an absolute \p filePath. This function does
     * not check whether the file exists, or is accessible. If either of these is the
     * case, subsequent #font calls fail. The \p fontName cannot have been assigned to a
     * different \p filePath or an error is reported. This method returns a hashed version
     * of the \p fontName that can be used in calls to the #font method for a more
     * efficient lookup.
     *
     * \param fontName The user-defined name under which this font is registered
     * \param filePath The filepath for this registered Font
     * \return The hashed representation of the \p fontName
     *
     * \throw FontManagerException If there was an error registering the Font
     * \pre \p fontName must not be empty
     * \pre \p filePath must not be empty
     */
    unsigned int registerFontPath(std::string_view fontName,
        std::filesystem::path filePath);

    /**
     * Retrieves the Font with the name `name`, which must have been previously registered
     * (#registerFontPath). If this is the first call to this function for a specific
     * combination of `name` and `fontSize` the Font is initialized. In this case, the
     * `loadGlyphs` parameter determines whether the Font object will be initialized with
     * a common set of English glyphs. Otherwise, the function will return immediately
     * with the correct Font object. If the `name` does not name a registered Font or the
     * registered path does not exist, a `nullptr` is returned.
     *
     * \param name User-defined name for the Font that is to be retrieved
     * \param fontSize The font size (in pt) for the Font
     * \param withOutline If this parameter is `true` the created Font will contain
     *        outlines as well as the base Font
     * \param loadGlyphs If `true`, the first initialization of the Font will also preload
     *        a set of commonly used glyphs
     * \return Returns a usable and initialized Font object, or `nullptr` if an error
     *         occurred
     *
     * \pre \p name must not be empty
     */
    std::shared_ptr<Font> font(std::string_view name, float fontSize,
        Outline withOutline = Outline::Yes, LoadGlyphs loadGlyphs = LoadGlyphs::Yes);

    /**
     * Retrieves the Font with the hashed name `hashName`, which must have been previously
     * registered (#registerFontPath). If this is the first call to this function for a
     * specific combination of `hashName` and `fontSize` the Font is initialized. In this
     * case, the `loadGlyphs` parameter determines whether the Font object will be
     * initialized with a common set of English glyphs. Otherwise, the function will
     * return immediately with the correct Font object. If the `name` does not name a
     * registered Font or the registered path does not exist, a `nullptr` is returned.
     *
     * \param hashName A hashed name of the font that is to be retrieved
     * \param fontSize The font size (in pt) for the Font
     * \param withOutline If this parameter is `true` the created Font will contain
     *        outlines as well as the base Font
     * \param loadGlyphs If `true`, the first initialization of the Font will also preload
     *        a set of commonly used glyphs
     * \return Returns a usable and initialized Font object, or `nullptr` if an error
     *         occurred
     */
    std::shared_ptr<Font> font(unsigned int hashName, float fontSize,
        Outline withOutline = Outline::Yes, LoadGlyphs loadGlyphs = LoadGlyphs::Yes);

private:
    FontManager(const FontManager& rhs) = delete;
    FontManager(FontManager&& rhs) = delete;
    FontManager& operator=(const FontManager& rhs) = delete;
    FontManager& operator=(FontManager&& rhs) = delete;

    /// The TextureAtlas that is used to store all glyphs for all registered Font objects
    opengl::TextureAtlas _textureAtlas;

    /// The map that is used to retrieve previously created Font objects
    std::multimap<unsigned int, std::shared_ptr<Font>> _fonts;

    /// The map that correlates the hashed names with the file paths for the fonts
    std::unordered_map<unsigned int, std::filesystem::path> _fontPaths;
};

} // namespace ghoul::fontrendering

#endif // __GHOUL___FONTMANAGER___H__
