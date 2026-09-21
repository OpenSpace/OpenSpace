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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___GLYPHHANDLER___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___GLYPHHANDLER___H__

namespace openspace::exoplanets {

class DataViewer;

/**
 * The GlyphHandler is responsible for managing the glyphs in the exoplanets data viewer.
 * This includes updating the renderables when switching between different views, and
 * settings related to the renderables used for the glyphs
 */
class GlyphHandler {
public:
    enum class GlyphMode {
        Rings = 0,
        Inclination,
        Star,
        InclinationAndStar
    };

    // The identifier used for the glyph cloud renderable throughout the module
    constexpr static std::string_view GlyphCloudIdentifier = "ExoplanetDataPoints";
    constexpr static std::string_view HostCloudIdentifier = "ExoplanetHostPoints";
    constexpr static float DefaultGlyphScale = 1.f;

    GlyphHandler(DataViewer& dataViewer);

    void initializeRenderables();

    void updateSelectionInRenderable(const std::vector<size_t>& selection);

    int getHoveredPlanetIndex() const;

    void setGlyphScale(float scale);
    void setGlyphMode(GlyphMode mode);

    void renderModeSpecificSettings();

private:
    GlyphMode _mode = GlyphMode::Rings;

    DataViewer& _dataViewer;
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___GLYPHHANDLER___H__
