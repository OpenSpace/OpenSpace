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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___COLORMAPPINGVIEW___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___COLORMAPPINGVIEW___H__

#include <modules/exoplanetsexperttool/datastructures.h>
#include <map>
#include <string>
#include <vector>

namespace openspace::exoplanets {

class DataViewer;
struct DataSettings;

class ColorMappingView {
public:
    struct CategoryInfo {
        glm::vec4 color = { 1.f, 1.f, 1.f, 1.f };
        size_t count = 0;
    };

    struct ColorMappedVariable {
        int colormapIndex = 0;
        ColumnKey column;
        float colorScaleMin = 0.f;
        float colorScaleMax = 100.f;
        float opacity = 1.f;
        bool useLogScale = false;

        // Categorical color mapping support
        int categoricalPaletteIndex = 0;
        std::map<std::string, CategoryInfo> categories;
    };

    ColorMappingView(DataViewer& dataViewer,
        const DataSettings& dataSettings);

    void initializeGL();

    const std::vector<ColorMappedVariable>& colorMapperVariables();
    const ColumnKey& firstNumericColumn() const;

    // Return true if the color map was changed
    bool render(bool* open);

    // Render an overview of all color mapped variables, with a small preview of the
    // color mapping
    void renderActiveColormapOverview();

    // Render an edit view for one individual color mapped value.
    // Returns true if value was changed. If relevantSystem given,
    // also show a button to color based on planets in that system
    bool renderColormapEdit(ColorMappedVariable& variable,
        std::string_view relevantSystem = "");

    glm::vec4 colorFromColormap(const ExoplanetItem& item,
        const ColorMappedVariable& variable);

    const char* colormapFromIndex(size_t index) const;
    const char* categoricalPaletteFromIndex(size_t index) const;

    void updateCategoriesForVariable(ColorMappedVariable& variable);
    void resetCategoryColorsToPalette(ColorMappedVariable& variable);

private:
    glm::vec4 _nanPointColor = { 0.3f, 0.3f, 0.3f, 1.f };
    std::vector<const char*> _colormaps;
    std::vector<const char*> _categoricalPalettes;

    std::vector<ColorMappedVariable> _variableSelection;

    ColumnKey _firstNumericColumn;

    DataViewer& _dataViewer;
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___COLORMAPPINGVIEW___H__
