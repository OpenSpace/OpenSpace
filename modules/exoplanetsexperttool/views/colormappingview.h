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
#include <string>
#include <vector>

namespace openspace::exoplanets {

class DataViewer;
struct DataSettings;

class ColorMappingView {
public:
    struct ColorMappedVariable {
        int colormapIndex = 0;
        size_t columnIndex = 0;
        float colorScaleMin = 0.f;
        float colorScaleMax = 100.f;
        float opacity = 1.f;
    };

    ColorMappingView(DataViewer& dataViewer,
        const DataSettings& dataSettings);

    void initializeGL();

    const std::vector<ColorMappedVariable>& colorMapperVariables() const;
    size_t firstNumericColumn() const;

    // Return true if the color map was changed
    bool renderViewContent();

    // Render an edit view for one individual color mapped value.
    // Returns true if value was changed. If relevantSystem given,
    // also show a button to color based on planets in that system
    bool renderColormapEdit(ColorMappedVariable& variable,
        std::string_view relevantSystem = "");

    glm::vec4 colorFromColormap(const ExoplanetItem& item,
        const ColorMappedVariable& variable);

private:
    glm::vec4 _nanPointColor = { 0.3f, 0.3f, 0.3f, 1.f };
    std::vector<const char*> _colormaps;

    std::vector<ColorMappedVariable> _variableSelection;

    size_t _firstNumericColumnIndex = 0;

    DataViewer& _dataViewer;
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___COLORMAPPINGVIEW___H__
