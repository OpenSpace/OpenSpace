/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___DASHBOARDITEMGLOBELOCATION___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___DASHBOARDITEMGLOBELOCATION___H__

#include <openspace/rendering/dashboarditem.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>

namespace ghoul::fontrendering { class Font; }

namespace openspace {

namespace documentation { struct Documentation; }

class DashboardItemGlobeLocation : public DashboardItem {
public:
    DashboardItemGlobeLocation(const ghoul::Dictionary& dictionary);
    ~DashboardItemGlobeLocation() override = default;

    void render(glm::vec2& penPosition) override;

    glm::vec2 size() const override;

    static documentation::Documentation Documentation();

private:
    enum class DisplayFormat {
        DecimalDegrees = 0,
        DegreeMinuteSeconds
    };

    properties::StringProperty _fontName;
    properties::FloatProperty _fontSize;

    properties::OptionProperty _displayFormat;
    properties::IntProperty _significantDigits;

    std::shared_ptr<ghoul::fontrendering::Font> _font;
    std::string _formatString;
    std::vector<char> _buffer;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___DASHBOARDITEMGLOBELOCATION___H__
