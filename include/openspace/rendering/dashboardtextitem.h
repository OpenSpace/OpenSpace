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

#ifndef __OPENSPACE_CORE___DASHBOARDTEXTITEM___H__
#define __OPENSPACE_CORE___DASHBOARDTEXTITEM___H__

#include <openspace/rendering/dashboarditem.h>

#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/stringproperty.h>

namespace ghoul { class Dictionary; }
namespace ghoul::fontrendering { class Font; }

namespace openspace {

namespace documentation { struct Documentation; }

class DashboardTextItem : public DashboardItem {
public:
    static documentation::Documentation Documentation();

    DashboardTextItem(const ghoul::Dictionary& dictionary, float fontSize = 10.f,
        const std::string& fontName = "Mono");

protected:
    properties::StringProperty _fontName;
    properties::FloatProperty _fontSize;

    std::shared_ptr<ghoul::fontrendering::Font> _font;
};

} // openspace

#endif // __OPENSPACE_CORE___DASHBOARDTEXTITEM___H__
