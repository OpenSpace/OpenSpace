/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_CORE___DASHBOARDITEM___H__
#define __OPENSPACE_CORE___DASHBOARDITEM___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/properties/scalar/boolproperty.h>
#include <ghoul/glm.h>

namespace ghoul { class Dictionary; }

namespace openspace {

namespace documentation { struct Documentation; }

class DashboardItem : public properties::PropertyOwner {
public:
    static std::unique_ptr<DashboardItem> createFromDictionary(
        const ghoul::Dictionary& dictionary
    );

    explicit DashboardItem(const ghoul::Dictionary& dictionary);

    bool isEnabled() const;
    virtual void update() = 0;

    /**
     * Renders this DashboardItem at the provided \p penPosition. The position indicates
     * where this DashboardItem should render itself and is provided in pixel-coordinates
     * with the top-left corner of the screen being at (0,0). Each derived subclass shall
     * update the \p penPosition according to the items that it should render. If, for
     * example, a single line of text is rendered, the \p penPosition shall be updated by
     * one line's worth of vertical separation.
     *
     * \p penPosition The position at which this DashboardItem should be rendered
     */
    virtual void render(glm::vec2& penPosition) = 0;

    static documentation::Documentation Documentation();

protected:
    properties::BoolProperty _enabled;
};

} // openspace

#endif // __OPENSPACE_CORE___DASHBOARDITEM___H__
