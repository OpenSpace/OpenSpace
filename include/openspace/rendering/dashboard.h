/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_CORE___DASHBOARD___H__
#define __OPENSPACE_CORE___DASHBOARD___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/properties/scalar/boolproperty.h>
#include <ghoul/glm.h>
#include <memory>
#include <vector>

namespace openspace {

namespace scripting { struct LuaLibrary; }

class DashboardItem;

class Dashboard : public properties::PropertyOwner {
public:
    Dashboard();
    ~Dashboard();

    void render(glm::vec2& penPosition);

    void addDashboardItem(std::unique_ptr<DashboardItem> item);
    bool hasItem(int index) const;
    const DashboardItem& item(int index) const;
    void removeDashboardItem(const std::string& identifier);
    void removeDashboardItem(int index);
    void clearDashboardItems();

    /**
    * Returns the Lua library that contains all Lua functions available to affect the
    * rendering.
    */
    static scripting::LuaLibrary luaLibrary();

private:
    properties::BoolProperty _isEnabled;

    std::vector<std::unique_ptr<DashboardItem>> _items;
};

} // openspace

#endif // __OPENSPACE_CORE___DASHBOARD___H__
