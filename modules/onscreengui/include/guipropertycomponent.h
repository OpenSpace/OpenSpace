/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __GUIPROPERTYCOMPONENT_H__
#define __GUIPROPERTYCOMPONENT_H__

#include <modules/onscreengui/include/guicomponent.h>

#include <functional>
#include <vector>

namespace openspace {

namespace properties {
    class Property;
    class PropertyOwner;
}

namespace gui {

class GuiPropertyComponent : public GuiComponent {
public:
    using SourceFunction = std::function<std::vector<properties::PropertyOwner*>()>;

    GuiPropertyComponent(std::string name);

    // This is the function that evaluates to the list of Propertyowners that this
    // component should render
    void setSource(SourceFunction func);

    void render();

protected:
    void renderProperty(properties::Property* prop, properties::PropertyOwner* owner);

    std::string _name;
    SourceFunction _function;
};

} // namespace gui
} // namespace openspace

#endif // __GUIPROPERTYCOMPONENT_H__
