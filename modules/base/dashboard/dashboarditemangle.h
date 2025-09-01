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

#ifndef __OPENSPACE_MODULE_BASE___DASHBOARDITEMANGLE___H__
#define __OPENSPACE_MODULE_BASE___DASHBOARDITEMANGLE___H__

#include <openspace/rendering/dashboardtextitem.h>

#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/misc/stringproperty.h>
#include <utility>

namespace openspace {

class SceneGraphNode;

namespace documentation { struct Documentation; }

class DashboardItemAngle : public DashboardTextItem {
public:
    explicit DashboardItemAngle(const ghoul::Dictionary& dictionary);
    ~DashboardItemAngle() override = default;

    void update() override;

    static documentation::Documentation Documentation();

private:
    struct Component {
        properties::OptionProperty type;
        properties::StringProperty nodeIdentifier;
        SceneGraphNode* node;
    };

    static std::pair<glm::dvec3, std::string> positionAndLabel(Component& comp);

    Component _source;
    Component _reference;
    Component _destination;

    std::vector<char> _localBuffer;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___DASHBOARDITEMANGLE___H__
