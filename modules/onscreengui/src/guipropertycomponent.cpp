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

#include <modules/onscreengui/include/guipropertycomponent.h>
#include <modules/onscreengui/include/renderproperties.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>

#include <openspace/properties/scalarproperty.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/selectionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vectorproperty.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/assert.h>
#include "imgui.h"

namespace {
    const std::string _loggerCat = "GuiPropertyComponent";
    const ImVec2 size = ImVec2(350, 500);
}

namespace openspace {
namespace gui {

GuiPropertyComponent::GuiPropertyComponent(std::string name) 
    : _name(std::move(name))
{}

void GuiPropertyComponent::setSource(SourceFunction function) {
    _function = std::move(function);
}

//void GuiPropertyComponent::registerPropertyOwner(properties::PropertyOwner* owner) {
//    _owners.push_back(owner);
//}
//
//void GuiPropertyComponent::unregisterPropertyOwner(properties::PropertyOwner* owner) {
//    _owners.erase(std::find(_owners.begin(), _owners.end(), owner));
//}

void GuiPropertyComponent::render() {
    ImGui::Begin(_name.c_str(), &_isEnabled, size, 0.5f);

    ImGui::Spacing();

    if (_function) {
        const std::vector<properties::PropertyOwner*>& owners = _function();

        for (properties::PropertyOwner* pOwner : owners) {
            auto header = [&]() -> bool {
                if (owners.size() > 1) {
                    // Create a header in case we have multiple owners
                    return ImGui::CollapsingHeader(pOwner->name().c_str());
                }
                else {
                    // Otherwise, do nothing
                    ImGui::Text(pOwner->name().c_str());
                    ImGui::Spacing();
                    return true;
                }
            };

            if (header()) {
                for (properties::Property* prop : pOwner->propertiesRecursive()) {
                    if (prop->isVisible())
                        renderProperty(prop, pOwner);
                }
            }
        }
    }

    ImGui::End();
}

void GuiPropertyComponent::renderProperty(properties::Property* prop, properties::PropertyOwner* owner)     {
    using Func = std::function<void(properties::Property*, const std::string&)>;
    static std::map<std::string, Func> FunctionMapping = {
        { "BoolProperty", &renderBoolProperty },
        { "IntProperty", &renderIntProperty },
        { "FloatProperty", &renderFloatProperty },
        { "Vec2Property", &renderVec2Property },
        { "Vec3Property", &renderVec3Property },
        { "Vec4Property", &renderVec4Property },
        { "StringProperty", &renderStringProperty },
        { "OptionProperty", &renderOptionProperty },
        { "TriggerProperty", &renderTriggerProperty },
        { "SelectionProperty", &renderSelectionProperty }
    };

    auto it = FunctionMapping.find(prop->className());
    if (it != FunctionMapping.end())
        it->second(prop, owner->name());
}

} // gui
} // openspace
