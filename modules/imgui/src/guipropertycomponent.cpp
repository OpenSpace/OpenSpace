/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/imgui/include/guipropertycomponent.h>

#include <modules/imgui/include/imgui_include.h>
#include <modules/imgui/include/renderproperties.h>

#include <openspace/properties/propertyowner.h>

#include <algorithm>

namespace {
    const ImVec2 size = ImVec2(350, 500);

    int nVisibleProperties(const std::vector<openspace::properties::Property*>& props,
        openspace::properties::Property::Visibility visibility)
    {
        return static_cast<int>(std::count_if(
            props.begin(),
            props.end(),
            [visibility](openspace::properties::Property* p) {
                using V = openspace::properties::Property::Visibility;
                return
                    static_cast<std::underlying_type_t<V>>(visibility) >=
                    static_cast<std::underlying_type_t<V>>(p->visibility());
            }
        ));
    }

    void renderTooltip(openspace::properties::PropertyOwner* propOwner) {
        const bool shouldDisplay =
            ImGui::IsItemHovered() && (!propOwner->description().empty());

        if (shouldDisplay) {
            ImGui::SetTooltip("%s", propOwner->description().c_str());
        }
    }
} // namespace

namespace openspace::gui {

GuiPropertyComponent::GuiPropertyComponent(std::string name) 
    : GuiComponent(std::move(name))
{}

void GuiPropertyComponent::setSource(SourceFunction function) {
    _function = std::move(function);
}

void GuiPropertyComponent::setVisibility(properties::Property::Visibility visibility) {
    _visibility = visibility;
}

void GuiPropertyComponent::setHasRegularProperties(bool hasOnlyRegularProperties) {
    _hasOnlyRegularProperties = hasOnlyRegularProperties;
}

void GuiPropertyComponent::renderPropertyOwner(properties::PropertyOwner* owner) {
    if (owner->propertiesRecursive().empty()) {
        return;
    }

    int nThisProperty = nVisibleProperties(owner->properties(), _visibility);
    ImGui::PushID(owner->name().c_str());
    const auto& subOwners = owner->propertySubOwners();
    for (properties::PropertyOwner* subOwner : subOwners) {
        std::vector<properties::Property*> properties = subOwner->propertiesRecursive();
        int count = nVisibleProperties(properties, _visibility);
        if (count == 0) {
            continue;
        }
        if (subOwners.size() == 1 && (nThisProperty == 0)) {
            renderPropertyOwner(subOwner);
        }
        else {
            bool opened = ImGui::TreeNode(subOwner->name().c_str());
            renderTooltip(subOwner);
            if (opened) {
                renderPropertyOwner(subOwner);
                ImGui::TreePop();
            }
        }
    }

    if (!subOwners.empty()) {
        ImGui::Spacing();
    }

    using Properties = std::vector<properties::Property*>;
    std::map<std::string, Properties> propertiesByGroup;
    Properties remainingProperies;

    for (properties::Property* p : owner->properties()) {
        std::string group = p->groupIdentifier();
        if (group.empty()) {
            remainingProperies.push_back(p);
        }
        else {
            propertiesByGroup[group].push_back(p);
        }
    }

    for (const std::pair<std::string, Properties>& p : propertiesByGroup) {
        if (ImGui::TreeNode(p.first.c_str())) {
            for (properties::Property* prop : p.second) {
                renderProperty(prop, owner);
            }
            ImGui::TreePop();
        }
    }

    if (!propertiesByGroup.empty()) {
        ImGui::Spacing();
    }

    for (properties::Property* prop : remainingProperies) {
        renderProperty(prop, owner);
    }
    ImGui::PopID();
}

void GuiPropertyComponent::render() {
    bool v = _isEnabled;
    ImGui::Begin(name().c_str(), &v, size, 0.5f);
    _isEnabled = v;

    if (_function) {
        std::vector<properties::PropertyOwner*> owners = _function();
        std::sort(
            owners.begin(),
            owners.end(),
            [](properties::PropertyOwner* lhs, properties::PropertyOwner* rhs) {
                return lhs->name() < rhs->name();
            }
        );

        for (properties::PropertyOwner* pOwner : owners) {
            int count = nVisibleProperties(pOwner->propertiesRecursive(), _visibility);

            if (count == 0) {
                continue;
            }

            auto header = [&]() -> bool {
                if (owners.size() > 1) {
                    // Create a header in case we have multiple owners
                    return ImGui::CollapsingHeader(pOwner->name().c_str());
                }
                else if (!pOwner->name().empty()) {
                    // If the owner has a name, print it first
                    ImGui::Text("%s", pOwner->name().c_str());
                    ImGui::Spacing();
                    return true;
                }
                else {
                    // Otherwise, do nothing
                    return true;
                }
            };

            if (header()) {
                renderPropertyOwner(pOwner);
            }
        }
    }

    ImGui::End();
}

void GuiPropertyComponent::renderProperty(properties::Property* prop,
                                          properties::PropertyOwner* owner)
{
    using Func = std::function<void(properties::Property*, const std::string&, IsRegularProperty)>;
    static const std::map<std::string, Func> FunctionMapping = {
        { "BoolProperty", &renderBoolProperty },
        { "DoubleProperty", &renderDoubleProperty},
        { "IntProperty", &renderIntProperty },
        { "IVec2Property", &renderIVec2Property },
        { "IVec3Property", &renderIVec3Property },
        { "IVec4Property", &renderIVec4Property },
        { "FloatProperty", &renderFloatProperty },
        { "Vec2Property", &renderVec2Property },
        { "Vec3Property", &renderVec3Property },
        { "Vec4Property", &renderVec4Property },
        { "DVec2Property", &renderDVec2Property },
        { "DVec3Property", &renderDVec3Property },
        { "DVec4Property", &renderDVec4Property },
        { "StringProperty", &renderStringProperty },
        { "OptionProperty", &renderOptionProperty },
        { "TriggerProperty", &renderTriggerProperty },
        { "SelectionProperty", &renderSelectionProperty }
    };

    // Check if the visibility of the property is high enough to be displayed
    using V = properties::Property::Visibility;
    auto v = static_cast<std::underlying_type_t<V>>(_visibility);
    auto propV = static_cast<std::underlying_type_t<V>>(prop->visibility());
    if (v >= propV) {
        auto it = FunctionMapping.find(prop->className());
        if (it != FunctionMapping.end()) {
            if (owner) {
                it->second(
                    prop,
                    owner->name(),
                    IsRegularProperty(_hasOnlyRegularProperties)
                );
            }
            else {
                it->second(
                    prop,
                    "",
                    IsRegularProperty(_hasOnlyRegularProperties)
                );
            }
        }
    }
}

} // namespace openspace::gui
