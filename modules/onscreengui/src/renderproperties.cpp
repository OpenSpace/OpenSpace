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

#include <modules/onscreengui/include/renderproperties.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/properties/scalarproperty.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/selectionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vectorproperty.h>
#include <openspace/scripting/scriptengine.h>

#include "imgui.h"

namespace openspace {

using namespace properties;

void renderTooltip(Property* prop) {
    if (ImGui::IsItemHovered()) {
        ImGui::SetTooltip(prop->fullyQualifiedIdentifier().c_str());
    }
}

void executeScript(const std::string& id, const std::string& value) {
    std::string script =
        "openspace.setPropertyValueSingle('" + id + "', " + value + ");";
    OsEng.scriptEngine().queueScript(script, scripting::ScriptEngine::RemoteScripting::Yes);
}

void renderBoolProperty(Property* prop, const std::string& ownerName) {
    BoolProperty* p = static_cast<BoolProperty*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    BoolProperty::ValueType value = *p;
    ImGui::Checkbox(name.c_str(), &value);
    renderTooltip(prop);

    if (value != p->value()) {
        executeScript(p->fullyQualifiedIdentifier(), value ? "true" : "false");
    }
    ImGui::PopID();
}

void renderOptionProperty(Property* prop, const std::string& ownerName) {
    OptionProperty* p = static_cast<OptionProperty*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    int value = *p;
    std::vector<OptionProperty::Option> options = p->options();
    switch (p->displayType()) {
    case OptionProperty::DisplayType::Radio: {
        ImGui::Text(name.c_str());
        ImGui::Separator();
        for (const OptionProperty::Option& o : options) {
            ImGui::RadioButton(o.description.c_str(), &value, o.value);
            renderTooltip(prop);
        }
        break;
    }
    case OptionProperty::DisplayType::Dropdown: {
        std::string nodeNames = "";
        for (const OptionProperty::Option& o : options) {
            nodeNames += o.description + '\0';
        }
        ImGui::Combo(name.c_str(), &value, nodeNames.c_str());
        break;
    }
    }
    if (value != p->value()) {
        executeScript(p->fullyQualifiedIdentifier(), std::to_string(value));
    }
    ImGui::PopID();
}

void renderSelectionProperty(Property* prop, const std::string& ownerName) {
    SelectionProperty* p = static_cast<SelectionProperty*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    if (ImGui::TreeNode(name.c_str())) {
        const std::vector<SelectionProperty::Option>& options = p->options();
        std::vector<int> newSelectedIndices;

        std::vector<int> selectedIndices = p->value();

        for (int i = 0; i < options.size(); ++i) {
            std::string description = options[i].description;
            bool selected = std::find(
                selectedIndices.begin(), selectedIndices.end(), i
            ) != selectedIndices.end();

            ImGui::Checkbox(description.c_str(), &selected);
            renderTooltip(prop);

            if (selected) {
                newSelectedIndices.push_back(i);
            }
        }

        if (newSelectedIndices != p->value()) {
            std::string parameters = "{";
            for (int i : newSelectedIndices) {
                parameters += std::to_string(i) + ",";
            }
            parameters += "}";
            executeScript(p->fullyQualifiedIdentifier(), parameters);
        }
        ImGui::TreePop();
    }
    ImGui::PopID();
}

void renderStringProperty(Property* prop, const std::string& ownerName) {
    StringProperty* p = static_cast<StringProperty*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    static const int bufferSize = 256;
    static char buffer[bufferSize];
#ifdef _MSC_VER
    strcpy_s(buffer, p->value().length() + 1, p->value().c_str());
#else
    strcpy(buffer, p->value().c_str());
#endif
    bool hasNewValue = ImGui::InputText(
        name.c_str(),
        buffer,
        bufferSize,
        ImGuiInputTextFlags_EnterReturnsTrue
    );
    renderTooltip(prop);


    if (hasNewValue) {
        executeScript(p->fullyQualifiedIdentifier(), "'" + std::string(buffer) + "'");
    }

    ImGui::PopID();
}

void renderDoubleProperty(properties::Property* prop, const std::string& ownerName) {
    DoubleProperty* p = static_cast<DoubleProperty*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    float value = *p;
    float min = p->minValue();
    float max = p->maxValue();

    ImGui::SliderFloat(name.c_str(), &value, min, max);
    renderTooltip(prop);

    if (value != static_cast<float>(p->value())) {
        executeScript(p->fullyQualifiedIdentifier(), std::to_string(value));
    }

    ImGui::PopID();
}

void renderIntProperty(Property* prop, const std::string& ownerName) {
    IntProperty* p = static_cast<IntProperty*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    IntProperty::ValueType value = *p;
    int min = p->minValue();
    int max = p->maxValue();
    
    ImGui::SliderInt(name.c_str(), &value, min, max);
    renderTooltip(prop);

    if (value != p->value()) {
        executeScript(p->fullyQualifiedIdentifier(), std::to_string(value));
    }

    ImGui::PopID();
}

void renderIVec2Property(Property* prop, const std::string& ownerName) {
    IVec2Property* p = static_cast<IVec2Property*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());
    
    IVec2Property::ValueType value = *p;
    float min = std::min(p->minValue().x, p->minValue().y);
    float max = std::max(p->maxValue().x, p->maxValue().y);
    ImGui::SliderInt2(
        name.c_str(),
        &value.x,
        min,
        max
    );
    renderTooltip(prop);
    
    if (value != p->value()) {
        executeScript(
            p->fullyQualifiedIdentifier(),
            "{" + std::to_string(value.x) + "," + std::to_string(value.y) + "}"
        );
    }
    
    ImGui::PopID();
}

void renderIVec3Property(Property* prop, const std::string& ownerName) {
    IVec3Property* p = static_cast<IVec3Property*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());
    
    IVec3Property::ValueType value = *p;
    float min = std::min(std::min(p->minValue().x, p->minValue().y), p->minValue().z);
    float max = std::max(std::max(p->maxValue().x, p->maxValue().y), p->maxValue().z);
    
    ImGui::SliderInt3(
        name.c_str(),
        &value.x,
        min,
        max
    );
    renderTooltip(prop);
    
    if (value != p->value()) {
        executeScript(
            p->fullyQualifiedIdentifier(),
            "{" + std::to_string(value.x) + "," + std::to_string(value.y) + "," +
            std::to_string(value.z) + "}"
        );
    }
    ImGui::PopID();
}

void renderIVec4Property(Property* prop, const std::string& ownerName) {
    IVec4Property* p = static_cast<IVec4Property*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());
    
    IVec4Property::ValueType value = *p;
    float min = std::min(std::min(std::min(
        p->minValue().x, p->minValue().y), p->minValue().z), p->minValue().w
    );
    float max = std::max(std::max(std::max(
        p->maxValue().x, p->maxValue().y), p->maxValue().z), p->maxValue().w
    );
    
    ImGui::SliderInt4(
        name.c_str(),
        &value.x,
        min,
        max
    );
    renderTooltip(prop);
    
    if (value != p->value()) {
        executeScript(
            p->fullyQualifiedIdentifier(),
            "{" + std::to_string(value.x) + "," +
            std::to_string(value.y) + "," +
            std::to_string(value.z) + "," +
            std::to_string(value.w) + "}"
        );
    }
    ImGui::PopID();
}

void renderFloatProperty(Property* prop, const std::string& ownerName) {
    FloatProperty* p = static_cast<FloatProperty*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    FloatProperty::ValueType value = *p;
    float min = p->minValue();
    float max = p->maxValue();
    ImGui::SliderFloat(name.c_str(), &value, min, max);
    renderTooltip(prop);

    if (value != p->value()) {
        executeScript(p->fullyQualifiedIdentifier(), std::to_string(value));
    }

    ImGui::PopID();
}

void renderVec2Property(Property* prop, const std::string& ownerName) {
    Vec2Property* p = static_cast<Vec2Property*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    Vec2Property::ValueType value = *p;
    float min = std::min(p->minValue().x, p->minValue().y);
    float max = std::max(p->maxValue().x, p->maxValue().y);
    ImGui::SliderFloat2(
        name.c_str(),
        &value.x,
        min,
        max
    );
    renderTooltip(prop);

    if (value != p->value()) {
        executeScript(
            p->fullyQualifiedIdentifier(),
            "{" + std::to_string(value.x) + "," + std::to_string(value.y) + "}"
        );
    }

    ImGui::PopID();
}

void renderVec3Property(Property* prop, const std::string& ownerName) {
    Vec3Property* p = static_cast<Vec3Property*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    Vec3Property::ValueType value = *p;
    float min = std::min(std::min(p->minValue().x, p->minValue().y), p->minValue().z);
    float max = std::max(std::max(p->maxValue().x, p->maxValue().y), p->maxValue().z);


    ImGui::SliderFloat3(
        name.c_str(),
        glm::value_ptr(value),
        min,
        max
    );
    renderTooltip(prop);

    if (value != p->value()) {
        executeScript(
            p->fullyQualifiedIdentifier(),
            "{" + std::to_string(value.x) + "," +
            std::to_string(value.y) + "," +
            std::to_string(value.z) + "}"
        );
    }

    ImGui::PopID();
}

void renderVec4Property(Property* prop, const std::string& ownerName) {
    Vec4Property* p = static_cast<Vec4Property*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    Vec4Property::ValueType value = *p;
    float min = std::min(std::min(std::min(
        p->minValue().x, p->minValue().y), p->minValue().z), p->minValue().w);
    float max = std::max(std::max(std::max(
        p->maxValue().x, p->maxValue().y), p->maxValue().z), p->maxValue().w);

    ImGui::SliderFloat4(
        name.c_str(),
        &value.x,
        min,
        max
    );
    renderTooltip(prop);

    if (value != p->value()) {
        executeScript(
            p->fullyQualifiedIdentifier(),
            "{" + std::to_string(value.x) + "," +
            std::to_string(value.y) + "," +
            std::to_string(value.z) + "," +
            std::to_string(value.w) + "}"
        );
    }

    ImGui::PopID();
}

void renderTriggerProperty(Property* prop, const std::string& ownerName) {
    std::string name = prop->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    bool pressed = ImGui::Button(name.c_str());
    if (pressed) {
        executeScript(prop->fullyQualifiedIdentifier(), "nil");
    }
    renderTooltip(prop);

    ImGui::PopID();
}

} // namespace openspace
