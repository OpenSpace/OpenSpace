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

#include <modules/imgui/include/renderproperties.h>

#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/selectionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/stringlistproperty.h>
#include <openspace/properties/matrix/dmat2property.h>
#include <openspace/properties/matrix/dmat3property.h>
#include <openspace/properties/matrix/dmat4property.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/vector/dvec2property.h>
#include <openspace/properties/vector/dvec3property.h>
#include <openspace/properties/vector/dvec4property.h>
#include <openspace/properties/vector/ivec2property.h>
#include <openspace/properties/vector/ivec3property.h>
#include <openspace/properties/vector/ivec4property.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/vector/vec4property.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/misc.h>

namespace openspace {

using namespace properties;

void renderTooltip(Property* prop, double delay) {
    if (ImGui::IsItemHovered() && (GImGui->HoveredIdTimer > delay)) {
        ImGui::BeginTooltip();
        if (!prop->description().empty()) {
            ImGui::TextWrapped("%s", prop->description().c_str());
            ImGui::Spacing();
        }
        ImGui::Text(
            "%s",
            (std::string("Identifier: ") + prop->fullyQualifiedIdentifier()).c_str()
        );
        ImGui::EndTooltip();
    }
}

void executeScriptSingle(const std::string& id, const std::string& value) {
    global::scriptEngine.queueScript(
        "openspace.setPropertyValueSingle('" + id + "', " + value + ");",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void executeScriptGroup(const std::string& id, const std::string& value) {
    global::scriptEngine.queueScript(
        "openspace.setPropertyValue('" + id + "', " + value + ");",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void executeScript(const std::string& id, const std::string& value,
                   IsRegularProperty isRegular)
{
    if (isRegular) {
        executeScriptSingle(id, value);
    }
    else {
        executeScriptGroup(id, value);
    }
}

void renderBoolProperty(Property* prop, const std::string& ownerName,
                        IsRegularProperty isRegular, ShowToolTip showTooltip,
                        double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");

    BoolProperty* p = static_cast<BoolProperty*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    BoolProperty::ValueType value = *p;
    ImGui::Checkbox(name.c_str(), &value);
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (value != p->value()) {
        executeScript(p->fullyQualifiedIdentifier(), value ? "true" : "false", isRegular);
    }
    ImGui::PopID();
}

void renderOptionProperty(Property* prop, const std::string& ownerName,
                          IsRegularProperty isRegular, ShowToolTip showTooltip,
                          double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");

    OptionProperty* p = static_cast<OptionProperty*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());
    bool isReadOnly = false;
    p->metaData().getValue("isReadOnly", isReadOnly);

    int value = *p;
    const std::vector<OptionProperty::Option>& options = p->options();
    switch (p->displayType()) {
    case OptionProperty::DisplayType::Radio: {
        ImGui::Text("%s", name.c_str());
        ImGui::Separator();
        for (const OptionProperty::Option& o : options) {
            ImGui::RadioButton(o.description.c_str(), &value, o.value);
            if (showTooltip) {
                renderTooltip(prop, tooltipDelay);
            }
        }
        ImGui::Separator();
        break;
    }
    case OptionProperty::DisplayType::Dropdown: {
        // The order of the options does not have to correspond with the value of the
        // option
        std::string nodeNames;
        for (const OptionProperty::Option& o : options) {
            nodeNames += o.description + '\0';
        }
        nodeNames += '\0';

        int idx = static_cast<int>(std::distance(
            options.begin(),
            std::find_if(
                options.begin(),
                options.end(),
                [value](const OptionProperty::Option& o) { return o.value == value; }
        )));

        const bool hasChanged = ImGui::Combo(name.c_str(), &idx, nodeNames.c_str());
        if (showTooltip) {
            renderTooltip(prop, tooltipDelay);
        }

        if (hasChanged) {
            value = options[idx].value;
        }

        break;
    }
    }
    if (value != p->value() && !isReadOnly) {
        executeScript(p->fullyQualifiedIdentifier(), std::to_string(value), isRegular);
    }
    ImGui::PopID();
}

void renderSelectionProperty(Property* prop, const std::string& ownerName,
                             IsRegularProperty isRegular, ShowToolTip showTooltip,
                             double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    SelectionProperty* p = static_cast<SelectionProperty*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    if (ImGui::TreeNode(name.c_str())) {
        const std::vector<SelectionProperty::Option>& options = p->options();
        std::vector<int> newSelectedIndices;

        std::vector<int> selectedIndices = p->value();

        for (int i = 0; i < static_cast<int>(options.size()); ++i) {
            std::string description = options[i].description;
            bool selected = std::find(
                selectedIndices.begin(), selectedIndices.end(), i
            ) != selectedIndices.end();

            ImGui::Checkbox(description.c_str(), &selected);
            if (showTooltip) {
                renderTooltip(prop, tooltipDelay);
            }

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
            executeScript(p->fullyQualifiedIdentifier(), parameters, isRegular);
        }
        ImGui::TreePop();
    }
    ImGui::PopID();
}

void renderStringProperty(Property* prop, const std::string& ownerName,
                          IsRegularProperty isRegular, ShowToolTip showTooltip,
                          double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    StringProperty* p = static_cast<StringProperty*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    const std::string value = p->value();

    static const int bufferSize = 256;
    static char buffer[bufferSize];
#ifdef _MSC_VER
    strcpy_s(buffer, value.length() + 1, value.c_str());
#else
    strcpy(buffer, value.c_str());
#endif
    bool hasNewValue = ImGui::InputText(
        name.c_str(),
        buffer,
        bufferSize,
        ImGuiInputTextFlags_EnterReturnsTrue
    );
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (hasNewValue) {
        executeScript(
            p->fullyQualifiedIdentifier(),
            "[[" + std::string(buffer) + "]]",
            isRegular
        );
    }

    ImGui::PopID();
}

void renderStringListProperty(Property* prop, const std::string& ownerName,
                              IsRegularProperty isRegular, ShowToolTip showTooltip,
                              double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    StringListProperty* p = static_cast<StringListProperty*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    std::string value;
    p->getStringValue(value);
    // const std::string value = p->value();

    static const int bufferSize = 512;
    static char buffer[bufferSize];
#ifdef _MSC_VER
    strcpy_s(buffer, value.length() + 1, value.c_str());
#else
    strcpy(buffer, value.c_str());
#endif
    bool hasNewValue = ImGui::InputText(
        name.c_str(),
        buffer,
        bufferSize,
        ImGuiInputTextFlags_EnterReturnsTrue
    );
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (hasNewValue) {
        std::vector<std::string> tokens = ghoul::tokenizeString(std::string(buffer), ',');
        std::string script = "{";
        for (std::string& token : tokens) {
            if (!token.empty()) {
                ghoul::trimWhitespace(token);
                script += "[[" + token + "]],";
            }
        }
        script += "}";

        executeScript(
            p->fullyQualifiedIdentifier(),
            std::move(script),
            isRegular
        );
    }

    ImGui::PopID();
}

void renderDoubleProperty(properties::Property* prop, const std::string& ownerName,
                          IsRegularProperty isRegular, ShowToolTip showTooltip,
                          double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    DoubleProperty* p = static_cast<DoubleProperty*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    float value = static_cast<float>(*p);
    float min = static_cast<float>(p->minValue());
    float max = static_cast<float>(p->maxValue());

    bool changed = ImGui::SliderFloat(
        name.c_str(),
        &value,
        min,
        max,
        "%.5f",
        p->exponent()
    );
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeScript(p->fullyQualifiedIdentifier(), std::to_string(value), isRegular);
    }

    ImGui::PopID();
}

void renderIntProperty(Property* prop, const std::string& ownerName,
                       IsRegularProperty isRegular, ShowToolTip showTooltip,
                       double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    IntProperty* p = static_cast<IntProperty*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    IntProperty::ValueType value = *p;
    int min = p->minValue();
    int max = p->maxValue();

    bool changed = ImGui::SliderInt(name.c_str(), &value, min, max);
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeScript(p->fullyQualifiedIdentifier(), std::to_string(value), isRegular);
    }

    ImGui::PopID();
}

void renderIVec2Property(Property* prop, const std::string& ownerName,
                         IsRegularProperty isRegular, ShowToolTip showTooltip,
                         double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    IVec2Property* p = static_cast<IVec2Property*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    IVec2Property::ValueType value = *p;
    int min = glm::compMin(p->minValue());
    int max = glm::compMax(p->maxValue());
    bool changed = ImGui::SliderInt2(
        name.c_str(),
        &value.x,
        min,
        max
    );
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeScript(
            p->fullyQualifiedIdentifier(),
            std::to_string(value),
            isRegular
        );
    }

    ImGui::PopID();
}

void renderIVec3Property(Property* prop, const std::string& ownerName,
                         IsRegularProperty isRegular, ShowToolTip showTooltip,
                         double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    IVec3Property* p = static_cast<IVec3Property*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    IVec3Property::ValueType value = *p;
    int min = glm::compMin(p->minValue());
    int max = glm::compMax(p->maxValue());

    bool changed = ImGui::SliderInt3(
        name.c_str(),
        &value.x,
        min,
        max
    );
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeScript(
            p->fullyQualifiedIdentifier(),
            std::to_string(value),
            isRegular
        );
    }
    ImGui::PopID();
}

void renderIVec4Property(Property* prop, const std::string& ownerName,
                         IsRegularProperty isRegular, ShowToolTip showTooltip,
                         double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    IVec4Property* p = static_cast<IVec4Property*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    IVec4Property::ValueType value = *p;
    int min = glm::compMin(p->minValue());
    int max = glm::compMax(p->maxValue());

    bool changed = ImGui::SliderInt4(
        name.c_str(),
        &value.x,
        min,
        max
    );
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeScript(
            p->fullyQualifiedIdentifier(),
            std::to_string(value),
            isRegular
        );
    }
    ImGui::PopID();
}

void renderFloatProperty(Property* prop, const std::string& ownerName,
                         IsRegularProperty isRegular, ShowToolTip showTooltip,
                         double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    FloatProperty* p = static_cast<FloatProperty*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    FloatProperty::ValueType value = *p;
    float min = p->minValue();
    float max = p->maxValue();
    bool changed = ImGui::SliderFloat(
        name.c_str(),
        &value,
        min,
        max,
        "%.5f",
        p->exponent()
    );
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeScript(p->fullyQualifiedIdentifier(), std::to_string(value), isRegular);
    }

    ImGui::PopID();
}

void renderVec2Property(Property* prop, const std::string& ownerName,
                        IsRegularProperty isRegular, ShowToolTip showTooltip,
                        double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    Vec2Property* p = static_cast<Vec2Property*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    Vec2Property::ValueType value = *p;
    float min = glm::compMin(p->minValue());
    float max = glm::compMax(p->maxValue());

    bool changed = ImGui::SliderFloat2(
        name.c_str(),
        &value.x,
        min,
        max,
        "%.5f",
        p->exponent()
    );
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeScript(
            p->fullyQualifiedIdentifier(),
            std::to_string(value),
            isRegular
        );
    }

    ImGui::PopID();
}

void renderVec3Property(Property* prop, const std::string& ownerName,
                        IsRegularProperty isRegular, ShowToolTip showTooltip,
                        double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    Vec3Property* p = static_cast<Vec3Property*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    Vec3Property::ValueType value = *p;
    float min = glm::compMin(p->minValue());
    float max = glm::compMax(p->maxValue());

    bool changed = false;
    if (prop->viewOption(Property::ViewOptions::Color)) {
        changed = ImGui::ColorEdit3(
            name.c_str(),
            glm::value_ptr(value)
        );
    }
    else {
        changed = ImGui::SliderFloat3(
            name.c_str(),
            glm::value_ptr(value),
            min,
            max,
            "%.5f",
            p->exponent()
        );
    }
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeScript(
            p->fullyQualifiedIdentifier(),
            std::to_string(value),
            isRegular
        );
    }

    ImGui::PopID();
}

void renderVec4Property(Property* prop, const std::string& ownerName,
                        IsRegularProperty isRegular, ShowToolTip showTooltip,
                        double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    Vec4Property* p = static_cast<Vec4Property*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    Vec4Property::ValueType value = *p;
    float min = glm::compMin(p->minValue());
    float max = glm::compMax(p->maxValue());

    bool changed = false;
    if (prop->viewOption(Property::ViewOptions::Color)) {
        changed = ImGui::ColorEdit4(
            name.c_str(),
            glm::value_ptr(value)
        );
    }
    else {
        changed = ImGui::SliderFloat4(
            name.c_str(),
            glm::value_ptr(value),
            min,
            max,
            "%.5f",
            p->exponent()
        );
    }
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeScript(
            p->fullyQualifiedIdentifier(),
            std::to_string(value),
            isRegular
        );
    }

    ImGui::PopID();
}

void renderDVec2Property(Property* prop, const std::string& ownerName,
                         IsRegularProperty isRegular, ShowToolTip showTooltip,
                         double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    DVec2Property* p = static_cast<DVec2Property*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    glm::vec2 value = glm::dvec2(*p);
    float min = static_cast<float>(glm::compMin(p->minValue()));
    float max = static_cast<float>(glm::compMax(p->maxValue()));
    bool changed = ImGui::SliderFloat2(
        name.c_str(),
        &value.x,
        min,
        max,
        "%.5f",
        p->exponent()
    );
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeScript(
            p->fullyQualifiedIdentifier(),
            std::to_string(value),
            isRegular
        );
    }

    ImGui::PopID();
}

void renderDVec3Property(Property* prop, const std::string& ownerName,
                         IsRegularProperty isRegular, ShowToolTip showTooltip,
                         double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    DVec3Property* p = static_cast<DVec3Property*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    glm::vec3 value = glm::dvec3(*p);
    float min = static_cast<float>(glm::compMin(p->minValue()));
    float max = static_cast<float>(glm::compMax(p->maxValue()));

    bool changed = ImGui::SliderFloat3(
        name.c_str(),
        glm::value_ptr(value),
        min,
        max,
        "%.5f",
        p->exponent()
    );
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeScript(
            p->fullyQualifiedIdentifier(),
            std::to_string(value),
            isRegular
        );
    }

    ImGui::PopID();
}

void renderDVec4Property(Property* prop, const std::string& ownerName,
                         IsRegularProperty isRegular, ShowToolTip showTooltip,
                         double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    DVec4Property* p = static_cast<DVec4Property*>(prop);
    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    glm::vec4 value = glm::dvec4(*p);
    float min = static_cast<float>(glm::compMin(p->minValue()));
    float max = static_cast<float>(glm::compMax(p->maxValue()));

    bool changed = ImGui::SliderFloat4(
        name.c_str(),
        &value.x,
        min,
        max,
        "%.5f",
        p->exponent()
    );
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeScript(
            p->fullyQualifiedIdentifier(),
            std::to_string(value),
            isRegular
        );
    }

    ImGui::PopID();
}

void renderDMat2Property(Property* prop, const std::string& ownerName,
                         IsRegularProperty isRegular, ShowToolTip showTooltip,
                         double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    DMat2Property* p = static_cast<DMat2Property*>(prop);

    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());
    ImGui::Text("%s", name.c_str());

    glm::mat2 value = glm::dmat2(*p);
    glm::dvec2 minValues = {
        glm::compMin(p->minValue()[0]),
        glm::compMin(p->minValue()[1])
    };
    float min = static_cast<float>(glm::compMin(minValues));

    glm::dvec2 maxValues = {
        glm::compMax(p->maxValue()[0]),
        glm::compMax(p->maxValue()[1])
    };
    float max = static_cast<float>(glm::compMax(maxValues));

    bool changed = false;
    changed |= ImGui::SliderFloat2(
        "[0]",
        glm::value_ptr(value[0]),
        min,
        max,
        "%.5f",
        p->exponent()
    );
    changed |= ImGui::SliderFloat2(
        "[1]",
        glm::value_ptr(value[1]),
        min,
        max,
        "%.5f",
        p->exponent()
    );

    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeScript(
            p->fullyQualifiedIdentifier(),
            std::to_string(value),
            isRegular
        );
    }

    ImGui::PopID();
}

void renderDMat3Property(Property* prop, const std::string& ownerName,
                         IsRegularProperty isRegular, ShowToolTip showTooltip,
                         double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    DMat3Property* p = static_cast<DMat3Property*>(prop);

    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());
    ImGui::Text("%s", name.c_str());

    glm::mat3 value = glm::dmat3(*p);
    glm::dvec3 minValues = {
        glm::compMin(p->minValue()[0]),
        glm::compMin(p->minValue()[1]),
        glm::compMin(p->minValue()[2])
    };
    float min = static_cast<float>(glm::compMin(minValues));

    glm::dvec3 maxValues = {
        glm::compMax(p->maxValue()[0]),
        glm::compMax(p->maxValue()[1]),
        glm::compMax(p->maxValue()[2])
    };
    float max = static_cast<float>(glm::compMax(maxValues));

    bool changed = false;
    changed |= ImGui::SliderFloat3(
        "[0]",
        &value[0].x,
        min,
        max,
        "%.5f",
        p->exponent()
    );
    changed |= ImGui::SliderFloat3(
        "[1]",
        &value[1].x,
        min,
        max,
        "%.5f",
        p->exponent()
    );
    changed |= ImGui::SliderFloat3(
        "[2]",
        &value[2].x,
        min,
        max,
        "%.5f",
        p->exponent()
    );

    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeScript(
            p->fullyQualifiedIdentifier(),
            std::to_string(value),
            isRegular
        );
    }

    ImGui::PopID();
}

void renderDMat4Property(Property* prop, const std::string& ownerName,
                         IsRegularProperty isRegular, ShowToolTip showTooltip,
                         double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    DMat4Property* p = static_cast<DMat4Property*>(prop);

    std::string name = p->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());
    ImGui::Text("%s", name.c_str());

    glm::mat4 value = glm::dmat4(*p);
    glm::dvec4 minValues = {
        glm::compMin(p->minValue()[0]),
        glm::compMin(p->minValue()[1]),
        glm::compMin(p->minValue()[2]),
        glm::compMin(p->minValue()[3])
    };
    float min = static_cast<float>(glm::compMin(minValues));

    glm::dvec4 maxValues = {
        glm::compMax(p->maxValue()[0]),
        glm::compMax(p->maxValue()[1]),
        glm::compMax(p->maxValue()[2]),
        glm::compMax(p->maxValue()[3])
    };
    float max = static_cast<float>(glm::compMax(maxValues));

    bool changed = false;
    changed |= ImGui::SliderFloat4(
        "[0]",
        &value[0].x,
        min,
        max,
        "%.5f",
        p->exponent()
    );
    changed |= ImGui::SliderFloat4(
        "[1]",
        &value[1].x,
        min,
        max,
        "%.5f",
        p->exponent()
    );
    changed |= ImGui::SliderFloat4(
        "[2]",
        &value[2].x,
        min,
        max,
        "%.5f",
        p->exponent()
    );
    changed |= ImGui::SliderFloat4(
        "[3]",
        &value[3].x,
        min,
        max,
        "%.5f",
        p->exponent()
    );

    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeScript(
            p->fullyQualifiedIdentifier(),
            std::to_string(value),
            isRegular
        );
    }

    ImGui::PopID();
}

void renderTriggerProperty(Property* prop, const std::string& ownerName,
                           IsRegularProperty isRegular, ShowToolTip showTooltip,
                           double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    std::string name = prop->guiName();
    ImGui::PushID((ownerName + "." + name).c_str());

    bool pressed = ImGui::Button(name.c_str());
    if (pressed) {
        executeScript(prop->fullyQualifiedIdentifier(), "nil", isRegular);
    }
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    ImGui::PopID();
}

} // namespace openspace
