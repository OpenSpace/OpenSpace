/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <openspace/properties/list/doublelistproperty.h>
#include <openspace/properties/list/intlistproperty.h>
#include <openspace/properties/list/stringlistproperty.h>
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
#include <ghoul/misc/dictionaryluaformatter.h>
#include <ghoul/misc/stringhelper.h>

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
            (std::string("Identifier: ") + prop->uri()).c_str()
        );
        ImGui::EndTooltip();
    }
}

void executeSetPropertyScript(const std::string& id, const std::string& value) {
    global::scriptEngine->queueScript(
        std::format("openspace.setPropertyValueSingle('{}', {});", id, value),
        scripting::ScriptEngine::ShouldBeSynchronized::Yes,
        scripting::ScriptEngine::ShouldSendToRemote::Yes
    );
}

void renderBoolProperty(Property* prop, const std::string& ownerName,
                        ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");

    BoolProperty* p = static_cast<BoolProperty*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());

    BoolProperty::ValueType value = *p;
    ImGui::Checkbox(name.c_str(), &value);
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (value != p->value()) {
        executeSetPropertyScript(p->uri(), value ? "true" : "false");
    }
    ImGui::PopID();
}

void renderOptionProperty(Property* prop, const std::string& ownerName,
                          ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");

    OptionProperty* p = static_cast<OptionProperty*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());
    bool isReadOnly = false;
    if (p->metaData().hasValue<bool>("isReadOnly")) {
        isReadOnly = p->metaData().value<bool>("isReadOnly");
    }

    int value = *p;
    const std::vector<OptionProperty::Option>& options = p->options();
    switch (p->displayType()) {
        case OptionProperty::DisplayType::Radio:
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
                )
            ));

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
        executeSetPropertyScript(p->uri(), std::to_string(value));
    }
    ImGui::PopID();
}

void renderSelectionProperty(Property* prop, const std::string& ownerName,
                             ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    SelectionProperty* p = static_cast<SelectionProperty*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());

    if (ImGui::TreeNode(name.c_str())) {
        bool selectionChanged = false;
        std::set<std::string> newSelected;

        const std::set<std::string> selected = p->value();
        const std::vector<std::string>& options = p->options();

        for (const std::string& key : options) {
            bool isSelected = p->isSelected(key);

            selectionChanged |= ImGui::Checkbox(key.c_str(), &isSelected);
            if (showTooltip) {
                renderTooltip(prop, tooltipDelay);
            }

            if (isSelected) {
                newSelected.insert(key);
            }
        }

        if (selectionChanged) {
            std::string parameters = "{";
            for (const std::string& s : newSelected) {
                parameters += std::format("'{}',", s);
            }
            if (!newSelected.empty()) {
                parameters.pop_back();
            }
            parameters += "}";
            executeSetPropertyScript(p->uri(), parameters);
        }
        ImGui::TreePop();
    }
    ImGui::PopID();
}

void renderStringProperty(Property* prop, const std::string& ownerName,
                          ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    StringProperty* p = static_cast<StringProperty*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());

    const std::string value = p->value();

    static constexpr int BufferSize = 256;
    static std::array<char, BufferSize> buffer;
#ifdef _MSC_VER
    strcpy_s(buffer.data(), value.length() + 1, value.c_str());
#else
    strcpy(buffer.data(), value.c_str());
#endif
    const bool hasNewValue = ImGui::InputText(name.c_str(), buffer.data(), BufferSize);
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (hasNewValue) {
        executeSetPropertyScript(
            p->uri(),
            "[[" + std::string(buffer.data()) + "]]"
        );
    }

    ImGui::PopID();
}

void renderListProperty(const std::string& name, const std::string& fullIdentifier,
                        const std::string& stringValue)
{
    ghoul_assert(
        stringValue.size() > 2,
        "an empty list should have the string value '[]'"
    );

    // Remove brackets from the string value
    const std::string value = stringValue.substr(1, stringValue.size() - 2);

    static constexpr int BufferSize = 512;
    static std::array<char, BufferSize> buffer;
#ifdef _MSC_VER
    strcpy_s(buffer.data(), value.length() + 1, value.c_str());
#else
    strcpy(buffer.data(), value.c_str());
#endif

    const bool hasNewValue = ImGui::InputText(name.c_str(), buffer.data(), BufferSize);
    if (hasNewValue) {
        std::vector<std::string> tokens = ghoul::tokenizeString(buffer.data(), ',');
        std::string script = "{";
        for (std::string& token : tokens) {
            if (!token.empty()) {
                ghoul::trimWhitespace(token);
                script += token + ',';
            }
        }
        script += '}';

        executeSetPropertyScript(fullIdentifier, script);
    }
}

void renderDoubleListProperty(Property* prop, const std::string& ownerName,
                              ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    DoubleListProperty* p = static_cast<DoubleListProperty*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());

    const std::string value = p->stringValue();
    renderListProperty(name, p->uri(), value);

    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    ImGui::PopID();
}

void renderIntListProperty(Property* prop, const std::string& ownerName,
                           ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    IntListProperty* p = static_cast<IntListProperty*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());

    const std::string value = p->stringValue();
    renderListProperty(name, p->uri(), value);

    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    ImGui::PopID();
}

void renderStringListProperty(Property* prop, const std::string& ownerName,
                              ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    StringListProperty* p = static_cast<StringListProperty*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());

    const std::string value = p->stringValue();
    renderListProperty(name, p->uri(), value);

    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    ImGui::PopID();
}

void renderDoubleProperty(properties::Property* prop, const std::string& ownerName,
                          ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    DoubleProperty* p = static_cast<DoubleProperty*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());

    float value = static_cast<float>(*p);
    float min = static_cast<float>(p->minValue());
    float max = static_cast<float>(p->maxValue());

    // Since we are doing a DoubleProperty, it would actually overflow here and produce
    // -inf and inf as the min and max which confuses ImGui
    min = std::max(min, std::numeric_limits<float>::min() / 2.f);
    max = std::min(max, std::numeric_limits<float>::max() / 2.f);

    const bool changed = ImGui::SliderFloat(
        name.c_str(),
        &value,
        min,
        max,
        "%.5f",
        ImGuiSliderFlags_Logarithmic
    );
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeSetPropertyScript(p->uri(), std::to_string(value));
    }

    ImGui::PopID();
}

void renderIntProperty(Property* prop, const std::string& ownerName,
                       ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    IntProperty* p = static_cast<IntProperty*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());

    IntProperty::ValueType value = *p;
    const int min = p->minValue();
    const int max = p->maxValue();

    const bool changed = ImGui::SliderInt(name.c_str(), &value, min, max);
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeSetPropertyScript(p->uri(), std::to_string(value));
    }

    ImGui::PopID();
}

void renderIVec2Property(Property* prop, const std::string& ownerName,
                         ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    IVec2Property* p = static_cast<IVec2Property*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());

    IVec2Property::ValueType value = *p;
    const int min = glm::compMin(p->minValue());
    const int max = glm::compMax(p->maxValue());
    const bool changed = ImGui::SliderInt2(name.c_str(), &value.x, min, max);
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeSetPropertyScript(p->uri(), ghoul::to_string(value));
    }

    ImGui::PopID();
}

void renderIVec3Property(Property* prop, const std::string& ownerName,
                         ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    IVec3Property* p = static_cast<IVec3Property*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());

    IVec3Property::ValueType value = *p;
    const int min = glm::compMin(p->minValue());
    const int max = glm::compMax(p->maxValue());
    const bool changed = ImGui::SliderInt3(name.c_str(), &value.x, min, max);
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeSetPropertyScript(p->uri(), ghoul::to_string(value));
    }
    ImGui::PopID();
}

void renderIVec4Property(Property* prop, const std::string& ownerName,
                         ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    IVec4Property* p = static_cast<IVec4Property*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());

    IVec4Property::ValueType value = *p;
    const int min = glm::compMin(p->minValue());
    const int max = glm::compMax(p->maxValue());
    const bool changed = ImGui::SliderInt4(name.c_str(), &value.x, min, max);
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeSetPropertyScript(p->uri(), ghoul::to_string(value));
    }
    ImGui::PopID();
}

void renderFloatProperty(Property* prop, const std::string& ownerName,
                         ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    FloatProperty* p = static_cast<FloatProperty*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());

    FloatProperty::ValueType value = *p;
    const float min = p->minValue();
    const float max = p->maxValue();
    const bool changed = ImGui::SliderFloat(
        name.c_str(),
        &value,
        min,
        max,
        "%.5f",
        ImGuiSliderFlags_Logarithmic
    );
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeSetPropertyScript(p->uri(), std::to_string(value));
    }

    ImGui::PopID();
}

void renderVec2Property(Property* prop, const std::string& ownerName,
                        ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    Vec2Property* p = static_cast<Vec2Property*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());

    Vec2Property::ValueType value = *p;
    const float min = glm::compMin(p->minValue());
    const float max = glm::compMax(p->maxValue());
    const bool changed = ImGui::SliderFloat2(
        name.c_str(),
        &value.x,
        min,
        max,
        "%.5f",
        ImGuiSliderFlags_Logarithmic
    );
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeSetPropertyScript(p->uri(), ghoul::to_string(value));
    }

    ImGui::PopID();
}

void renderVec3Property(Property* prop, const std::string& ownerName,
                        ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    Vec3Property* p = static_cast<Vec3Property*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());

    Vec3Property::ValueType value = *p;
    const float min = glm::compMin(p->minValue());
    const float max = glm::compMax(p->maxValue());
    bool changed = false;
    if (prop->viewOption(Property::ViewOptions::Color)) {
        changed = ImGui::ColorEdit3(name.c_str(), glm::value_ptr(value));
    }
    else {
        changed = ImGui::SliderFloat3(
            name.c_str(),
            glm::value_ptr(value),
            min,
            max,
            "%.5f",
            ImGuiSliderFlags_Logarithmic
        );
    }
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeSetPropertyScript(p->uri(), ghoul::to_string(value));
    }

    ImGui::PopID();
}

void renderVec4Property(Property* prop, const std::string& ownerName,
                        ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    Vec4Property* p = static_cast<Vec4Property*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());

    Vec4Property::ValueType value = *p;
    const float min = glm::compMin(p->minValue());
    const float max = glm::compMax(p->maxValue());
    bool changed = false;
    if (prop->viewOption(Property::ViewOptions::Color)) {
        changed = ImGui::ColorEdit4(name.c_str(), glm::value_ptr(value));
    }
    else {
        changed = ImGui::SliderFloat4(
            name.c_str(),
            glm::value_ptr(value),
            min,
            max,
            "%.5f",
            ImGuiSliderFlags_Logarithmic
        );
    }
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeSetPropertyScript(p->uri(), ghoul::to_string(value));
    }

    ImGui::PopID();
}

void renderDVec2Property(Property* prop, const std::string& ownerName,
                         ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    DVec2Property* p = static_cast<DVec2Property*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());

    glm::vec2 value = glm::dvec2(*p);
    const float min = static_cast<float>(glm::compMin(p->minValue()));
    const float max = static_cast<float>(glm::compMax(p->maxValue()));
    const bool changed = ImGui::SliderFloat2(
        name.c_str(),
        &value.x,
        min,
        max,
        "%.5f",
        ImGuiSliderFlags_Logarithmic
    );
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeSetPropertyScript(p->uri(), ghoul::to_string(value));
    }

    ImGui::PopID();
}

void renderDVec3Property(Property* prop, const std::string& ownerName,
                         ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    DVec3Property* p = static_cast<DVec3Property*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());

    glm::vec3 value = glm::dvec3(*p);
    const float min = static_cast<float>(glm::compMin(p->minValue()));
    const float max = static_cast<float>(glm::compMax(p->maxValue()));
    const bool changed = ImGui::SliderFloat3(
        name.c_str(),
        glm::value_ptr(value),
        min,
        max,
        "%.5f",
        ImGuiSliderFlags_Logarithmic
    );
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeSetPropertyScript(p->uri(), ghoul::to_string(value));
    }

    ImGui::PopID();
}

void renderDVec4Property(Property* prop, const std::string& ownerName,
                         ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    DVec4Property* p = static_cast<DVec4Property*>(prop);
    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());

    glm::vec4 value = glm::dvec4(*p);
    const float min = static_cast<float>(glm::compMin(p->minValue()));
    const float max = static_cast<float>(glm::compMax(p->maxValue()));
    const bool changed = ImGui::SliderFloat4(
        name.c_str(),
        &value.x,
        min,
        max,
        "%.5f",
        ImGuiSliderFlags_Logarithmic
    );
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeSetPropertyScript(p->uri(), ghoul::to_string(value));
    }

    ImGui::PopID();
}

void renderDMat2Property(Property* prop, const std::string& ownerName,
                         ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    DMat2Property* p = static_cast<DMat2Property*>(prop);

    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());
    ImGui::Text("%s", name.c_str());

    glm::mat2 value = glm::dmat2(*p);
    const glm::dvec2 minValues = glm::dvec2(
        glm::compMin(p->minValue()[0]),
        glm::compMin(p->minValue()[1])
    );
    const float min = static_cast<float>(glm::compMin(minValues));

    const glm::dvec2 maxValues = glm::dvec2(
        glm::compMax(p->maxValue()[0]),
        glm::compMax(p->maxValue()[1])
    );
    const float max = static_cast<float>(glm::compMax(maxValues));

    bool changed = false;
    changed |= ImGui::SliderFloat2(
        "[0]",
        glm::value_ptr(value[0]),
        min,
        max,
        "%.5f",
        ImGuiSliderFlags_Logarithmic
    );
    changed |= ImGui::SliderFloat2(
        "[1]",
        glm::value_ptr(value[1]),
        min,
        max,
        "%.5f",
        ImGuiSliderFlags_Logarithmic
    );

    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeSetPropertyScript(p->uri(), ghoul::to_string(value));
    }

    ImGui::PopID();
}

void renderDMat3Property(Property* prop, const std::string& ownerName,
                         ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    DMat3Property* p = static_cast<DMat3Property*>(prop);

    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());
    ImGui::Text("%s", name.c_str());

    glm::mat3 value = glm::dmat3(*p);
    const glm::dvec3 minValues = glm::dvec3(
        glm::compMin(p->minValue()[0]),
        glm::compMin(p->minValue()[1]),
        glm::compMin(p->minValue()[2])
    );
    const float min = static_cast<float>(glm::compMin(minValues));

    const glm::dvec3 maxValues = glm::dvec3(
        glm::compMax(p->maxValue()[0]),
        glm::compMax(p->maxValue()[1]),
        glm::compMax(p->maxValue()[2])
    );
    const float max = static_cast<float>(glm::compMax(maxValues));

    bool changed = false;
    changed |= ImGui::SliderFloat3(
        "[0]",
        &value[0].x,
        min,
        max,
        "%.5f",
        ImGuiSliderFlags_Logarithmic
    );
    changed |= ImGui::SliderFloat3(
        "[1]",
        &value[1].x,
        min,
        max,
        "%.5f",
        ImGuiSliderFlags_Logarithmic
    );
    changed |= ImGui::SliderFloat3(
        "[2]",
        &value[2].x,
        min,
        max,
        "%.5f",
        ImGuiSliderFlags_Logarithmic
    );

    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeSetPropertyScript(p->uri(), ghoul::to_string(value));
    }

    ImGui::PopID();
}

void renderDMat4Property(Property* prop, const std::string& ownerName,
                         ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    DMat4Property* p = static_cast<DMat4Property*>(prop);

    const std::string& name = p->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());
    ImGui::Text("%s", name.c_str());

    glm::mat4 value = glm::dmat4(*p);
    const glm::dvec4 minValues = glm::dvec4(
        glm::compMin(p->minValue()[0]),
        glm::compMin(p->minValue()[1]),
        glm::compMin(p->minValue()[2]),
        glm::compMin(p->minValue()[3])
    );
    const float min = static_cast<float>(glm::compMin(minValues));

    const glm::dvec4 maxValues = glm::dvec4(
        glm::compMax(p->maxValue()[0]),
        glm::compMax(p->maxValue()[1]),
        glm::compMax(p->maxValue()[2]),
        glm::compMax(p->maxValue()[3])
    );
    const float max = static_cast<float>(glm::compMax(maxValues));

    bool changed = false;
    changed |= ImGui::SliderFloat4(
        "[0]",
        &value[0].x,
        min,
        max,
        "%.5f",
        ImGuiSliderFlags_Logarithmic
    );
    changed |= ImGui::SliderFloat4(
        "[1]",
        &value[1].x,
        min,
        max,
        "%.5f",
        ImGuiSliderFlags_Logarithmic
    );
    changed |= ImGui::SliderFloat4(
        "[2]",
        &value[2].x,
        min,
        max,
        "%.5f",
        ImGuiSliderFlags_Logarithmic
    );
    changed |= ImGui::SliderFloat4(
        "[3]",
        &value[3].x,
        min,
        max,
        "%.5f",
        ImGuiSliderFlags_Logarithmic
    );

    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    if (changed) {
        executeSetPropertyScript(p->uri(), ghoul::to_string(value));
    }

    ImGui::PopID();
}

void renderTriggerProperty(Property* prop, const std::string& ownerName,
                           ShowToolTip showTooltip, double tooltipDelay)
{
    ghoul_assert(prop, "prop must not be nullptr");
    const std::string& name = prop->guiName();
    ImGui::PushID((ownerName + '.' + name).c_str());

    const bool pressed = ImGui::Button(name.c_str());
    if (pressed) {
        executeSetPropertyScript(prop->uri(), "nil");
    }
    if (showTooltip) {
        renderTooltip(prop, tooltipDelay);
    }

    ImGui::PopID();
}

} // namespace openspace
