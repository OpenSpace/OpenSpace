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
#include <openspace/properties/scalarproperty.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/selectionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vectorproperty.h>

#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/assert.h>

#include "imgui.h"


using namespace openspace::properties;

    void executeScript(const std::string& id, const std::string& value) {
        std::string script =
            "openspace.setPropertyValue('" + id + "', " + value + ");";
        OsEng.scriptEngine().queueScript(script);
    }

    void renderBoolProperty(Property* prop, const std::string& ownerName) {
        BoolProperty* p = static_cast<BoolProperty*>(prop);
        std::string name = p->guiName();
        ImGui::PushID((ownerName + "." + name).c_str());

        BoolProperty::ValueType value = *p;
        ImGui::Checkbox(name.c_str(), &value);

        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(), value ? "true": "false");
        ImGui::PopID();
    }

    void renderOptionProperty(Property* prop, const std::string& ownerName) {
        OptionProperty* p = static_cast<OptionProperty*>(prop);
        std::string name = p->guiName();
        ImGui::PushID((ownerName + "." + name).c_str());

        int value = *p;
        std::vector<OptionProperty::Option> options = p->options();
        for (const OptionProperty::Option& o : options) {
            ImGui::RadioButton(name.c_str(), &value, o.value);
            ImGui::SameLine();
            ImGui::Text(o.description.c_str());
        }
        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(), std::to_string(value));
        ImGui::PopID();
    }

    void renderSelectionProperty(Property* prop, const std::string& ownerName) {
        SelectionProperty* p = static_cast<SelectionProperty*>(prop);
        std::string name = p->guiName();
        ImGui::PushID((ownerName + "." + name).c_str());

        if (ImGui::CollapsingHeader(name.c_str())) {
            const std::vector<SelectionProperty::Option>& options = p->options();
            std::vector<int> newSelectedIndices;

            std::vector<int> selectedIndices = p->value();

            for (int i = 0; i < options.size(); ++i) {
                std::string description = options[i].description;
                bool selected = std::find(selectedIndices.begin(), selectedIndices.end(), i) != selectedIndices.end();
                ImGui::Checkbox(description.c_str(), &selected);

                if (selected)
                    newSelectedIndices.push_back(i);
            }

            if (newSelectedIndices != p->value()) {
                std::string parameters = "{";
                for (int i : newSelectedIndices)
                    parameters += std::to_string(i) + ",";
                parameters += "}";
                executeScript(p->fullyQualifiedIdentifier(), parameters);
            }
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
        ImGui::InputText(name.c_str(), buffer, bufferSize);
        std::string newValue(buffer);

        if (newValue != p->value() && FileSys.fileExists(newValue))
            executeScript(p->fullyQualifiedIdentifier(), "'" + newValue + "'");

        ImGui::PopID();
    }

    void renderIntProperty(Property* prop, const std::string& ownerName) {
        IntProperty* p = static_cast<IntProperty*>(prop);
        std::string name = p->guiName();
        ImGui::PushID((ownerName + "." + name).c_str());

        IntProperty::ValueType value = *p;
        ImGui::SliderInt(name.c_str(), &value, p->minValue(), p->maxValue());

        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(), std::to_string(value));
        
        ImGui::PopID();
    }

    void renderFloatProperty(Property* prop, const std::string& ownerName) {
        FloatProperty* p = static_cast<FloatProperty*>(prop);
        std::string name = p->guiName();
        ImGui::PushID((ownerName + "." + name).c_str());

        FloatProperty::ValueType value = *p;
        ImGui::SliderFloat(name.c_str(), &value, p->minValue(), p->maxValue());

        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(), std::to_string(value));
        
        ImGui::PopID();
    }

    void renderVec2Property(Property* prop, const std::string& ownerName) {
        Vec2Property* p = static_cast<Vec2Property*>(prop);
        std::string name = p->guiName();
        ImGui::PushID((ownerName + "." + name).c_str());

        Vec2Property::ValueType value = *p;

        ImGui::SliderFloat2(name.c_str(), &value.x, std::min(p->minValue().x, p->minValue().y), std::max(p->maxValue().x, p->maxValue().y));

        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(),
            "{" + std::to_string(value.x) + "," + std::to_string(value.y) + "}");
        
        ImGui::PopID();
    }

    void renderVec3Property(Property* prop, const std::string& ownerName) {
        Vec3Property* p = static_cast<Vec3Property*>(prop);
        std::string name = p->guiName();
        ImGui::PushID((ownerName + "." + name).c_str());

        Vec3Property::ValueType value = *p;

        ImGui::SliderFloat3(name.c_str(), &value.x, p->minValue().x, p->maxValue().x);

        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(),
            "{" + std::to_string(value.x) + "," +
                  std::to_string(value.y) + "," +
                  std::to_string(value.z) + "}");
        
        ImGui::PopID();
    }

    void renderVec4Property(Property* prop, const std::string& ownerName) {
        Vec4Property* p = static_cast<Vec4Property*>(prop);
        std::string name = p->guiName();
        ImGui::PushID((ownerName + "." + name).c_str());

        Vec4Property::ValueType value = *p;

        ImGui::SliderFloat4(name.c_str(), &value.x, p->minValue().x, p->maxValue().x);

        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(),
            "{" + std::to_string(value.x) + "," +
                  std::to_string(value.y) + "," +
                  std::to_string(value.z) + "," +
                  std::to_string(value.w) + "}");

        ImGui::PopID();
    }

    void renderTriggerProperty(Property* prop, const std::string& ownerName) {
        std::string name = prop->guiName();
        ImGui::PushID((ownerName + "." + name).c_str());

        bool pressed = ImGui::Button(name.c_str());
        if (pressed)
            executeScript(prop->fullyQualifiedIdentifier(), "nil");
        
        ImGui::PopID();
    }

//void renderBoolProperty(Property* prop, const std::string& ownerName) {
//    BoolProperty* p = static_cast<BoolProperty*>(prop);
//    std::string name = p->guiName();

//    BoolProperty::ValueType value = *p;
//    ImGui::Checkbox(name.c_str(), &value);
//    p->set(value);
//}

//void renderOptionProperty(Property* prop, const std::string& ownerName) {
//    OptionProperty* p = static_cast<OptionProperty*>(prop);
//    std::string name = p->guiName();

//    int value = *p;
//    std::vector<OptionProperty::Option> options = p->options();
//    for (const OptionProperty::Option& o : options) {
//        ImGui::RadioButton(name.c_str(), &value, o.value);
//        ImGui::SameLine();
//        ImGui::Text(o.description.c_str());
//    }
//    p->set(value);
//}

//void renderSelectionProperty(Property* prop, const std::string& ownerName) {
//    SelectionProperty* p = static_cast<SelectionProperty*>(prop);
//    std::string name = p->guiName();

//    if (ImGui::CollapsingHeader(name.c_str())) {
//        const std::vector<SelectionProperty::Option>& options = p->options();
//        std::vector<int> newSelectedIndices;

//        std::vector<int> selectedIndices = p->value();

//        for (int i = 0; i < options.size(); ++i) {
//            std::string description = options[i].description;
//            bool selected = std::find(selectedIndices.begin(), selectedIndices.end(), i) != selectedIndices.end();
//            ImGui::Checkbox(description.c_str(), &selected);

//            if (selected)
//                newSelectedIndices.push_back(i);
//        }

//        p->setValue(std::move(newSelectedIndices));
//    }
//}

//void renderIntProperty(Property* prop, const std::string& ownerName) {
//    IntProperty* p = static_cast<IntProperty*>(prop);
//    std::string name = p->guiName();

//    IntProperty::ValueType value = *p;
//    ImGui::SliderInt(name.c_str(), &value, p->minValue(), p->maxValue());
//    p->set(value);
//}

//void renderFloatProperty(Property* prop, const std::string& ownerName) {
//    FloatProperty* p = static_cast<FloatProperty*>(prop);
//    std::string name = p->guiName();

//    FloatProperty::ValueType value = *p;
//    ImGui::SliderFloat(name.c_str(), &value, p->minValue(), p->maxValue());
//    p->set(value);
//}

//void renderVec2Property(Property* prop, const std::string& ownerName) {
//    Vec2Property* p = static_cast<Vec2Property*>(prop);
//    std::string name = p->guiName();

//    Vec2Property::ValueType value = *p;

//    ImGui::SliderFloat2(name.c_str(), &value.x, p->minValue().x, p->maxValue().x);
//    p->set(value);
//}


//void renderVec3Property(Property* prop, const std::string& ownerName) {
//    Vec3Property* p = static_cast<Vec3Property*>(prop);
//    std::string name = p->guiName();

//    Vec3Property::ValueType value = *p;

//    ImGui::SliderFloat3(name.c_str(), &value.x, p->minValue().x, p->maxValue().x);
//    p->set(value);
//}

//void renderTriggerProperty(Property* prop, const std::string& ownerName) {
//    std::string name = prop->guiName();
//    bool pressed = ImGui::Button(name.c_str());
//    if (pressed)
//        prop->set(0);
//}