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

#include <modules/onscreengui/include/guiiswacomponent.h>

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

#include <ext/json/json.hpp>
#include <openspace/engine/downloadmanager.h>

#include <fstream>

#include "imgui.h"

namespace {
    // const ImVec2 size = ImVec2(350, 200);
    using json = nlohmann::json;
    const std::string _loggerCat = "iSWAComponent";
    const ImVec2 size = ImVec2(350, 500);

    using namespace openspace::properties;

    void executeScript(const std::string& id, const std::string& value) {
        std::string script =
            "openspace.setPropertyValue('" + id + "', " + value + ");";
        OsEng.scriptEngine().queueScript(script);
    }

    void renderBoolProperty(Property* prop) {
        BoolProperty* p = static_cast<BoolProperty*>(prop);
        std::string name = p->guiName();

        BoolProperty::ValueType value = *p;
        ImGui::Checkbox((name).c_str(), &value);

        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(), value ? "true": "false");
    }

    void renderOptionProperty(Property* prop) {
        OptionProperty* p = static_cast<OptionProperty*>(prop);
        std::string name = p->guiName();

        int value = *p;
        std::vector<OptionProperty::Option> options = p->options();
        for (const OptionProperty::Option& o : options) {
            ImGui::RadioButton((name).c_str(), &value, o.value);
            ImGui::SameLine();
            ImGui::Text(o.description.c_str());
        }
        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(), std::to_string(value));
    }

    void renderSelectionProperty(Property* prop) {
        SelectionProperty* p = static_cast<SelectionProperty*>(prop);
        std::string name = p->guiName();

        if (ImGui::CollapsingHeader((name).c_str())) {
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
    }

    void renderStringProperty(Property* prop) {
        StringProperty* p = static_cast<StringProperty*>(prop);
        std::string name = p->guiName();

        static const int bufferSize = 256;
        static char buffer[bufferSize];
#ifdef _MSC_VER
        strcpy_s(buffer, p->value().length() + 1, p->value().c_str());
#else
        strcpy(buffer, p->value().c_str());
#endif
        ImGui::InputText((name).c_str(), buffer, bufferSize);
        std::string newValue(buffer);

        if (newValue != p->value() && FileSys.fileExists(newValue))
            executeScript(p->fullyQualifiedIdentifier(), "'" + newValue + "'");
    }

    void renderIntProperty(Property* prop) {
        IntProperty* p = static_cast<IntProperty*>(prop);
        std::string name = p->guiName();

        IntProperty::ValueType value = *p;
        ImGui::SliderInt((name).c_str(), &value, p->minValue(), p->maxValue());

        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(), std::to_string(value));
    }

    void renderFloatProperty(Property* prop) {
        FloatProperty* p = static_cast<FloatProperty*>(prop)
;        std::string name = p->guiName();

        FloatProperty::ValueType value = *p;
        ImGui::SliderFloat((name).c_str(), &value, p->minValue(), p->maxValue());

        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(), std::to_string(value));

    }

    void renderVec2Property(Property* prop) {
        Vec2Property* p = static_cast<Vec2Property*>(prop);
        std::string name = p->guiName();

        Vec2Property::ValueType value = *p;

        ImGui::SliderFloat2((name).c_str(), &value.x, std::min(p->minValue().x, p->minValue().y), std::max(p->maxValue().x, p->maxValue().y));

        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(),
            "{" + std::to_string(value.x) + "," + std::to_string(value.y) + "}");
    }

    void renderVec3Property(Property* prop) {
        Vec3Property* p = static_cast<Vec3Property*>(prop);
        std::string name = p->guiName();

        Vec3Property::ValueType value = *p;

        ImGui::SliderFloat3((name).c_str(), &value.x, p->minValue().x, p->maxValue().x);

        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(),
            "{" + std::to_string(value.x) + "," +
                  std::to_string(value.y) + "," +
                  std::to_string(value.z) + "}");
    }

    void renderVec4Property(Property* prop) {
        Vec4Property* p = static_cast<Vec4Property*>(prop);
        std::string name = p->guiName();

        Vec4Property::ValueType value = *p;

        ImGui::SliderFloat4((name).c_str(), &value.x, p->minValue().x, p->maxValue().x);

        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(),
            "{" + std::to_string(value.x) + "," +
                  std::to_string(value.y) + "," +
                  std::to_string(value.z) + "," +
                  std::to_string(value.w) + "}");
    }

    void renderTriggerProperty(Property* prop) {
        std::string name = prop->guiName();
        bool pressed = ImGui::Button((name).c_str());
        if (pressed)
            executeScript(prop->fullyQualifiedIdentifier(), "nil");
    }
}

namespace openspace {
namespace gui {

void GuiIswaComponent::initialize(){
    DlManager.fetchFile(
        "http://iswa2.ccmc.gsfc.nasa.gov/IswaSystemWebApp/CygnetHealthServlet",
        [this](const DownloadManager::MemoryFile& file){
            fillOptions(std::string(file.buffer));
        },
        [](const std::string& err){
            LWARNING("Download to memory was aborted: " + err);
        }
    );
}

void GuiIswaComponent::render() {
    bool gmdatavalue = gmdata;
    bool gmimagevalue = gmimage;
    bool iondatavalue = iondata;

    ImGui::Begin("ISWA", &_isEnabled, size, 0.5f);
    ImGui::Text("Global Magnetosphere");
    ImGui::Checkbox("Gm From Data", &gmdata); ImGui::SameLine();
    ImGui::Checkbox("Gm From Images", &gmimage);

    ImGui::Text("Ionosphere");
    ImGui::Checkbox("Ion From Data", &iondata);

    ImGui::Spacing();
    static const int addCygnetBufferSize = 256;
    static char addCygnetBuffer[addCygnetBufferSize];
    ImGui::InputText("addCynget", addCygnetBuffer, addCygnetBufferSize);

    if(ImGui::SmallButton("Add Cygnet"))
        OsEng.scriptEngine().queueScript("openspace.iswa.addCygnet('"+std::string(addCygnetBuffer)+"');");

    if(gmdata != gmdatavalue){
        if(gmdata){
            std::string x = "openspace.iswa.addCygnet('-1,Data,1');";
            std::string y = "openspace.iswa.addCygnet('-2,Data,1');";
            std::string z = "openspace.iswa.addCygnet('-3,Data,1');";
            OsEng.scriptEngine().queueScript(x+y+z);
        }else{
            OsEng.scriptEngine().queueScript("openspace.iswa.removeGroup(1);");
        }
    }

    if(gmimage != gmimagevalue){
        if(gmimage){
            std::string x = "openspace.iswa.addCygnet('-1,Texture,2');";
            std::string y = "openspace.iswa.addCygnet('-2,Texture,2');";
            std::string z = "openspace.iswa.addCygnet('-3,Texture,2');";
            OsEng.scriptEngine().queueScript(x+y+z);
        }else{
            OsEng.scriptEngine().queueScript("openspace.iswa.removeGroup(2);");
        }
    }

    if(iondata != iondatavalue){
        if(iondata){
            OsEng.scriptEngine().queueScript("openspace.iswa.addCygnet('0,Data,3');");
        }else{
            OsEng.scriptEngine().queueScript("openspace.iswa.removeGroup(3);");
        }
    }

    for (const auto& p : _propertiesByOwner) {
        if (ImGui::CollapsingHeader(p.first.c_str())) {
            for (properties::Property* prop : p.second) {
                if (_boolProperties.find(prop) != _boolProperties.end()) {
                    renderBoolProperty(prop);
                    continue;
                }

                if (_intProperties.find(prop) != _intProperties.end()) {
                    renderIntProperty(prop);
                    continue;
                }

                if (_floatProperties.find(prop) != _floatProperties.end()) {
                    renderFloatProperty(prop);
                    continue;
                }

                if (_vec2Properties.find(prop) != _vec2Properties.end()) {
                    renderVec2Property(prop);
                    continue;
                }

                if (_vec3Properties.find(prop) != _vec3Properties.end()) {
                    renderVec3Property(prop);
                    continue;
                }

                if (_vec4Properties.find(prop) != _vec4Properties.end()) {
                    renderVec4Property(prop);
                    continue;
                }

                if (_optionProperties.find(prop) != _optionProperties.end()) {
                    renderOptionProperty(prop);
                    continue;
                }

                if (_triggerProperties.find(prop) != _triggerProperties.end()) {
                    renderTriggerProperty(prop);
                    continue;
                }

                if (_selectionProperties.find(prop) != _selectionProperties.end()) {
                    renderSelectionProperty(prop);
                    continue;
                }

                if (_stringProperties.find(prop) != _stringProperties.end()) {
                    renderStringProperty(prop);
                    continue;
                }
            }
        }
    }


    if (ImGui::CollapsingHeader("iSWA screen space cygntes")) {       
        for (int i = 0; i < options.size(); ++i) {
            // std::string name = options[i].name;
            bool selected = options[i].selected;//std::find(selectedIndices.begin(), selectedIndices.end(), i) != selectedIndices.end();
            ImGui::Checkbox(options[i].name.c_str(), &options[i].selected);
            ImGui::SameLine();
            if (ImGui::CollapsingHeader(("Description" + std::to_string(options[i].id)).c_str())) {
                ImGui::TextWrapped(options[i].description.c_str());
                ImGui::Spacing();
            }

            if(selected != options[i].selected){
                if(options[i].selected){
                    std::string arguments = std::to_string(options[i].id) + ", '" + options[i].name + "'," + std::to_string(options[i].updateInterval);
                    OsEng.scriptEngine().queueScript("openspace.iswa.addScreenSpaceCygnet("+arguments+");");
                }else{
                    OsEng.scriptEngine().queueScript("openspace.iswa.removeScreenSpaceCygnet('"+options[i].name+"');");
                }
            }
        }
    }

    ImGui::End();
}

void GuiIswaComponent::fillOptions(std::string jsonString){
    if(jsonString != ""){
        json j = json::parse(jsonString);
        
        json jCygnets = j["listOfPriorityCygnets"];
        for(int i=0; i<jCygnets.size(); i++){
            json jCygnet = jCygnets.at(i);
            // std::cout << jCygnets.at(i) << std::endl;   
            options.push_back({
                jCygnet["cygnetID"],
                jCygnet["cygnetDisplayTitle"],
                jCygnet["cygnetDescription"],
                jCygnet["cygnetUpdateInterval"],
                false
            });
        }
    }
}

} // gui
} // openspace