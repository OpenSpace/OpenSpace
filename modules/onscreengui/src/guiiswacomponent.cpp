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

#include <ext/json/json.hpp>
#include <openspace/engine/downloadmanager.h>
#include <modules/iswa/util/iswamanager.h>
#include <ghoul/filesystem/filesystem>


#include <fstream>

#include "imgui.h"

namespace {
    using json = nlohmann::json;
    const std::string _loggerCat = "iSWAComponent";
    const ImVec2 size = ImVec2(350, 500);
}

namespace openspace {
namespace gui {

void GuiIswaComponent::render() {
    bool gmdatavalue = gmdata;
    bool gmimagevalue = gmimage;
    bool iondatavalue = iondata;
    bool kameleonvalue = kameleon;

    ImGui::Begin("ISWA", &_isEnabled, size, 0.5f);
    ImGui::Text("Global Magnetosphere");
    ImGui::Checkbox("Gm From Data", &gmdata); ImGui::SameLine();
    ImGui::Checkbox("Gm From Images", &gmimage);

    ImGui::Text("Ionosphere");
    ImGui::Checkbox("Ion From Data", &iondata);

    ImGui::Text("Kameleon");
    ImGui::Checkbox("BATSRUS.cdf", &kameleon);

    ImGui::Spacing();
    static const int addCygnetBufferSize = 256;
    static char addCygnetBuffer[addCygnetBufferSize];
    ImGui::InputText("addCynget", addCygnetBuffer, addCygnetBufferSize);

    if(ImGui::SmallButton("Add Cygnet"))
        OsEng.scriptEngine().queueScript("openspace.iswa.addCygnet("+std::string(addCygnetBuffer)+");");

    static const int cdfListSize = 256;
    static char cdfList[cdfListSize];

    std::string startPath = absPath("${OPENSPACE_DATA}/cdflist.txt");
    std::copy(startPath.begin(), startPath.end(),cdfList);

    ImGui::InputText("cdf list", cdfList, cdfListSize);
    if(ImGui::SmallButton("Add Cdf files")){
        _cdfOption = -1;
        if(FileSys.fileExists(cdfList)){
            std::string list = std::string(cdfList);

            std::string line;
            std::string basePath = list.substr(0, list.find_last_of("/\\"));

            std::ifstream cdfListFile(list);
            int i = 0;
            if(cdfListFile.is_open()){
                while(getline(cdfListFile, line)){
                    std::string path = line.substr(0, line.find_first_of(" "));
                    std::string date = line.substr(line.find_first_of(" ")+1);
                    

                    _cdfOptions.push_back({i, basePath+"/"+path, date});
                    i++;
                }
            }

        }else{
            LWARNING( std::string(cdfList) + " is not a cdf file or can't be found.");
        }


    }

    if(gmdata != gmdatavalue){
        if(gmdata){
            std::string x = "openspace.iswa.addCygnet(-1,'Data','GMData');";
            std::string y = "openspace.iswa.addCygnet(-2,'Data','GMData');";
            std::string z = "openspace.iswa.addCygnet(-3,'Data','GMData');";
            OsEng.scriptEngine().queueScript(x+y+z);
        }else{
            OsEng.scriptEngine().queueScript("openspace.iswa.removeGroup('GMData');");
        }
    }

    if(gmimage != gmimagevalue){
        if(gmimage){
            std::string x = "openspace.iswa.addCygnet(-1,'Texture','GMImage');";
            std::string y = "openspace.iswa.addCygnet(-2,'Texture','GMImage');";
            std::string z = "openspace.iswa.addCygnet(-3,'Texture','GMImage');";
            OsEng.scriptEngine().queueScript(x+y+z);
        }else{
            OsEng.scriptEngine().queueScript("openspace.iswa.removeGroup('GMImage');");
        }
    }

    if(iondata != iondatavalue){
        if(iondata){
            OsEng.scriptEngine().queueScript("openspace.iswa.addCygnet(-4,'Data','Ionosphere');");
        }else{
            OsEng.scriptEngine().queueScript("openspace.iswa.removeGroup('Ionosphere');");
        }
    }

    if(kameleon != kameleonvalue){
        if(kameleon){
            OsEng.scriptEngine().queueScript("openspace.iswa.addKameleonPlane('${OPENSPACE_DATA}/BATSRUS.cdf','z','BATSRUS');");
            OsEng.scriptEngine().queueScript("openspace.iswa.addKameleonPlane('${OPENSPACE_DATA}/BATSRUS.cdf','y','BATSRUS');");
            OsEng.scriptEngine().queueScript("openspace.iswa.addKameleonPlane('${OPENSPACE_DATA}/BATSRUS.cdf','x','BATSRUS');");
        }else{
            OsEng.scriptEngine().queueScript("openspace.iswa.removeGroup('BATSRUS');");
        }
    }

    if(ImGui::CollapsingHeader("Cdf files")){
        int cdfOptionValue = _cdfOption;

        for(auto radioOption : _cdfOptions){
            std::string path = radioOption.path;
            std::string cdfName = path.substr(path.find_last_of("/\\")+1);
            ImGui::RadioButton(cdfName.c_str(), &_cdfOption, radioOption.value);
            // ImGui::Text(cdfName.c_str());
        }

        if(_cdfOption != cdfOptionValue){
            OsEng.scriptEngine().queueScript("openspace.iswa.removeGroup('Kameleon');");

            std::string path = _cdfOptions[_cdfOption].path;
            std::string date = _cdfOptions[_cdfOption].date;

            OsEng.scriptEngine().queueScript("openspace.iswa.addKameleonPlane('"+path+"','z','Kameleon');");
            OsEng.scriptEngine().queueScript("openspace.iswa.addKameleonPlane('"+path+"','y','Kameleon');");
            OsEng.scriptEngine().queueScript("openspace.iswa.addKameleonPlane('"+path+"','x','Kameleon');");
    
            OsEng.scriptEngine().queueScript("openspace.time.setTime('"+date+"');");
            OsEng.scriptEngine().queueScript("openspace.time.setDeltaTime(0);");
        }
    }

    for (const auto& p : _propertiesByOwner) {
        if (ImGui::CollapsingHeader(p.first.c_str())) {
            for (properties::Property* prop : p.second) {
                if (_boolProperties.find(prop) != _boolProperties.end()) {
                    renderBoolProperty(prop, p.first);
                    continue;
                }

                if (_intProperties.find(prop) != _intProperties.end()) {
                    renderIntProperty(prop, p.first);
                    continue;
                }

                if (_floatProperties.find(prop) != _floatProperties.end()) {
                    renderFloatProperty(prop, p.first);
                    continue;
                }

                if (_vec2Properties.find(prop) != _vec2Properties.end()) {
                    renderVec2Property(prop, p.first);
                    continue;
                }

                if (_vec3Properties.find(prop) != _vec3Properties.end()) {
                    renderVec3Property(prop, p.first);
                    continue;
                }

                if (_vec4Properties.find(prop) != _vec4Properties.end()) {
                    renderVec4Property(prop, p.first);
                    continue;
                }

                if (_optionProperties.find(prop) != _optionProperties.end()) {
                    renderOptionProperty(prop, p.first);
                    continue;
                }

                if (_triggerProperties.find(prop) != _triggerProperties.end()) {
                    renderTriggerProperty(prop, p.first);
                    continue;
                }

                if (_selectionProperties.find(prop) != _selectionProperties.end()) {
                    renderSelectionProperty(prop, p.first);
                    continue;
                }

                if (_stringProperties.find(prop) != _stringProperties.end()) {
                    renderStringProperty(prop, p.first);
                    continue;
                }
            }
        }
    }


    if (ImGui::CollapsingHeader("iSWA screen space cygntes")) {

        auto map = IswaManager::ref().cygnetInformation();
        for(auto cygnetInfo : map){
            int id = cygnetInfo.first;
            auto info = cygnetInfo.second;

            bool selected = info->selected;
            ImGui::Checkbox(info->name.c_str(), &info->selected);
            ImGui::SameLine();

            if(ImGui::CollapsingHeader(("Description" + std::to_string(id)).c_str())){
                ImGui::TextWrapped(info->description.c_str());
                ImGui::Spacing();
            }

            if(selected != info->selected){
                if(info->selected){
                    OsEng.scriptEngine().queueScript("openspace.iswa.addScreenSpaceCygnet("
                        "{CygnetId = "+std::to_string(id)+" });");
                }else{
                    OsEng.scriptEngine().queueScript("openspace.iswa.removeScreenSpaceCygnet("+std::to_string(id)+");");

                }
            }

        }
    }

    ImGui::End();
}

} // gui
} // openspace