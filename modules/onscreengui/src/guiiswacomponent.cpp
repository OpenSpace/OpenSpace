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
#include <modules/iswa/util/iswamanager.h>
#include <openspace/engine/downloadmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/scripting/scriptengine.h>

#include <ghoul/filesystem/filesystem>

#include <ext/json/json.hpp>

#include "imgui.h"

namespace {
    using json = nlohmann::json;
    const std::string _loggerCat = "iSWAComponent";
    const ImVec2 size = ImVec2(350, 500);
}

namespace openspace {
namespace gui {

GuiIswaComponent::GuiIswaComponent()
    : GuiPropertyComponent("iSWA")
    , _gmData(false)
    , _gmImage(false)
    , _ionData(false)
{}

void GuiIswaComponent::render() {
#ifdef OPENSPACE_MODULE_ISWA_ENABLED
    bool oldGmDataValue = _gmData;
    bool oldGmImageValue = _gmImage;
    bool oldIonDataValue = _ionData;

    ImGui::Begin("ISWA", &_isEnabled, size, 0.5f);
    // ImGui::Text("Global Magnetosphere");

  if(ImGui::CollapsingHeader("Loaded Cdf Files")){
        ImGui::Spacing();
        ImGui::SameLine();
        auto cdfInfo = IswaManager::ref().cdfInformation();

        for(auto group : cdfInfo){
            std::string groupName = group.first;

            //if group does not exist in _cdfOptionsMap yet, create it and set selected to -1
            if(_cdfOptionsMap.find(groupName) == _cdfOptionsMap.end()){
                _cdfOptionsMap[groupName] = -1;
            }

            if(ImGui::CollapsingHeader((groupName+" Files").c_str())){
                ImGui::Spacing();

                // old selected index
                int cdfOptionValue = _cdfOptionsMap[groupName];
                auto cdfs = group.second;

                // get new selected index from radio button
                for(int i=0; i<cdfs.size(); i++){
                    ImGui::Spacing();
                    ImGui::SameLine();
                    ImGui::Spacing();
                    ImGui::SameLine();
                    ImGui::RadioButton(cdfs[i].name.c_str(), &_cdfOptionsMap[groupName], i);
                }

                //if different, add kameleon planes
                int cdfOption = _cdfOptionsMap[groupName];
                if(cdfOptionValue != cdfOption){
                   // if(cdfOptionValue >= 0){
                   //      groupName = cdfs[cdfOptionValue].group;
                   //      // std::cout << groupName << std::endl;
                   //      // OsEng.scriptEngine().queueScript("openspace.iswa.removeGroup('"+groupName+"');");
                   //  }

                    std::string path  = cdfs[cdfOption].path;
                    std::string date  = cdfs[cdfOption].date;
                    groupName = cdfs[cdfOption].group;
                    OsEng.scriptEngine().queueScript(
                        "openspace.iswa.addKameleonPlanes('"+groupName+"',"+std::to_string(cdfOption)+");",
                        scripting::ScriptEngine::RemoteScripting::Yes
                    );
                    // OsEng.scriptEngine().queueScript(
                    //     "openspace.time.setTime('"+date+"');",
                    //     scripting::ScriptEngine::RemoteScripting::Yes
                    // );
                    OsEng.scriptEngine().queueScript(
                        "openspace.time.setDeltaTime(0);",
                        scripting::ScriptEngine::RemoteScripting::Yes
                    );
                }
            }
        }
    }

    if(ImGui::CollapsingHeader("OpenSpace Cygnets")){
        ImGui::Spacing();
        ImGui::SameLine();
        ImGui::Checkbox("Global Magnetosphere From Data", &_gmData);

        ImGui::Spacing();
        ImGui::SameLine();
        ImGui::Checkbox("Global Magnetosphere From Images", &_gmImage);

        ImGui::Spacing();
        ImGui::SameLine();
        ImGui::Checkbox("Ionosphere From Data", &_ionData);
    }

    // static const int addCygnetBufferSize = 256;
    // static char addCygnetBuffer[addCygnetBufferSize];
    // ImGui::InputText("addCynget", addCygnetBuffer, addCygnetBufferSize);

    // if(ImGui::SmallButton("Add Cygnet"))
    //     OsEng.scriptEngine().queueScript("openspace.iswa.addCygnet("+std::string(addCygnetBuffer)+");");
    //     // IswaManager::ref().setFit(std::stof(std::string(addCygnetBuffer)));

    if(_gmData != oldGmDataValue){
        if(_gmData){
            // IswaManager::ResourceType::Text = 2
            std::string x = "openspace.iswa.addCygnet(-4, 2, 'Magnetosphere_Data');";
            std::string y = "openspace.iswa.addCygnet(-5, 2, 'Magnetosphere_Data');";
            std::string z = "openspace.iswa.addCygnet(-6, 2, 'Magnetosphere_Data');";
            OsEng.scriptEngine().queueScript(
                x+y+z,
                scripting::ScriptEngine::RemoteScripting::Yes
            );
            OsEng.scriptEngine().queueScript(
                "openspace.iswa.clearGroupBuildData('Magnetosphere_Data');",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }else{
            OsEng.scriptEngine().queueScript(
                "openspace.iswa.removeGroup('Magnetosphere_Data');",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    }

    if(_gmImage != oldGmImageValue){
        if(_gmImage){
            // IswaManager::ResourceType::Texture = 0
            std::string x = "openspace.iswa.addCygnet(-4, 0, 'Magnetosphere_Image');";
            std::string y = "openspace.iswa.addCygnet(-5, 0, 'Magnetosphere_Image');";
            std::string z = "openspace.iswa.addCygnet(-6, 0, 'Magnetosphere_Image');";
            OsEng.scriptEngine().queueScript(
                x+y+z,
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }else{
            OsEng.scriptEngine().queueScript(
                "openspace.iswa.removeGroup('Magnetosphere_Image');",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    }

    if(_ionData != oldIonDataValue){
        if(_ionData){
            // IswaManager::ResourceType::Json = 1
            OsEng.scriptEngine().queueScript(
                "openspace.iswa.addCygnet(-10, 1, 'Ionosphere');",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
            OsEng.scriptEngine().queueScript(
                "openspace.iswa.clearGroupBuildData('Ionosphere');",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }else{
            OsEng.scriptEngine().queueScript(
                "openspace.iswa.removeGroup('Ionosphere');",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    }

    if (ImGui::CollapsingHeader("iSWA Screen Space Cygntes")) {

        auto map = IswaManager::ref().cygnetInformation();
        for(auto cygnetInfo : map){
            ImGui::Spacing();
            ImGui::SameLine();

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
                    OsEng.scriptEngine().queueScript(
                        "openspace.iswa.addScreenSpaceCygnet({CygnetId = "+std::to_string(id)+" });",
                        scripting::ScriptEngine::RemoteScripting::Yes
                    );
                }else{
                    OsEng.scriptEngine().queueScript(
                        "openspace.iswa.removeScreenSpaceCygnet("+std::to_string(id)+");",
                        scripting::ScriptEngine::RemoteScripting::Yes
                    );
                }
            }

        }
    }

    GuiPropertyComponent::render();
    
    ImGui::End();
#endif
}

} // namespace gui
} // namespace openspace
