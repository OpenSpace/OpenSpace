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

    bool e = _isEnabled;

    ImGui::Begin("ISWA", &e, size, 0.5f);

    _isEnabled = e;
    
    ImGui::Text("Global Magnetosphere");
    ImGui::Checkbox("Gm From Data", &_gmData); ImGui::SameLine();
    ImGui::Checkbox("Gm From Images", &_gmImage);

    ImGui::Text("Ionosphere");
    ImGui::Checkbox("Ion From Data", &_ionData);

    ImGui::Spacing();
    static const int addCygnetBufferSize = 256;
    static char addCygnetBuffer[addCygnetBufferSize];
    ImGui::InputText("addCynget", addCygnetBuffer, addCygnetBufferSize);

    if (ImGui::SmallButton("Add Cygnet")) {
        OsEng.scriptEngine().queueScript(
            "openspace.iswa.addCygnet(" + std::string(addCygnetBuffer) + ");",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    if (_gmData != oldGmDataValue) {
        if (_gmData) {
            std::string x = "openspace.iswa.addCygnet(-4, 'Data', 'GMData');";
            std::string y = "openspace.iswa.addCygnet(-5, 'Data', 'GMData');";
            std::string z = "openspace.iswa.addCygnet(-6, 'Data', 'GMData');";
            OsEng.scriptEngine().queueScript(
                x + y + z,
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        } else {
            OsEng.scriptEngine().queueScript(
                "openspace.iswa.removeGroup('GMData');",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    }

    if (_gmImage != oldGmImageValue) {
        if (_gmImage) {
            std::string x = "openspace.iswa.addCygnet(-4, 'Texture', 'GMImage');";
            std::string y = "openspace.iswa.addCygnet(-5, 'Texture', 'GMImage');";
            std::string z = "openspace.iswa.addCygnet(-6, 'Texture', 'GMImage');";
            OsEng.scriptEngine().queueScript(
                x + y + z,
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        } else {
            OsEng.scriptEngine().queueScript(
                "openspace.iswa.removeGroup('GMImage');",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    }

    if(_ionData != oldIonDataValue) {
        if(_ionData) {
            OsEng.scriptEngine().queueScript(
                "openspace.iswa.addCygnet(-10,'Data','Ionosphere');",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        } else {
            OsEng.scriptEngine().queueScript(
                "openspace.iswa.removeGroup('Ionosphere');",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    }

    if (ImGui::CollapsingHeader("Cdf files")) {
        const auto& cdfInfo = IswaManager::ref().cdfInformation();

        for (const auto& group : cdfInfo) {
            std::string groupName = group.first;
            if (_cdfOptionsMap.find(groupName) == _cdfOptionsMap.end()){
                _cdfOptionsMap[groupName] = -1;
            }

            if (ImGui::CollapsingHeader(groupName.c_str())) {
                int cdfOptionValue = _cdfOptionsMap[groupName];
                const auto& cdfs = group.second;

                for (int i = 0; i < cdfs.size(); ++i) {
                    ImGui::RadioButton(
                        cdfs[i].name.c_str(),
                        &_cdfOptionsMap[groupName],
                        i
                    );
                }

                int cdfOption = _cdfOptionsMap[groupName];
                if (cdfOptionValue != cdfOption) {
                   if (cdfOptionValue >= 0) {
                        groupName = cdfs[cdfOptionValue].group;
                        // std::cout << groupName << std::endl;
                        // OsEng.scriptEngine().queueScript("openspace.iswa.removeGroup('"+groupName+"');");
                    }

                    std::string path  = cdfs[cdfOption].path;
                    std::string date  = cdfs[cdfOption].date;
                    groupName = cdfs[cdfOption].group;
                    OsEng.scriptEngine().queueScript(
                        "openspace.iswa.addKameleonPlanes('" +
                        groupName +
                        "'," +
                        std::to_string(cdfOption) +
                        ");",
                        scripting::ScriptEngine::RemoteScripting::Yes
                    );
                    OsEng.scriptEngine().queueScript(
                        "openspace.time.setTime('" + date + "');",
                        scripting::ScriptEngine::RemoteScripting::Yes
                    );
                    OsEng.scriptEngine().queueScript(
                        "openspace.time.setDeltaTime(0);",
                        scripting::ScriptEngine::RemoteScripting::Yes
                    );
                }
            }
        }
    }

    GuiPropertyComponent::render();

    if (ImGui::CollapsingHeader("iSWA screen space cygntes")) {
        const auto& map = IswaManager::ref().cygnetInformation();
        for (const auto& cygnetInfo : map) {
            int id = cygnetInfo.first;
            auto info = cygnetInfo.second;

            bool selected = info->selected;
            ImGui::Checkbox(info->name.c_str(), &info->selected);
            ImGui::SameLine();

            if (ImGui::CollapsingHeader(("Description" + std::to_string(id)).c_str())) {
                ImGui::TextWrapped(info->description.c_str());
                ImGui::Spacing();
            }

            if (selected != info->selected) {
                if (info->selected) {
                    OsEng.scriptEngine().queueScript(
                        "openspace.iswa.addScreenSpaceCygnet("
                        "{CygnetId = " + std::to_string(id) + " });",
                        scripting::ScriptEngine::RemoteScripting::Yes
                    );
                } else {
                    OsEng.scriptEngine().queueScript(
                        "openspace.iswa.removeScreenSpaceCygnet(" +
                        std::to_string(id) +
                        ");",
                        scripting::ScriptEngine::RemoteScripting::Yes
                    );
                }
            }
        }
    }
    
    ImGui::End();
#endif
}

} // namespace gui
} // namespace openspace
