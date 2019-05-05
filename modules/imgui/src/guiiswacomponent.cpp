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

#ifdef OPENSPACE_MODULE_ISWA_ENABLED

#include <modules/imgui/include/guiiswacomponent.h>

#include <modules/imgui/include/imgui_include.h>
#include <modules/iswa/util/iswamanager.h>
#include <openspace/json.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspace/engine/globals.h>
#include <openspace/scripting/scriptengine.h>

namespace {
    using json = nlohmann::json;
    const ImVec2 WindowSize = ImVec2(350, 500);
} // namespace

namespace openspace::gui {

GuiIswaComponent::GuiIswaComponent() : GuiPropertyComponent("iSWA") {}

void GuiIswaComponent::render() {
#ifdef OPENSPACE_MODULE_ISWA_ENABLED
    const bool oldGmDataValue = _gmData;
    const bool oldGmImageValue = _gmImage;
    const bool oldIonDataValue = _ionData;

    ImGui::SetNextWindowCollapsed(_isCollapsed);

    bool e = _isEnabled;
    ImGui::Begin("ISWA", &e, WindowSize, 0.5f);
    _isEnabled = e;
    _isCollapsed = ImGui::IsWindowCollapsed();

    ImGui::Text("Global Magnetosphere");
    ImGui::Checkbox("Gm From Data", &_gmData);
    ImGui::SameLine();
    ImGui::Checkbox("Gm From Images", &_gmImage);

    ImGui::Text("Ionosphere");
    ImGui::Checkbox("Ion From Data", &_ionData);

    ImGui::Spacing();
    constexpr const int AddCygnetBufferSize = 256;
    static char addCygnetBuffer[AddCygnetBufferSize];
    ImGui::InputText("addCynget", addCygnetBuffer, AddCygnetBufferSize);

    if (ImGui::SmallButton("Add Cygnet")) {
        global::scriptEngine.queueScript(
            "openspace.iswa.addCygnet(" + std::string(addCygnetBuffer) + ");",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    if (_gmData != oldGmDataValue) {
        if (_gmData) {
            constexpr const char* script = R"(
                openspace.iswa.addCygnet(-4, 'Data', 'GMData');
                openspace.iswa.addCygnet(-5, 'Data', 'GMData');
                openspace.iswa.addCygnet(-6, 'Data', 'GMData');
            )";
            global::scriptEngine.queueScript(
                script,
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        } else {
            global::scriptEngine.queueScript(
                "openspace.iswa.removeGroup('GMData');",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    }

    if (_gmImage != oldGmImageValue) {
        if (_gmImage) {
            constexpr const char* script = R"(
                openspace.iswa.addCygnet(-4, 'Texture', 'GMImage');
                openspace.iswa.addCygnet(-5, 'Texture', 'GMImage');
                openspace.iswa.addCygnet(-6, 'Texture', 'GMImage');
            )";
            global::scriptEngine.queueScript(
                script,
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        } else {
            global::scriptEngine.queueScript(
                "openspace.iswa.removeGroup('GMImage');",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    }

    if(_ionData != oldIonDataValue) {
        if(_ionData) {
            global::scriptEngine.queueScript(
                "openspace.iswa.addCygnet(-10, 'Data', 'Ionosphere');",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        } else {
            global::scriptEngine.queueScript(
                "openspace.iswa.removeGroup('Ionosphere');",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    }

    if (ImGui::CollapsingHeader("Cdf Files")) {
        const std::map<std::string, std::vector<CdfInfo>>& cdfInfo =
            IswaManager::ref().cdfInformation();

        using K = std::string;
        using V = std::vector<CdfInfo>;
        for (const std::pair<const K, V>& group : cdfInfo) {
            const std::string& groupName = group.first;
            if (_cdfOptionsMap.find(groupName) == _cdfOptionsMap.end()) {
                _cdfOptionsMap[groupName] = -1;
            }

            if (ImGui::CollapsingHeader(groupName.c_str())) {
                int cdfOptionValue = _cdfOptionsMap[groupName];
                const std::vector<CdfInfo>& cdfs = group.second;

                for (size_t i = 0; i < cdfs.size(); ++i) {
                    ImGui::RadioButton(
                        cdfs[i].name.c_str(),
                        &_cdfOptionsMap[groupName],
                        static_cast<int>(i)
                    );
                }

                const int cdfOption = _cdfOptionsMap[groupName];
                if (cdfOptionValue != cdfOption) {
                    const std::string& date = cdfs[cdfOption].date;
                    global::scriptEngine.queueScript(
                        "openspace.iswa.addKameleonPlanes('" +
                        cdfs[cdfOption].group +
                        "'," +
                        std::to_string(cdfOption) +
                        ");",
                        scripting::ScriptEngine::RemoteScripting::Yes
                    );
                    global::scriptEngine.queueScript(
                        "openspace.time.setTime('" + date + "');",
                        scripting::ScriptEngine::RemoteScripting::Yes
                    );
                    global::scriptEngine.queueScript(
                        "openspace.time.setDeltaTime(0);",
                        scripting::ScriptEngine::RemoteScripting::Yes
                    );
                }
            }
        }
    }

    GuiPropertyComponent::render();

    if (ImGui::CollapsingHeader("iSWA screen space cygntes")) {
        const std::map<int, std::shared_ptr<CygnetInfo>>& map =
            IswaManager::ref().cygnetInformation();

        for (const std::pair<int, std::shared_ptr<CygnetInfo>>& cygnetInfo : map) {
            int id = cygnetInfo.first;
            CygnetInfo& info = *cygnetInfo.second;

            bool selected = info.selected;
            ImGui::Checkbox(info.name.c_str(), &info.selected);
            ImGui::SameLine();

            if (ImGui::CollapsingHeader(("Description" + std::to_string(id)).c_str())) {
                ImGui::TextWrapped("%s", info.description.c_str());
                ImGui::Spacing();
            }

            if (selected != info.selected) {
                const std::string idStr = std::to_string(id);
                if (info.selected) {
                    global::scriptEngine.queueScript(
                        "openspace.iswa.addScreenSpaceCygnet({CygnetId=" + idStr + "});",
                        scripting::ScriptEngine::RemoteScripting::Yes
                    );
                } else {
                    global::scriptEngine.queueScript(
                        "openspace.iswa.removeScreenSpaceCygnet(" + idStr + ");",
                        scripting::ScriptEngine::RemoteScripting::Yes
                    );
                }
            }
        }
    }

    ImGui::End();
#endif
}

} // namespace openspace::gui

#endif // OPENSPACE_MODULE_ISWA_ENABLED
