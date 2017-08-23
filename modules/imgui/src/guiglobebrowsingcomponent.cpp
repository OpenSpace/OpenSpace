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

#include <modules/imgui/include/guiglobebrowsingcomponent.h>

#include <modules/imgui/include/imgui_include.h>

#include <modules/globebrowsing/globebrowsingmodule.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>

#include <ghoul/misc/onscopeexit.h>

#include <ghoul/lua/ghoul_lua.h>

#include <fmt/format.h>

#include <numeric>

namespace {
    const ImVec2 WindowSize = ImVec2(350, 500);
} // namespace

namespace openspace::gui {

GuiGlobeBrowsingComponent::GuiGlobeBrowsingComponent()
    : GuiPropertyComponent("GlobeBrowsing")
{}

void GuiGlobeBrowsingComponent::render() {
    GlobeBrowsingModule* module = OsEng.moduleEngine().module<GlobeBrowsingModule>();
    using UrlInfo = GlobeBrowsingModule::UrlInfo;
    using Capabilities = GlobeBrowsingModule::Capabilities;
    using Layer = GlobeBrowsingModule::Layer;

    bool e = _isEnabled;
    e = e;

    ImGui::Begin("Globe Browsing", &e, WindowSize, 0.5f);
    _isEnabled = e;
    OnExit([]() {ImGui::End(); }); // We escape early from this function in a few places


    // Render the list of planets
    std::vector<SceneGraphNode*> nodes =
        OsEng.renderEngine().scene()->allSceneGraphNodes();

    nodes.erase(
        std::remove_if(
            nodes.begin(),
            nodes.end(),
            [](SceneGraphNode* n) {
                return !(n->renderable() && n->renderable()->name() == "RenderableGlobe");
            }
        ),
        nodes.end()
    );
    std::sort(
        nodes.begin(),
        nodes.end(),
        [](SceneGraphNode* lhs, SceneGraphNode* rhs) { return lhs->name() < rhs->name(); }
    );
    std::string nodeNames;
    for (SceneGraphNode* n : nodes) {
        nodeNames += n->name() + '\0';
    }

    int iNode = -1;
    if (_currentNode.empty()) {
        // We haven't selected a node yet, so first instinct is to use the current focus
        // node

        // Check if the focus node is a RenderableGlobe
        const SceneGraphNode* const focus = OsEng.navigationHandler().focusNode();
        auto it = std::find(nodes.cbegin(), nodes.cend(), focus);
        if (it != nodes.end()) {
            _currentNode = focus->name();
            iNode = static_cast<int>(std::distance(nodes.cbegin(), it));
        }
    }
    else {
        auto it = std::find_if(
            nodes.cbegin(),
            nodes.cend(),
            [this](SceneGraphNode* lhs) {
                return lhs->name() == _currentNode;
            }
        );
        iNode = static_cast<int>(std::distance(nodes.cbegin(), it));
    }

    bool nodeChanged = ImGui::Combo("Globe", &iNode, nodeNames.c_str());

    ImGui::SameLine();
    bool selectFocusNode = ImGui::Button("From Focus");
    if (selectFocusNode) {
        const SceneGraphNode* const focus = OsEng.navigationHandler().focusNode();
        auto it = std::find(nodes.cbegin(), nodes.cend(), focus);
        if (it != nodes.end()) {
            _currentNode = focus->name();
            iNode = static_cast<int>(std::distance(nodes.cbegin(), it));
            nodeChanged = true;
        }
    }

    if (iNode == -1) {
        // This should only occur if the Focusnode is not a RenderableGlobe
        // or if there are no nodes
        return;
    }
    _currentNode = nodes[iNode]->name();

    if (nodeChanged) {
        _currentServer = "";
    }

    ImGui::Separator();

    // Render the list of servers for the planet
    std::vector<UrlInfo> urlInfo = module->urlInfo(_currentNode);

    std::string serverList = std::accumulate(
        urlInfo.cbegin(),
        urlInfo.cend(),
        std::string(),
        [](std::string lhs, const UrlInfo& i) {
            return lhs + i.name + ": (" + i.url + ")" + '\0';
        }
    );

    int iServer = -1;
    if (_currentServer.empty()) {
        // We haven't selected a server yet, so first instinct is to just use the first
        if (!urlInfo.empty()) {
            _currentServer = urlInfo[0].name;
            iServer = 0;
        }
    }
    else {
        auto it = std::find_if(
            urlInfo.cbegin(),
            urlInfo.cend(),
            [this](const UrlInfo& i) {
                return i.name == _currentServer;
            }
        );
        if (it != urlInfo.end()) {
            iServer = static_cast<int>(std::distance(urlInfo.cbegin(), it));
        }
    }

    ImGui::Combo("Server", &iServer, serverList.c_str());

    ImGui::SameLine(0.f, 60.f);

    if (ImGui::Button("Add Server")) {
        ImGui::OpenPopup("globebrowsing_add_server");
    }

    if (ImGui::BeginPopup("globebrowsing_add_server")) {
        constexpr int InputBufferSize = 512;
        static char NameInputBuffer[InputBufferSize];
        static char UrlInputBuffer[InputBufferSize];
        ImGui::InputText("Server Name", NameInputBuffer, InputBufferSize);

        ImGui::InputText("Server URL", UrlInputBuffer, InputBufferSize);

        bool addServer = ImGui::Button("Add Server");
        if (addServer && (!_currentNode.empty())) {
            module->loadWMSCapabilities(
                std::string(NameInputBuffer),
                _currentNode,
                std::string(UrlInputBuffer)
            );
            std::memset(NameInputBuffer, 0, InputBufferSize * sizeof(char));
            std::memset(UrlInputBuffer, 0, InputBufferSize * sizeof(char));

            urlInfo = module->urlInfo(_currentNode);
            _currentServer = urlInfo.back().name;
            --iServer;
            ImGui::CloseCurrentPopup();
        }
        ImGui::EndPopup();
    }
    ImGui::SameLine(0.f, 20.f);

    bool deleteServer = ImGui::Button("Delete Server");
    if (deleteServer) {
        module->removeWMSServer(_currentServer);
        _currentServer = "";
        iServer = -1;

    }

    if (iServer == -1) {
        return;
    }
    _currentServer = urlInfo[iServer].name;



    // Can't use urlIt here since it might have been invalidated before
    if (urlInfo.empty()) {
        // There are no server so we have to bail
        return;
    }

    ImGui::Separator();

    Capabilities cap = module->capabilities(_currentServer);

    if (cap.empty()) {
        LWARNINGC(
            "GlobeBrowsingGUI",
            "Unknown server: '" << _currentServer << "'"
        );
    }

    ImGui::Columns(6, nullptr, false);

    float width = ImGui::GetWindowWidth();
    constexpr float ButtonWidth = 60.f;
    ImGui::SetColumnOffset(5, width - 1.5f * ButtonWidth);
    ImGui::SetColumnOffset(4, width - 2.5f * ButtonWidth);
    ImGui::SetColumnOffset(3, width - 3.5f * ButtonWidth);
    ImGui::SetColumnOffset(2, width - 4.5f * ButtonWidth);
    ImGui::SetColumnOffset(1, width - 5.5f * ButtonWidth);
    ImGui::SetColumnOffset(0, 0);

    //ImGui::PushItemWidth(500.f);
    ImGui::Text("%s", "Layer name");
    ImGui::NextColumn();
    ImGui::Text("%s", "Add as ...");
    ImGui::NextColumn();
    ImGui::NextColumn();
    ImGui::NextColumn();
    ImGui::NextColumn();
    ImGui::NextColumn();
    ImGui::Separator();

    for (const Layer& l : cap) {
        if (l.name.empty() || l.url.empty()) {
            continue;
        }

        ImGui::PushID(l.url.c_str());

        ImGui::Text("%s", l.name.c_str());
        ImGui::NextColumn();

        bool addColor = ImGui::Button("Color", { ButtonWidth, 25.f });
        ImGui::NextColumn();

        bool addNight = ImGui::Button("Night", { ButtonWidth, 25.f });
        ImGui::NextColumn();

        bool addOverlay = ImGui::Button("Overlay", { ButtonWidth, 25.f });
        ImGui::NextColumn();

        bool addHeight = ImGui::Button("Height", { ButtonWidth, 25.f });
        ImGui::NextColumn();

        bool addWaterMask = ImGui::Button("Water", { ButtonWidth, 25.f });
        ImGui::NextColumn();

        auto addFunc = [this, &l](const std::string& type) {
            std::string layerName = l.name;
            std::replace(layerName.begin(), layerName.end(), '.', '-');
            OsEng.scriptEngine().queueScript(
                fmt::format(
                    "openspace.globebrowsing.addLayer(\
                        '{}', \
                        '{}', \
                        {{ Name = '{}', FilePath = '{}', Enabled = true }}\
                    );",
                    _currentNode,
                    type,
                    layerName,
                    l.url
                ),
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        };

        if (addColor) {
            addFunc("ColorLayers");
        }
        if (addNight) {
            addFunc("NightLayers");
        }
        if (addOverlay) {
            addFunc("Overlays");
        }
        if (addHeight) {
            addFunc("HeightLayers");
        }
        if (addWaterMask) {
            addFunc("WaterMasks");
        }

        ImGui::PopID();
    }
    ImGui::Columns(1);
}

} // namespace openspace::gui
