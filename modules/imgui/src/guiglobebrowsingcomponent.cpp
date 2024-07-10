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

#include <modules/imgui/include/guiglobebrowsingcomponent.h>

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/src/renderableglobe.h>
#endif // OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <numeric>

namespace {
    const ImVec2 WindowSize = ImVec2(350, 500);
} // namespace

namespace openspace::gui {

GuiGlobeBrowsingComponent::GuiGlobeBrowsingComponent()
    : GuiPropertyComponent("GlobeBrowsing", "Globe Browsing")
{}

void GuiGlobeBrowsingComponent::render() {
#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
    GlobeBrowsingModule* module = global::moduleEngine->module<GlobeBrowsingModule>();
    using UrlInfo = GlobeBrowsingModule::UrlInfo;
    using Capabilities = GlobeBrowsingModule::Capabilities;
    using Layer = GlobeBrowsingModule::Layer;

    ImGui::SetNextWindowCollapsed(_isCollapsed);

    bool e = _isEnabled;

    ImGui::SetNextWindowSize(WindowSize, ImGuiCond_FirstUseEver);
    ImGui::SetNextWindowBgAlpha(0.5f);
    ImGui::Begin("Globe Browsing", &e);
    _isEnabled = e;
    _isCollapsed = ImGui::IsWindowCollapsed();
    defer { ImGui::End(); };

    // Render the list of planets
    std::vector<SceneGraphNode*> nodes =
        global::renderEngine->scene()->allSceneGraphNodes();

    nodes.erase(
        std::remove_if(
            nodes.begin(),
            nodes.end(),
            [](SceneGraphNode* n) {
                using namespace globebrowsing;
                const Renderable* r = n->renderable();
                const RenderableGlobe* rg = dynamic_cast<const RenderableGlobe*>(r);
                return rg == nullptr;
            }
        ),
        nodes.end()
    );
    std::sort(
        nodes.begin(),
        nodes.end(),
        [module](SceneGraphNode* lhs, SceneGraphNode* rhs) {
            const bool lhsHasUrl = module->hasUrlInfo(lhs->identifier());
            const bool rhsHasUrl = module->hasUrlInfo(rhs->identifier());

            if (lhsHasUrl && !rhsHasUrl) {
                return true;
            }
            if (!lhsHasUrl && rhsHasUrl) {
                return false;
            }

            return lhs->guiName() < rhs->guiName();
        }
    );


    auto firstWithoutUrl = std::find_if(
        nodes.begin(),
        nodes.end(),
        [module](SceneGraphNode* n) {
            return !module->hasUrlInfo(n->identifier());
        }
    );
    nodes.insert(firstWithoutUrl, nullptr);

    std::string nodeNames;
    for (SceneGraphNode* n : nodes) {
        // Add separator between nodes with URL and nodes without
        if (n) {
            nodeNames += n->identifier() + '\0';
        }
        else {
            nodeNames += std::string("===== =====") + '\0';
        }
    }

    int iNode = -1;
    if (_currentNode.empty()) {
        // We haven't selected a node yet, so first instinct is to use the current focus
        // node

        // Check if the focus node is a RenderableGlobe
        const SceneGraphNode* const focus =
            global::navigationHandler->orbitalNavigator().anchorNode();

        const auto it = std::find(nodes.cbegin(), nodes.cend(), focus);
        if (it != nodes.end()) {
            _currentNode = focus->identifier();
            iNode = static_cast<int>(std::distance(nodes.cbegin(), it));
        }
    }
    else {
        const auto it = std::find_if(
            nodes.cbegin(),
            nodes.cend(),
            [this](SceneGraphNode* lhs) {
                return lhs && (lhs->identifier() == _currentNode);
            }
        );
        iNode = static_cast<int>(std::distance(nodes.cbegin(), it));
    }

    bool isNodeChanged = ImGui::Combo("Globe", &iNode, nodeNames.c_str());

    ImGui::SameLine();
    const bool selectFocusNode = ImGui::Button("From Focus");
    if (selectFocusNode) {
        const SceneGraphNode* const focus =
            global::navigationHandler->orbitalNavigator().anchorNode();

        const auto it = std::find(nodes.cbegin(), nodes.cend(), focus);
        if (it != nodes.end()) {
            _currentNode = focus->identifier();
            iNode = static_cast<int>(std::distance(nodes.cbegin(), it));
            isNodeChanged = true;
        }
    }

    if (iNode == -1) {
        // This should only occur if the Focusnode is not a RenderableGlobe
        // or if there are no nodes
        return;
    }

    if (!nodes[iNode]) {
        // The user selected the separator
        return;
    }

    _currentNode = nodes[iNode]->identifier();

    if (isNodeChanged) {
        _currentServer = "";
    }

    ImGui::Separator();

    // Render the list of servers for the planet
    std::vector<UrlInfo> urlInfo = module->urlInfo(_currentNode);

    const std::string serverList = std::accumulate(
        urlInfo.cbegin(),
        urlInfo.cend(),
        std::string(),
        [](const std::string& lhs, const UrlInfo& i) {
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
        const auto it = std::find_if(
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
        static std::array<char, InputBufferSize> NameInputBuffer;
        ImGui::InputText("Server Name", NameInputBuffer.data(), InputBufferSize);

        static std::array<char, InputBufferSize> UrlInputBuffer;
        ImGui::InputText("Server URL", UrlInputBuffer.data(), InputBufferSize);

        const bool addServer = ImGui::Button("Add Server");
        if (addServer && (!_currentNode.empty())) {
            module->loadWMSCapabilities(
                std::string(NameInputBuffer.data()),
                _currentNode,
                std::string(UrlInputBuffer.data())
            );
            std::memset(NameInputBuffer.data(), 0, InputBufferSize * sizeof(char));
            std::memset(UrlInputBuffer.data(), 0, InputBufferSize * sizeof(char));

            urlInfo = module->urlInfo(_currentNode);
            _currentServer = urlInfo.back().name;
            --iServer;
            ImGui::CloseCurrentPopup();
        }
        ImGui::EndPopup();
    }
    ImGui::SameLine(0.f, 20.f);

    const bool deleteServer = ImGui::Button("Delete Server");
    if (deleteServer) {
        module->removeWMSServer(_currentServer);
        _currentServer = "";
        iServer = -1;

    }

    if (iServer < 0 || urlInfo.empty()) {
        // There are no server so we have to bail
        return;
    }
    _currentServer = urlInfo[iServer].name;

    ImGui::Separator();

    const Capabilities cap = module->capabilities(_currentServer);

    if (cap.empty()) {
        LWARNINGC("GlobeBrowsing", std::format("Unknown server '{}'", _currentServer));
    }

    ImGui::Columns(6, nullptr, false);

    const float width = ImGui::GetWindowWidth();
    constexpr float ButtonWidth = 60.f;
    ImGui::SetColumnOffset(5, width - 1.5f * ButtonWidth);
    ImGui::SetColumnOffset(4, width - 2.5f * ButtonWidth);
    ImGui::SetColumnOffset(3, width - 3.5f * ButtonWidth);
    ImGui::SetColumnOffset(2, width - 4.5f * ButtonWidth);
    ImGui::SetColumnOffset(1, width - 5.5f * ButtonWidth);
    ImGui::SetColumnOffset(0, 0);

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

        const bool addColor = ImGui::Button("Color", { ButtonWidth, 25.f });
        ImGui::NextColumn();

        const bool addNight = ImGui::Button("Night", { ButtonWidth, 25.f });
        ImGui::NextColumn();

        const bool addOverlay = ImGui::Button("Overlay", { ButtonWidth, 25.f });
        ImGui::NextColumn();

        const bool addHeight = ImGui::Button("Height", { ButtonWidth, 25.f });
        ImGui::NextColumn();

        const bool addWaterMask = ImGui::Button("Water", { ButtonWidth, 25.f });
        ImGui::NextColumn();

        auto addFunc = [n = _currentNode, &l](const std::string& type) {
            std::string layerName = l.name;
            std::replace(layerName.begin(), layerName.end(), '.', '-');
            layerName.erase(
                std::remove(layerName.begin(), layerName.end(), ' '),
                layerName.end()
            );
            global::scriptEngine->queueScript(
                std::format(
                    "openspace.globebrowsing.addLayer(\
                        '{}', \
                        '{}', \
                        {{ \
                            Identifier = '{}',\
                            Name = '{}',\
                            FilePath = '{}',\
                            Enabled = true\
                        }}\
                    );",
                    n,
                    type,
                    layerName,
                    l.name,
                    l.url
                ),
                scripting::ScriptEngine::ShouldBeSynchronized::Yes,
                scripting::ScriptEngine::ShouldSendToRemote::Yes
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
#else
    ImGui::Text("%s", "OpenSpace compiled without GlobeBrowsing support");
#endif // OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
}

} // namespace openspace::gui
