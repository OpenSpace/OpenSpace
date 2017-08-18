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

 //#include <openspace/scripting/scriptengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>

#include <ghoul/misc/onscopeexit.h>

#include <gdal.h>

namespace {
    const ImVec2 WindowSize = ImVec2(350, 500);
} // namespace

namespace openspace::gui {

GuiGlobeBrowsingComponent::GuiGlobeBrowsingComponent()
    : GuiPropertyComponent("GlobeBrowsing")
    , _currentNode(-1)
    , _currentServer(-1)
{}

void GuiGlobeBrowsingComponent::render() {
    bool e = _isEnabled;
    e = e; 

    ImGui::Begin("Globe Browsing", &e, WindowSize, 0.5f);
    _isEnabled = e;
    OnExit([]() {ImGui::End(); }); // We escape early from this function


    // Render the list of planets
    std::vector<SceneGraphNode*> nodes = OsEng.renderEngine().scene()->allSceneGraphNodes();
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
    ImGui::Combo("Globe", &_currentNode, nodeNames.c_str());

    if (_currentNode == -1) {
        return;
    }


    // Render the list of servers for the planet
    std::string serverList;
    if (_currentNode != -1) {
        auto it = _urlMap.find(nodes[_currentNode]->name());

        if (it != _urlMap.end()) {
            for (std::string s : it->second) {
                serverList += s + '\0';
            }
        }
    }
    ImGui::Combo("Servers", &_currentServer, serverList.c_str());

    // Add server
    static char Buffer[256];
    ImGui::InputText("", Buffer, 256);

    ImGui::SameLine();
    bool addServer = ImGui::Button("Add Server");
    if (addServer && (_currentNode != -1)) {
        std::string currentNode = nodes[_currentNode]->name();
        auto it = _urlMap.find(currentNode);
        if (it != _urlMap.end()) {
            it->second.push_back(std::string(Buffer));
            _currentServer = it->second.size() - 1;
        }
        else {
            _urlMap[nodes[_currentNode]->name()] = { std::string(Buffer) };
            _currentServer = 0;
        }
        std::memset(Buffer, 0, 256 * sizeof(char));
    }

    if (_currentServer == -1) {
        return;
    }

    std::string currentServer = _urlMap.find(nodes[_currentNode]->name())->second[_currentServer];

    // If the capabilities haven't been requested yet, do so
    Capabilities& cap = [&currentServer, this]() -> Capabilities& {
        auto it = _capabilities.find(currentServer);
        if (it != _capabilities.end()) {
            return it->second;
        }
        else {
            _capabilities[currentServer] = Capabilities();
            return _capabilities[currentServer];
        }
    }();

    if (!cap.isRequested) {
        LDEBUGC("GlobeBrowsingGui", "File '" << nodes[_currentNode]->name() << "'/'" << currentServer << "' requested");
        std::future<DownloadManager::MemoryFile> f = OsEng.downloadManager().fetchFile(
            currentServer,
            [&cap](const DownloadManager::MemoryFile& f) {
                if (f.corrupted) {
                    LERRORC("GlobeBrowsingGui", "File is corrupted");
                }
                else {
                    std::string res(f.size, ' ');
                    std::memmove(res.data(), f.buffer, f.size);

                    GDALOpen();

                    cap.values.push_back(std::move(res));
                    cap.isReceived = true;
                }
            }
        );
        cap.isRequested = true;
    }
    
    // If values have been received, display them
    if (cap.isReceived) {
        for (std::string v : cap.values) {
            ImGui::Text("%s", v.c_str());
        }
    }
}

} // namespace openspace::gui
