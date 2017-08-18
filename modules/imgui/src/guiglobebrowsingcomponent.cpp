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

#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>

#include <ghoul/misc/onscopeexit.h>

#include <gdal.h>
#include <cpl_string.h>

#include <fmt/format.h>

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

    if (_currentNode == -1) {
        // We haven't selected a node yet, so first instinct is to use the current focus
        // node
        const SceneGraphNode* const focus = OsEng.navigationHandler().focusNode();
        auto it = std::find(nodes.begin(), nodes.end(), focus);
if (it != nodes.end()) {
    _currentNode = std::distance(nodes.begin(), it);
}
    }

    ImGui::Combo("Globe", &_currentNode, nodeNames.c_str());

    if (_currentNode == -1) {
        // This should only occur if the Focusnode is not a RenderableGlobe
        return;
    }

    ImGui::Separator();

    std::string currentNode = nodes[_currentNode]->name();

    // Render the list of servers for the planet

    auto urlIt = _urlMap.find(currentNode);

    std::string serverList;
    if (_currentNode != -1) {

        if (urlIt != _urlMap.end()) {
            for (std::string s : urlIt->second) {
                serverList += s + '\0';
            }
        }
    }
    ImGui::Combo("Servers", &_currentServer, serverList.c_str());

    // Add server
    constexpr int InputBufferSize = 512;
    static char InputBuffer[InputBufferSize];
    ImGui::InputText("", InputBuffer, 256);

    ImGui::SameLine();
    bool addServer = ImGui::Button("Add Server");
    if (addServer && (_currentNode != -1)) {
        if (urlIt != _urlMap.end()) {
            urlIt->second.push_back(std::string(InputBuffer));
            _currentServer = urlIt->second.size() - 1;
        }
        else {
            _urlMap[currentNode] = { std::string(InputBuffer) };
            _currentServer = 0;
        }
        std::memset(InputBuffer, 0, InputBufferSize * sizeof(char));
    }

    if (_currentServer == -1) {
        return;
    }

    ImGui::Separator();

    // Can't use urlIt here since it might have been invalidated before
    std::string currentServer = _urlMap.find(currentNode)->second[_currentServer];

    // If the capabilities haven't been created yet, do so
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

    if (!cap.hasData) {
        GDALDatasetH dataset = GDALOpenEx(
            currentServer.c_str(),
            GDAL_OF_READONLY | GDAL_OF_RASTER | GDAL_OF_VERBOSE_ERROR,
            nullptr,
            nullptr,
            nullptr
        );

        char** subDatasets = GDALGetMetadata(dataset, "SUBDATASETS");
        int nSubdatasets = CSLCount(subDatasets);
        cap.layers = parseSubDatasets(subDatasets, nSubdatasets);

        GDALClose(dataset);
        cap.hasData = true;
    }

    ImGui::Columns(6, nullptr, false);

    float width = ImGui::GetWindowWidth();
    constexpr float ButtonWidth = 60.f;
    ImGui::SetColumnOffset(5, width - 1.5 * ButtonWidth);
    ImGui::SetColumnOffset(4, width - 2.5 * ButtonWidth);
    ImGui::SetColumnOffset(3, width - 3.5 * ButtonWidth);
    ImGui::SetColumnOffset(2, width - 4.5 * ButtonWidth);
    ImGui::SetColumnOffset(1, width - 5.5 * ButtonWidth);
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

    for (const Capabilities::Layer& l : cap.layers) {
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

        auto addFunc = [&currentNode, &l](const std::string& type) {
            std::string layerName = l.name;
            std::replace(layerName.begin(), layerName.end(), '.', '-');
            OsEng.scriptEngine().queueScript(
                fmt::format(
                    "openspace.globebrowsing.addLayer(\
                        '{}', \
                        '{}', \
                        {{ Name = '{}', FilePath = '{}', Enabled = true \}}\
                    );",
                    currentNode,
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

std::vector<GuiGlobeBrowsingComponent::Capabilities::Layer>
GuiGlobeBrowsingComponent::parseSubDatasets(char** subDatasets, int nSubdatasets)
{
    // Idea:  Iterate over the list of sublayers keeping a current layer and identify it
    //        by its number.  If this number changes, we know that we have a new layer


    using Layer = Capabilities::Layer;
    std::vector<Layer> result;

    int currentLayerNumber = -1;
    Layer currentLayer;
    for (int i = 0; i < nSubdatasets; ++i) {
        int iDataset = -1;
        static char IdentifierBuffer[64];
        sscanf(
            subDatasets[i],
            "SUBDATASET_%i_%[^=]",
            &iDataset,
            IdentifierBuffer
        );

        

        if (iDataset != currentLayerNumber) {
            // We are done with the previous version
            result.push_back(std::move(currentLayer));
            currentLayer = Layer();
            currentLayerNumber = iDataset;
        }

        std::string identifier = std::string(IdentifierBuffer);
        std::string ds(subDatasets[i]);
        std::string value = ds.substr(ds.find_first_of('=') + 1);

        // The DESC/NAME difference is not a typo
        if (identifier == "DESC") {
            currentLayer.name = value;
        }
        else if (identifier == "NAME") {
            currentLayer.url = value;
        }
        else {
            LINFOC("GlobeBrowsingGUI", "Unknown subdataset identifier: " + identifier);
        }
    }

    return result;
}

} // namespace openspace::gui
