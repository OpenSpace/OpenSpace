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

#include <modules/imgui/include/guiassetcomponent.h>

#include <modules/imgui/include/imgui_include.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/scene/assetmanager.h>
#include <openspace/scene/asset.h>

namespace openspace::gui {

GuiAssetComponent::GuiAssetComponent()
    : GuiComponent("Assets")
{}

void GuiAssetComponent::render() {
    bool v = _isEnabled;
    ImGui::Begin("Assets", &v);

    ImGui::Columns(2);
    ImGui::Separator();

    AssetManager& assetManager = OsEng.assetManager();

    std::vector<std::shared_ptr<Asset>> allAssets =
        assetManager.rootAsset()->subTreeAssets();

    for (const auto& a : allAssets) {
        ImGui::Text("%s", a->assetFilePath().c_str());
        ImGui::NextColumn();
        
        std::string stateText;
        switch(a->state()) {
            case Asset::State::Loaded: stateText = "Loaded"; break;
            case Asset::State::LoadingFailed: stateText = "LoadingFailed"; break;
            case Asset::State::Synchronizing: stateText = "Synchronizing"; break;
            case Asset::State::SyncRejected: stateText = "SyncRejected"; break;
            case Asset::State::SyncResolved: stateText = "SyncResolved"; break;
            case Asset::State::Initialized: stateText = "Initialized"; break;
            case Asset::State::InitializationFailed: stateText = "InitializationFailed"; break;
            default: stateText = "Unknown"; break;
        }
        ImGui::Text("%s", stateText.c_str());
        
        ImGui::NextColumn();
        ImGui::Separator();
/*
        ImGui::Text("%s", t.c_str());
        ImGui::NextColumn();
        ImGui::Text("%s", absPath(t).c_str());
        ImGui::NextColumn();
        ImGui::Separator();*/
    }
    ImGui::End();
}

} // namespace openspace::gui
