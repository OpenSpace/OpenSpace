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

#include <modules/imgui/include/guiassetcomponent.h>

#include <modules/imgui/include/imgui_include.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/scene/assetmanager.h>
#include <openspace/scene/asset.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>

namespace {
    std::string assetStateToString(openspace::Asset::State state) {
        using State = openspace::Asset::State;

        switch (state) {
        case State::Loaded: return "Loaded";
        case State::LoadingFailed: return "LoadingFailed";
        case State::Synchronizing: return "Synchronizing";
        case State::SyncRejected: return "SyncRejected";
        case State::SyncResolved: return "SyncResolved";
        case State::Initialized: return "Initialized";
        case State::InitializationFailed: return "InitializationFailed";
        default: return "Unknown";
        }
    }

    std::string syncStateToString(openspace::ResourceSynchronization::State state) {
        using State = openspace::ResourceSynchronization::State;

        switch (state) {
        case State::Unsynced: return "Unsynced";
        case State::Syncing: return "Syncing";
        case State::Resolved: return "Resolved";
        case State::Rejected: return "Rejected";
        default: return "Unknown";
        }
    }
}

namespace openspace::gui {

GuiAssetComponent::GuiAssetComponent()
    : GuiComponent("Assets")
{}


void GuiAssetComponent::render() {
    bool e = _isEnabled;
    ImGui::Begin("Assets", &e);
    _isEnabled = e;

    AssetManager& assetManager = global::openSpaceEngine.assetManager();

    std::string rootPath;

    for (const std::shared_ptr<Asset>& a : assetManager.rootAsset()->childAssets()) {
        renderTree(*a, rootPath);
    }

    ImGui::End();
}

void GuiAssetComponent::renderTree(const Asset& asset, const std::string& relativeToPath)
{
    using namespace ghoul::filesystem;

    std::string assetPath = asset.assetFilePath();
    const std::string& assetDirectory = File(assetPath).directoryName();

    if (!relativeToPath.empty()) {
        assetPath = FileSys.relativePath(assetPath, relativeToPath);
    }

    std::string assetText = assetPath + " " + assetStateToString(asset.state());

    if (asset.state() == Asset::State::Synchronizing) {
        int prog = static_cast<int>(asset.requiredSynchronizationProgress() * 100);
        assetText += " (" + std::to_string(prog) + "%)";
    }

    const std::vector<std::shared_ptr<Asset>>& requested = asset.requestedAssets();
    const std::vector<std::shared_ptr<Asset>>& required = asset.requiredAssets();

    const std::vector<std::shared_ptr<ResourceSynchronization>>& resourceSyncs =
        asset.ownSynchronizations();

    if (requested.empty() && required.empty() && resourceSyncs.empty()) {
        ImGui::Text("%s", assetText.c_str());
    } else if (ImGui::TreeNode(assetPath.c_str(), "%s", assetText.c_str())) {
        for (const std::shared_ptr<Asset>& child : required) {
            renderTree(*child, assetDirectory);
        }

        if (!requested.empty() && ImGui::TreeNode("Requested assets")) {
            for (const std::shared_ptr<Asset>& child : requested) {
                renderTree(*child, assetDirectory);
            }
            ImGui::TreePop();
        }

        if (!resourceSyncs.empty() && ImGui::TreeNode("Resource Synchronizations")) {
            for (const std::shared_ptr<ResourceSynchronization>& sync : resourceSyncs) {
                std::string resourceText = sync->directory() +
                    " " + syncStateToString(sync->state());
                if (sync->state() == ResourceSynchronization::State::Syncing) {
                    resourceText += " (" + std::to_string(
                        static_cast<int>(sync->progress() * 100)
                        ) + "%)";
                }
                ImGui::Text("%s", resourceText.c_str());
            }
            ImGui::TreePop();
        }

        ImGui::TreePop();
    }
}

} // namespace openspace::gui
