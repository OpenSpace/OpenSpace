/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/imgui/include/guimemorycomponent.h>

#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/memorymanager.h>

namespace {
    const ImVec2 Size = ImVec2(350, 500);

    template <typename MemoryPool>
    void renderMemoryPoolInformation(const MemoryPool& p) {
        //ImGui::Text("Bucket Size: %i", p.BucketSize);
        ImGui::Text("Number of Buckets: %i", p.nBuckets());
        const std::vector<int>& occupancies = p.occupancies();
        for (size_t i = 0; i < occupancies.size(); i++) {
            ImGui::Text(
                "  %i: %i/%i (%.2f/%.2f kiB)",
                static_cast<int>(i),
                occupancies[i],
                p._bucketSize,
                occupancies[i] / 1024.f,
                p._bucketSize / 1024.f
            );
        }
    }
} // namespace

namespace openspace::gui {

GuiMemoryComponent::GuiMemoryComponent()
    : GuiComponent("memory_information", "Memory Information")
{}

void GuiMemoryComponent::render() {
    ImGui::SetNextWindowCollapsed(_isCollapsed);

    bool v = _isEnabled;
    ImGui::SetNextWindowSize(Size, ImGuiCond_FirstUseEver);
    ImGui::SetNextWindowBgAlpha(0.5f);
    ImGui::Begin("Memory Information", &v);
    _isEnabled = v;
    _isCollapsed = ImGui::IsWindowCollapsed();

    uint64_t ram = global::openSpaceEngine->ramInUse();
    ImGui::Text("RAM: %lu (%f MiB)", ram, ram / (1024.f * 1024.f));
    uint64_t vram = global::openSpaceEngine->vramInUse();
    ImGui::Text("VRAM: %lu (%f MiB)", vram, vram / (1024.f * 1024.f));

    ImGui::Spacing();

    ImGui::Text("%s", "Persistent Memory Pool");
    renderMemoryPoolInformation(global::memoryManager->PersistentMemory);

    ImGui::End();
}

} // namespace openspace::gui
