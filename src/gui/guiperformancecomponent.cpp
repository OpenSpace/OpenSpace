/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#include <openspace/gui/guiperformancecomponent.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/misc/sharedmemory.h>
#include <imgui.h>

namespace {
	const std::string _loggerCat = "GuiPerformanceComponent";
}

namespace openspace {
namespace gui {

void GuiPerformanceComponent::initialize() {
}

void GuiPerformanceComponent::render() {
	// Copy and paste from renderengine.cpp::storePerformanceMeasurements method
	const int8_t Version = 0;
	const int nValues = 250;
	const int lengthName = 256;
	const int maxValues = 50;

	struct PerformanceLayout {
		int8_t version;
		int32_t nValuesPerEntry;
		int32_t nEntries;
		int32_t maxNameLength;
		int32_t maxEntries;

		struct PerformanceLayoutEntry {
			char name[lengthName];
			float renderTime[nValues];
			float updateRenderable[nValues];
			float updateEphemeris[nValues];

			int32_t currentRenderTime;
			int32_t currentUpdateRenderable;
			int32_t currentUpdateEphemeris;
		};

		PerformanceLayoutEntry entries[maxValues];
	};

	ImGui::Begin("Performance", &_isEnabled);
	if (OsEng.renderEngine().doesPerformanceMeasurements() &&
			ghoul::SharedMemory::exists(RenderEngine::PerformanceMeasurementSharedData))
	{
		ImGui::SliderFloat2("Min values, max Value", _minMaxValues, 0.f, 10000.f);
		_minMaxValues[1] = std::max(_minMaxValues[0], _minMaxValues[1]);

		if (!_performanceMemory)
			_performanceMemory = new ghoul::SharedMemory(RenderEngine::PerformanceMeasurementSharedData);

		PerformanceLayout* layout = reinterpret_cast<PerformanceLayout*>(_performanceMemory->pointer());

		for (int i = 0; i < layout->nEntries; ++i) {
			const PerformanceLayout::PerformanceLayoutEntry& entry = layout->entries[i];

			if (ImGui::CollapsingHeader(entry.name)) {
				std::string updateEphemerisTime = std::to_string(entry.updateEphemeris[entry.currentUpdateEphemeris - 1]) + "us";
				ImGui::PlotLines("UpdateEphemeris", &entry.updateEphemeris[0], layout->nValuesPerEntry, 0, updateEphemerisTime.c_str(), _minMaxValues[0], _minMaxValues[1], ImVec2(0, 40));

				std::string updateRenderableTime = std::to_string(entry.updateRenderable[entry.currentUpdateRenderable - 1]) + "us";
				ImGui::PlotLines("UpdateRender", &entry.updateRenderable[0], layout->nValuesPerEntry, 0, updateRenderableTime.c_str(), _minMaxValues[0], _minMaxValues[1], ImVec2(0, 40));

				std::string renderTime = std::to_string(entry.renderTime[entry.currentRenderTime - 1]) + "us";
				ImGui::PlotLines("RenderTime", &entry.renderTime[0], layout->nValuesPerEntry, 0, renderTime.c_str(), _minMaxValues[0], _minMaxValues[1], ImVec2(0, 40));
			}
		}
	}
	else {
		ImGui::TextWrapped("Performance monitoring is disabled. Enable with "
						   "'openspace.setPerformanceMeasurement(true)'");
	}

	ImGui::End();
}


} // namespace gui
} // namespace openspace
