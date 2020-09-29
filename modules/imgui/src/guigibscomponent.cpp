/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <modules/imgui/include/guigibscomponent.h>

#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/scripting/scriptengine.h>
#include <array>
#pragma optimize ("", off)
namespace {
    const ImVec2 WindowSize = ImVec2(350, 500);
} // namespace

namespace openspace::gui {

GuiGIBSComponent::GuiGIBSComponent()
    : GuiComponent("GIBS", "GIBS")
{}

void GuiGIBSComponent::render() {
    ImGui::SetNextWindowCollapsed(_isCollapsed);
    bool e = _isEnabled;
    ImGui::Begin("GIBS", &e, WindowSize, 0.5f);
    _isEnabled = e;
    _isCollapsed = ImGui::IsWindowCollapsed();

    ImGui::Text("%s", "GIBS Layers");
    ImGui::Text("%s", "Find the information on the GIBS layer page at:");
    ImGui::Text(
        "%s",
        "https://wiki.earthdata.nasa.gov/display/GIBS/GIBS+Available+Imagery+Products"
    );
    ImGui::Separator();

    static std::array<char, 256> LayerBuffer;
    ImGui::InputText("Layer Name", LayerBuffer.data(), LayerBuffer.size());

    static std::array<char, 64> StartDateBuffer;
    ImGui::InputText("Start Date", StartDateBuffer.data(), StartDateBuffer.size());

    static std::array<char, 64> EndDateBuffer = { "Yesterday" };
    ImGui::InputText("End Date", EndDateBuffer.data(), EndDateBuffer.size());

    static std::array<char, 64> TemporalResolutionBuffer = { "1d" };
    ImGui::InputText(
        "Temporal Resolution",
        TemporalResolutionBuffer.data(),
        TemporalResolutionBuffer.size()
    );

    //static std::array<char, 64> TemporalFormatBuffer;
    // @TODO Replace with dropdown menu
    constexpr std::array<const char*, 5> TemporalFormats = {
        // @FRAGILE: Synchronized with tileprovider.cpp `from_string` method
        "YYYY-MM-DD",
        "YYYY-MM-DDThh:mm:ssZ",
        "YYYY-MM-DDThh_mm_ssZ",
        "YYYYMMDD_hhmmss",
        "YYYYMMDD_hhmm"
    };
    static const char* currentTemporalFormat = "YYYY-MM-DD";
    if (ImGui::BeginCombo("##Temporal Format", currentTemporalFormat)) {
        for (const char* f : TemporalFormats) {
            bool isSelected = (f == currentTemporalFormat);
            if (ImGui::Selectable(f, &isSelected)) {
                currentTemporalFormat = f;
            }
        }
        ImGui::EndCombo();
    }

    static std::array<char, 64> ImageResolutionBuffer = { "250m" };
    ImGui::InputText(
        "Image Resolution",
        ImageResolutionBuffer.data(),
        ImageResolutionBuffer.size()
    );

    static std::array<char, 64> ImageFormatBuffer = { "png" };
    ImGui::InputText("Image Format", ImageFormatBuffer.data(), ImageFormatBuffer.size());

    if (ImGui::Button("Add Layer")) {
        std::string layer = std::string(LayerBuffer.data());
        std::string startDate = std::string(StartDateBuffer.data());
        std::string endDate = std::string(EndDateBuffer.data());
        std::string temporalRes = std::string(TemporalResolutionBuffer.data());
        std::string temporalFormat = std::string(currentTemporalFormat);
        std::string imageRes = std::string(ImageResolutionBuffer.data());
        std::string imageFormat = std::string(ImageFormatBuffer.data());

        std::string xmlFunc = fmt::format(
            "openspace.globebrowsing.createTemporalGibsGdalXml("
            "'{}', '{}', '{}', '{}', '{}', '{}', '{}')",
            layer, startDate, endDate, temporalRes, imageRes, imageFormat, temporalFormat
        );

        std::string script = fmt::format(
            "openspace.globebrowsing.addLayer('Earth', 'ColorLayers', {{ "
            "Identifier = '{}', Enabled = true, Type = 'TemporalTileLayer', "
            "FilePath = {} }})",
            layer, xmlFunc
        );
        global::scriptEngine.queueScript(
            script,
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    ImGui::End();
}

} // namespace openspace::gui
