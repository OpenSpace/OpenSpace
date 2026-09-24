/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <modules/exoplanetsexperttool/views/viewhelper.h>
#include <format>
#include <string>

namespace openspace::view {

namespace helper {

ImVec4 toImVec4(const glm::vec4& v) {
    return ImVec4(v.x, v.y, v.z, v.w);
}

void renderDescriptiveText(const char* text) {
    ImGui::TextColored(toImVec4(colors::DescriptiveText), text);
}

void renderHelpMarker(const char* text) {
    ImGui::TextDisabled("(?)");
    if (ImGui::IsItemHovered()) {
        ImGui::BeginTooltip();
        ImGui::PushTextWrapPos(ImGui::GetFontSize() * 35.0f);
        ImGui::TextUnformatted(text);
        ImGui::PopTextWrapPos();
        ImGui::EndTooltip();
    }
}

void renderTruncatedTextWithTooltip(std::string_view text, size_t maxLength,
                                    std::string_view suffix)
{
    const bool isTruncated = text.length() > maxLength;
    const std::string truncated = isTruncated ?
        std::string(text.substr(0, maxLength > 3 ? maxLength - 3 : maxLength)) + "..." :
        std::string(text);

    if (suffix.empty()) {
        ImGui::TextUnformatted(truncated.c_str());
        if (isTruncated && ImGui::IsItemHovered()) {
            ImGui::SetTooltip("%.*s", static_cast<int>(text.length()), text.data());
        }
    }
    else {
        ImGui::Text("%s%.*s", truncated.c_str(), static_cast<int>(suffix.length()), suffix.data());
        if (isTruncated && ImGui::IsItemHovered()) {
            ImGui::SetTooltip("%.*s%.*s", static_cast<int>(text.length()), text.data(),
                static_cast<int>(suffix.length()), suffix.data());
        }
    }
}

} // namespace helper

} // namespace openspace::view
