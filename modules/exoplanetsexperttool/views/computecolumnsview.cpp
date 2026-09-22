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

#include <modules/exoplanetsexperttool/views/computecolumnsview.h>

#include <modules/exoplanetsexperttool/dataviewer.h>
#include <modules/exoplanetsexperttool/expressionparser.h>
#include <modules/exoplanetsexperttool/views/viewhelper.h>
#include <modules/imgui/include/imgui_include.h>
#include <algorithm>
#include <cfloat>
#include <cmath>
#include <cstring>
#include <format>
#include <limits>

namespace openspace::exoplanets {

ComputeColumnsView::ComputeColumnsView(DataViewer& dataViewer,
                                       const DataSettings& dataSettings)
    : _dataViewer(dataViewer)
{

}

bool ComputeColumnsView::appendColumnToExpression(const std::string& columnName) {
    const size_t currentLength = std::strlen(_expressionBuffer);
    const bool needsSeparator = currentLength > 0 &&
        _expressionBuffer[currentLength - 1] != ' ' &&
        _expressionBuffer[currentLength - 1] != '\n' &&
        _expressionBuffer[currentLength - 1] != '\t' &&
        _expressionBuffer[currentLength - 1] != '\r';
    const size_t separatorLength = needsSeparator ? 1 : 0;
    constexpr size_t bufferSize = sizeof(_expressionBuffer);

    if (currentLength + separatorLength + columnName.size() >= bufferSize) {
        _errorMessage = "Column cannot be added: expression is too long";
        return false;
    }

    if (needsSeparator) {
        _expressionBuffer[currentLength] = ' ';
    }
    std::memcpy(
        _expressionBuffer + currentLength + separatorLength,
        columnName.c_str(),
        columnName.size()
    );
    _expressionBuffer[currentLength + separatorLength + columnName.size()] = '\0';
    _errorMessage.clear();
    return true;
}

void ComputeColumnsView::renderColumnBrowser() {
    if (!_showColumnBrowser) {
        return;
    }

    ImGui::SetNextWindowSize(ImVec2(560.f, 420.f), ImGuiCond_FirstUseEver);
    ImGui::SetNextWindowPos(
        ImGui::GetMainViewport()->GetCenter(),
        ImGuiCond_Appearing,
        ImVec2(0.5f, 0.5f)
    );

    if (!ImGui::Begin("Available numeric columns", &_showColumnBrowser)) {
        ImGui::End();
        return;
    }

    static ImGuiTextFilter columnFilter;
    const float availableWidth = ImGui::GetContentRegionAvail().x;
    columnFilter.Draw("Filter columns", availableWidth);

    if (ImGui::BeginChild("ColumnList", ImVec2(0.f, 0.f), true)) {
        ImGui::TextUnformatted("Loaded columns");
        ImGui::Separator();

        for (const ColumnKey& column : _dataViewer.columns()) {
            if (!_dataViewer.isNumericColumn(column)) {
                continue;
            }

            const char* displayName = _dataViewer.columnName(column);
            if (!columnFilter.PassFilter(displayName) &&
                !columnFilter.PassFilter(column.c_str()))
            {
                continue;
            }

            ImGui::PushID(column.c_str());
            if (ImGui::Selectable(displayName)) {
                appendColumnToExpression(column);
            }
            if (ImGui::IsItemHovered()) {
                ImGui::BeginTooltip();
                ImGui::Text("Identifier: %s", column.c_str());
                if (_dataViewer.hasColumnDescription(column)) {
                    ImGui::TextWrapped("%s", _dataViewer.columnDescription(column));
                }
                ImGui::EndTooltip();
            }
            ImGui::PopID();
        }

        if (!_computedColumns.empty()) {
            ImGui::Spacing();
            ImGui::TextUnformatted("Computed columns");
            ImGui::Separator();

            for (const auto& entry : _computedColumns) {
                const std::string& column = entry.first;
                if (!columnFilter.PassFilter(column.c_str())) {
                    continue;
                }

                ImGui::PushID(column.c_str());
                if (ImGui::Selectable(column.c_str())) {
                    appendColumnToExpression(column);
                }
                if (ImGui::IsItemHovered()) {
                    ImGui::SetTooltip("Computed column; no description available");
                }
                ImGui::PopID();
            }
        }

        ImGui::EndChild();
    }

    ImGui::End();
}

bool ComputeColumnsView::isNameTaken(const std::string& name) const {
    if (_computedColumns.contains(name)) {
        return true;
    }
    const std::vector<ColumnKey>& columns = _dataViewer.columns();
    return std::find(columns.begin(), columns.end(), name) != columns.end();
}

bool ComputeColumnsView::computeColumn(const std::string& name,
                                        const std::string& expressionText)
{
    if (name.empty()) {
        _errorMessage = "Column name cannot be empty";
        return false;
    }
    if (isNameTaken(name)) {
        _errorMessage = std::format("Column name '{}' is already in use", name);
        return false;
    }

    const Expression expression = Expression::parse(expressionText);
    if (!expression.isValid()) {
        _errorMessage = expression.errorMessage();
        return false;
    }

    bool foundUnknownVariable = false;
    std::string unknownVariableName;
    const std::vector<ExoplanetItem>& data = _dataViewer.data();

    std::vector<float> result;
    result.reserve(data.size());

    for (size_t rowIndex = 0; rowIndex < data.size(); rowIndex++) {
        const ExoplanetItem& item = data[rowIndex];
        auto resolveVariable = [this, &item, rowIndex, &foundUnknownVariable,
                                 &unknownVariableName](const std::string& varName)
        {
            if (auto it = _computedColumns.find(varName); it != _computedColumns.end()) {
                return it->second[rowIndex];
            }
            const std::vector<ColumnKey>& columns = _dataViewer.columns();
            const bool isKnownColumn =
                std::find(columns.begin(), columns.end(), varName) != columns.end();

            if (!isKnownColumn) {
                foundUnknownVariable = true;
                unknownVariableName = varName;
                return std::numeric_limits<float>::quiet_NaN();
            }
            if (!_dataViewer.isNumericColumn(varName)) {
                return std::numeric_limits<float>::quiet_NaN();
            }
            return std::get<float>(_dataViewer.columnValue(varName, item));
        };

        result.push_back(expression.evaluate(resolveVariable));

        if (foundUnknownVariable) {
            _errorMessage = std::format("Unknown column: '{}'", unknownVariableName);
            return false;
        }
    }

    _computedColumns[name] = std::move(result);
    _errorMessage.clear();
    return true;
}

void ComputeColumnsView::render(bool* open) {
    if (!ImGui::Begin("Compute data columns", open)) {
        ImGui::End();
        return;
    }

    ImGui::InputText("Column name", _nameBuffer, IM_ARRAYSIZE(_nameBuffer));

    ImGui::Text("Expression");
    ImGui::SameLine();
    if (ImGui::Button("Browse columns")) {
        _showColumnBrowser = true;
    }
    ImGui::SameLine();
    view::helper::renderHelpMarker(
        "Enter an arithmetic expression using existing column names as variables, e.g. "
        "'mass / radius^2' (use pow(mass, 2) instead of '^'). Supported functions: "
        "sqrt, log, log10, abs, pow, min, max."
    );

    ImGui::InputTextMultiline(
        "##Expression",
        _expressionBuffer,
        IM_ARRAYSIZE(_expressionBuffer),
        ImVec2(-FLT_MIN, ImGui::GetTextLineHeight() * 4),
        ImGuiInputTextFlags_AllowTabInput
    );

    if (ImGui::Button("Compute")) {
        if (computeColumn(_nameBuffer, _expressionBuffer)) {
            _nameBuffer[0] = '\0';
            _expressionBuffer[0] = '\0';
        }
    }

    if (!_errorMessage.empty()) {
        ImGui::TextColored(
            view::helper::toImVec4(view::colors::Error),
            "%s",
            _errorMessage.c_str()
        );
    }

    ImGui::Separator();
    ImGui::Text("Computed columns");

    std::string columnToRemove;
    for (const auto& [name, values] : _computedColumns) {
        ImGui::PushID(name.c_str());

        if (ImGui::Button("x")) {
            columnToRemove = name;
        }
        ImGui::SameLine();

        float minValue = std::numeric_limits<float>::max();
        float maxValue = std::numeric_limits<float>::lowest();
        for (float v : values) {
            if (!std::isnan(v)) {
                minValue = std::min(minValue, v);
                maxValue = std::max(maxValue, v);
            }
        }
        ImGui::Text("%s (min: %f, max: %f)", name.c_str(), minValue, maxValue);

        ImGui::PopID();
    }
    if (!columnToRemove.empty()) {
        _computedColumns.erase(columnToRemove);
    }

    ImGui::End();
    renderColumnBrowser();
}

} // namespace openspace::exoplanets
