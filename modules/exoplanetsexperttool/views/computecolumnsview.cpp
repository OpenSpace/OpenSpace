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
#include <openspace/json.h>
#include <openspace/util/distanceconstants.h>
#include <algorithm>
#include <array>
#include <cfloat>
#include <charconv>
#include <cmath>
#include <cstring>
#include <format>
#include <fstream>
#include <limits>

namespace openspace::exoplanets {

namespace {

struct NumericConstant {
    const char* name;
    double value;
};

constexpr std::array<NumericConstant, 16> NumericConstants = {{
    { "EarthRadius", distanceconstants::EarthRadius },
    { "JupiterRadius", distanceconstants::JupiterRadius },
    { "SolarRadius", distanceconstants::SolarRadius },
    { "LightYear", distanceconstants::LightYear },
    { "LightMonth", distanceconstants::LightMonth },
    { "LightDay", distanceconstants::LightDay },
    { "LightHour", distanceconstants::LightHour },
    { "LightSecond", distanceconstants::LightSecond },
    { "AstronomicalUnit", distanceconstants::AstronomicalUnit },
    { "Parsec", distanceconstants::Parsec },
    { "Inch", distanceconstants::Inch },
    { "Foot", distanceconstants::Foot },
    { "Yard", distanceconstants::Yard },
    { "Chain", distanceconstants::Chain },
    { "Mile", distanceconstants::Mile },
    { "NauticalMile", distanceconstants::NauticalMile }
}};

std::string formatNumericConstant(double value) {
    std::array<char, 32> buffer = {};
    const auto [end, error] = std::to_chars(
        buffer.data(),
        buffer.data() + buffer.size(),
        static_cast<float>(value)
    );
    return error == std::errc() ? std::string(buffer.data(), end) : std::string();
}

} // namespace

ComputeColumnsView::ComputeColumnsView(DataViewer& dataViewer,
                                       const DataSettings& dataSettings)
    : _dataViewer(dataViewer)
    , _historyFile(dataViewer.computedColumnHistoryFile())
{
    loadHistory();
}

void ComputeColumnsView::loadHistory() {
    if (!std::filesystem::is_regular_file(_historyFile)) {
        return;
    }

    try {
        std::ifstream file(_historyFile);
        const nlohmann::json json = nlohmann::json::parse(file);
        if (json.value("version", 0) != 1 || !json.contains("columns")) {
            _errorMessage = "Computed-column history has an unsupported format";
            return;
        }

        // Parse in reverse order, so we get the most recent first in the list
        const nlohmann::json& columns = json.at("columns");
        for (auto it = columns.rbegin(); it != columns.rend(); ++it) {
            const nlohmann::json& entry = *it;
            const std::string name = entry.value("name", "");
            const std::string expression = entry.value("expression", "");
            const std::string description = entry.value("description", "");
            if (!name.empty() && !expression.empty()) {
                _history.push_back({ name, expression, description });
            }
        }
    }
    catch (const std::exception&) {
        _errorMessage = "Failed to load computed-column history";
    }
}

void ComputeColumnsView::saveHistory() const {
    nlohmann::json columns = nlohmann::json::array();
    for (const HistoryEntry& entry : _history) {
        columns.push_back({
            { "name", entry.name },
            { "expression", entry.expression },
            { "description", entry.description }
        });
    }

    std::ofstream file(_historyFile, std::ofstream::trunc);
    if (file.good()) {
        file << nlohmann::json({ { "version", 1 }, { "columns", columns } }).dump(2);
    }
}

void ComputeColumnsView::rememberQuery(const std::string& name,
                                       const std::string& expression,
                                       const std::string& description)
{
    auto entry = std::find_if(
        _history.begin(),
        _history.end(),
        [&name](const HistoryEntry& value) { return value.name == name; }
    );
    if (entry == _history.end()) {
        _history.push_back({ name, expression, description });
    }
    else {
        entry->expression = expression;
        entry->description = description;
    }
    saveHistory();
}

void ComputeColumnsView::renderHistory() {
    if (ImGui::Button("Query history")) {
        ImGui::OpenPopup("Query history");
    }

    if (!ImGui::BeginPopup("Query history")) {
        return;
    }

    if (_history.empty()) {
        ImGui::TextUnformatted("No computed-column queries saved yet");
    }
    else if (ImGui::BeginTable(
        "QueryHistoryTable",
        2,
        ImGuiTableFlags_SizingStretchProp
    )) {
        auto entryToRemove = _history.end();

        for (auto it = _history.begin(); it != _history.end(); ++it) {
            const HistoryEntry& entry = *it;
            ImGui::PushID(entry.name.c_str());
            ImGui::TableNextRow();

            ImGui::TableNextColumn();
            const bool selected = ImGui::Selectable(
                entry.name.c_str(),
                false,
                ImGuiSelectableFlags_SpanAllColumns
            );

            // Attach the context menu to the row-spanning selectable so it can
            // be opened from anywhere across the row
            if (ImGui::BeginPopupContextItem("QueryHistoryContextMenu")) {
                if (ImGui::MenuItem("Remove from history")) {
                    entryToRemove = it;
                }
                ImGui::EndPopup();
            }

            ImGui::TableNextColumn();
            ImGui::TextUnformatted(entry.expression.c_str());

            if (selected) {
                std::strncpy(_nameBuffer, entry.name.c_str(), sizeof(_nameBuffer) - 1);
                _nameBuffer[sizeof(_nameBuffer) - 1] = '\0';
                std::strncpy(
                    _descriptionBuffer,
                    entry.description.c_str(),
                    sizeof(_descriptionBuffer) - 1
                );
                _descriptionBuffer[sizeof(_descriptionBuffer) - 1] = '\0';
                std::strncpy(
                    _expressionBuffer,
                    entry.expression.c_str(),
                    sizeof(_expressionBuffer) - 1
                );
                _expressionBuffer[sizeof(_expressionBuffer) - 1] = '\0';
                _errorMessage.clear();
                ImGui::CloseCurrentPopup();
            }

            ImGui::PopID();
        }

        if (entryToRemove != _history.end()) {
            _history.erase(entryToRemove);
            saveHistory();
        }

        ImGui::EndTable();
    }

    ImGui::EndPopup();
}

bool ComputeColumnsView::appendToExpression(std::string_view text) {
    const size_t currentLength = std::strlen(_expressionBuffer);
    const bool needsSeparator = currentLength > 0 &&
        _expressionBuffer[currentLength - 1] != ' ' &&
        _expressionBuffer[currentLength - 1] != '\n' &&
        _expressionBuffer[currentLength - 1] != '\t' &&
        _expressionBuffer[currentLength - 1] != '\r';
    const size_t separatorLength = needsSeparator ? 1 : 0;
    constexpr size_t bufferSize = sizeof(_expressionBuffer);

    if (currentLength + separatorLength + text.size() >= bufferSize) {
        _errorMessage = "Value cannot be added: expression is too long";
        return false;
    }

    if (needsSeparator) {
        _expressionBuffer[currentLength] = ' ';
    }
    std::memcpy(
        _expressionBuffer + currentLength + separatorLength,
        text.data(),
        text.size()
    );
    _expressionBuffer[currentLength + separatorLength + text.size()] = '\0';
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
    if (_focusColumnBrowser) {
        ImGui::SetNextWindowFocus();
        _focusColumnBrowser = false;
    }

    if (!ImGui::Begin("Available numeric columns", &_showColumnBrowser)) {
        ImGui::End();
        return;
    }

    static ImGuiTextFilter columnFilter;
    const float availableWidth = ImGui::GetContentRegionAvail().x;
    columnFilter.Draw("Filter columns", availableWidth);

    if (ImGui::BeginChild("ColumnList", ImVec2(0.f, 0.f), true)) {
        const auto& computedColumns = _dataViewer.computedColumns();

        ImGui::TextUnformatted("Loaded columns");
        ImGui::Separator();

        for (const ColumnKey& column : _dataViewer.columns()) {
            if (!_dataViewer.isNumericColumn(column) ||
                computedColumns.contains(column))
            {
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
                appendToExpression(column);
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

        if (!computedColumns.empty()) {
            ImGui::Spacing();
            ImGui::TextUnformatted("Computed columns");
            ImGui::Separator();

            for (const auto& entry : computedColumns) {
                const std::string& column = entry.first;
                if (!columnFilter.PassFilter(column.c_str())) {
                    continue;
                }

                ImGui::PushID(column.c_str());
                if (ImGui::Selectable(column.c_str())) {
                    appendToExpression(column);
                }
                if (ImGui::IsItemHovered()) {
                    if (entry.second.description.empty()) {
                        ImGui::SetTooltip("Computed column; no description available");
                    }
                    else {
                        ImGui::BeginTooltip();
                        ImGui::TextWrapped("%s", entry.second.description.c_str());
                        ImGui::EndTooltip();
                    }
                }
                ImGui::PopID();
            }
        }

        ImGui::EndChild();
    }

    ImGui::End();
}

void ComputeColumnsView::renderConstantBrowser() {
    if (!_showConstantBrowser) {
        return;
    }

    ImGui::SetNextWindowSize(ImVec2(440.f, 420.f), ImGuiCond_FirstUseEver);
    ImGui::SetNextWindowPos(
        ImGui::GetMainViewport()->GetCenter(),
        ImGuiCond_Appearing,
        ImVec2(0.5f, 0.5f)
    );
    if (_focusConstantBrowser) {
        ImGui::SetNextWindowFocus();
        _focusConstantBrowser = false;
    }

    if (!ImGui::Begin("Available numeric constants", &_showConstantBrowser)) {
        ImGui::End();
        return;
    }

    static ImGuiTextFilter constantFilter;
    constantFilter.Draw("Filter constants", ImGui::GetContentRegionAvail().x);
    ImGui::TextDisabled("Values are expressed in meters");

    if (ImGui::BeginChild("ConstantList", ImVec2(0.f, 0.f), true) &&
        ImGui::BeginTable(
            "ConstantsTable",
            2,
            ImGuiTableFlags_SizingStretchProp
        ))
    {
        for (const NumericConstant& constant : NumericConstants) {
            if (!constantFilter.PassFilter(constant.name)) {
                continue;
            }

            const std::string value = formatNumericConstant(constant.value);
            ImGui::PushID(constant.name);
            ImGui::TableNextRow();
            ImGui::TableNextColumn();
            const bool selected = ImGui::Selectable(
                constant.name,
                false,
                ImGuiSelectableFlags_SpanAllColumns
            );
            const bool hovered = ImGui::IsItemHovered();
            ImGui::TableNextColumn();
            ImGui::TextDisabled("%s m", value.c_str());

            if (selected) {
                appendToExpression(value);
            }
            if (hovered) {
                ImGui::SetTooltip("%s meters", value.c_str());
            }
            ImGui::PopID();
        }
        ImGui::EndTable();
    }
    ImGui::EndChild();

    ImGui::End();
}

bool ComputeColumnsView::isNameTaken(const std::string& name) const {
    const std::vector<ColumnKey>& columns = _dataViewer.columns();
    return std::find(columns.begin(), columns.end(), name) != columns.end();
}

bool ComputeColumnsView::computeColumn(const std::string& name,
                                        const std::string& expressionText,
                                        const std::string& description)
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
        auto resolveVariable = [this, rowIndex, &foundUnknownVariable,
                                &unknownVariableName](const std::string& varName)
        {
            const auto& computedColumns = _dataViewer.computedColumns();
            if (auto it = computedColumns.find(varName); it != computedColumns.end()) {
                return it->second.values[rowIndex];
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
            return std::get<float>(_dataViewer.columnValue(varName, rowIndex));
        };

        result.push_back(expression.evaluate(resolveVariable));

        if (foundUnknownVariable) {
            _errorMessage = std::format("Unknown column: '{}'", unknownVariableName);
            return false;
        }
    }

    if (!_dataViewer.addComputedColumn(
        name,
        expressionText,
        description,
        std::move(result)
    )) {
        _errorMessage = std::format("Column name '{}' is already in use", name);
        return false;
    }
    rememberQuery(name, expressionText, description);
    _errorMessage.clear();
    return true;
}

void ComputeColumnsView::render(bool* open) {
    if (!ImGui::Begin("Compute data columns", open)) {
        _showColumnBrowser = false;
        _showConstantBrowser = false;
        ImGui::End();
        return;
    }

    ImGui::InputText("Column name", _nameBuffer, IM_ARRAYSIZE(_nameBuffer));
    ImGui::InputTextWithHint(
        "Description",
        "Enter a description... (optional)",
        _descriptionBuffer,
        IM_ARRAYSIZE(_descriptionBuffer)
    );

    ImGui::Text("Expression");
    ImGui::SameLine();
    if (ImGui::Button("Browse columns")) {
        _showColumnBrowser = true;
        _focusColumnBrowser = true;
    }
    ImGui::SameLine();
    if (ImGui::Button("Browse constants")) {
        _showConstantBrowser = true;
        _focusConstantBrowser = true;
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
        if (computeColumn(_nameBuffer, _expressionBuffer, _descriptionBuffer)) {
            _nameBuffer[0] = '\0';
            _descriptionBuffer[0] = '\0';
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

    renderHistory();

    ImGui::Separator();
    ImGui::Text("Computed columns");

    std::string columnToRemove;
    for (const auto& [name, column] : _dataViewer.computedColumns()) {
        ImGui::PushID(name.c_str());

        if (ImGui::Button("x")) {
            columnToRemove = name;
        }
        ImGui::SameLine();

        float minValue = std::numeric_limits<float>::max();
        float maxValue = std::numeric_limits<float>::lowest();
        for (float v : column.values) {
            if (!std::isnan(v)) {
                minValue = std::min(minValue, v);
                maxValue = std::max(maxValue, v);
            }
        }
        ImGui::Text("%s (min: %f, max: %f)", name.c_str(), minValue, maxValue);
        if (ImGui::IsItemHovered()) {
            ImGui::BeginTooltip();
            ImGui::TextUnformatted("Query:");
            ImGui::TextUnformatted(column.expression.c_str());
            if (!column.description.empty()) {
                ImGui::Spacing();
                ImGui::TextUnformatted("Description:");
                ImGui::TextWrapped("%s", column.description.c_str());
            }
            ImGui::EndTooltip();
        }

        ImGui::PopID();
    }
    if (!columnToRemove.empty()) {
        _dataViewer.removeComputedColumn(columnToRemove);
    }

    ImGui::End();
    renderColumnBrowser();
    renderConstantBrowser();
}

} // namespace openspace::exoplanets
