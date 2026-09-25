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
#include <modules/exoplanetsexperttool/columnfilter.h>
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
#include <numeric>
#include <unordered_set>

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
        if (!json.contains("columns")) {
            _errorMessage = "Computed-column history is missing saved columns";
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
                HistoryEntry historyEntry = { name, expression, description };
                historyEntry.mode = entry.value("mode", 0) == 1 ?
                    ComputeMode::WithinSystem : ComputeMode::PerPlanet;

                const int operation = entry.value("operation", 0);
                if (operation >= 0 && operation <= static_cast<int>(AggregateOperation::Range)) {
                    historyEntry.operation = static_cast<AggregateOperation>(operation);
                }
                historyEntry.aggregateColumn = entry.value("aggregateColumn", "");

                for (const nlohmann::json& filter : entry.value(
                    "countFilters",
                    nlohmann::json::array()
                )) {
                    const std::string column = filter.value("column", "");
                    const std::string query = filter.value("query", "");
                    if (!column.empty()) {
                        historyEntry.countFilters.push_back({ column, query });
                    }
                }
                _history.push_back(std::move(historyEntry));
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
            { "description", entry.description },
            { "mode", static_cast<int>(entry.mode) },
            { "operation", static_cast<int>(entry.operation) },
            { "aggregateColumn", entry.aggregateColumn },
            { "countFilters", nlohmann::json::array() }
        });

        for (const CountFilterRule& filter : entry.countFilters) {
            columns.back()["countFilters"].push_back({
                { "column", filter.column },
                { "query", filter.query }
            });
        }
    }

    std::ofstream file(_historyFile, std::ofstream::trunc);
    if (file.good()) {
        file << nlohmann::json({ { "columns", columns } }).dump(2);
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
        _history.push_back({
            name,
            expression,
            description,
            _computeMode,
            _aggregateOperation,
            _aggregateColumn,
            _countFilterRules
        });
    }
    else {
        entry->expression = expression;
        entry->description = description;
        entry->mode = _computeMode;
        entry->operation = _aggregateOperation;
        entry->aggregateColumn = _aggregateColumn;
        entry->countFilters = _countFilterRules;
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
                _computeMode = entry.mode;
                _aggregateOperation = entry.operation;
                _aggregateColumn = entry.aggregateColumn;
                _countFilterRules = entry.countFilters;
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

    if (_computeMode == ComputeMode::WithinSystem) {
        return computeSystemColumn(name, description);
    }
    return computePerPlanetColumn(name, expressionText, description);
}

bool ComputeColumnsView::computePerPlanetColumn(const std::string& name,
                                                const std::string& expressionText,
                                                const std::string& description)
{
    const Expression expression = Expression::parse(expressionText);
    if (!expression.isValid()) {
        _errorMessage = expression.errorMessage();
        return false;
    }

    const std::vector<ExoplanetItem>& data = _dataViewer.data();

    std::vector<float> result;
    result.reserve(data.size());

    for (size_t rowIndex = 0; rowIndex < data.size(); rowIndex++) {
        float value = std::numeric_limits<float>::quiet_NaN();
        if (!evaluateExpressionForRow(expression, rowIndex, value)) {
            return false;
        }
        result.push_back(value);
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

bool ComputeColumnsView::evaluateExpressionForRow(const Expression& expression,
                                                  size_t rowIndex, float& value)
{
    bool foundUnknownVariable = false;
    std::string unknownVariableName;
    const auto resolveVariable = [this, rowIndex, &foundUnknownVariable,
                                  &unknownVariableName](const std::string& varName)
    {
        const auto& computedColumns = _dataViewer.computedColumns();
        if (auto it = computedColumns.find(varName); it != computedColumns.end()) {
            return it->second.values[rowIndex];
        }
        if (!_dataViewer.hasColumn(varName)) {
            foundUnknownVariable = true;
            unknownVariableName = varName;
            return std::numeric_limits<float>::quiet_NaN();
        }
        if (!_dataViewer.isNumericColumn(varName)) {
            return std::numeric_limits<float>::quiet_NaN();
        }
        return std::get<float>(_dataViewer.columnValue(varName, rowIndex));
    };

    value = expression.evaluate(resolveVariable);
    if (!foundUnknownVariable) {
        return true;
    }

    _errorMessage = std::format("Unknown column: '{}'", unknownVariableName);
    return false;
}

const std::array<const char*, 9>& ComputeColumnsView::aggregateOperationNames() {
    static constexpr std::array<const char*, 9> Names = {
        "Count", "Sum", "Mean", "Minimum", "Maximum", "Standard deviation",
        "Variance", "Median", "Range"
    };
    return Names;
}

float ComputeColumnsView::aggregateValues(std::vector<float>& values) const {
    if (values.empty()) {
        return std::numeric_limits<float>::quiet_NaN();
    }

    const float sum = std::accumulate(values.begin(), values.end(), 0.f);
    switch (_aggregateOperation) {
        case AggregateOperation::Sum:
            return sum;
        case AggregateOperation::Mean:
            return sum / values.size();
        case AggregateOperation::Minimum:
            return *std::min_element(values.begin(), values.end());
        case AggregateOperation::Maximum:
            return *std::max_element(values.begin(), values.end());
        case AggregateOperation::StandardDeviation:
        case AggregateOperation::Variance: {
            const float mean = sum / values.size();
            float variance = 0.f;
            for (float value : values) {
                const float delta = value - mean;
                variance += delta * delta;
            }
            variance /= values.size();
            return _aggregateOperation == AggregateOperation::Variance ?
                variance : std::sqrt(variance);
        }
        case AggregateOperation::Median: {
            std::sort(values.begin(), values.end());
            const size_t mid = values.size() / 2;
            if (values.size() % 2 == 0) {
                return (values[mid] + values[mid - 1]) / 2.f;
            }
            return values[mid];
        }
        case AggregateOperation::Range: {
            const float minimum = *std::min_element(values.begin(), values.end());
            const float maximum = *std::max_element(values.begin(), values.end());
            return maximum - minimum;
        }
        case AggregateOperation::Count:
            return std::numeric_limits<float>::quiet_NaN();
    }

    return std::numeric_limits<float>::quiet_NaN();
}

std::string ComputeColumnsView::systemExpressionDescription() const {
    const char* operation = aggregateOperationNames()[
        static_cast<size_t>(_aggregateOperation)
    ];
    if (_aggregateOperation != AggregateOperation::Count) {
        return std::format("{}({})", operation, _aggregateColumn);
    }

    std::string result = std::format("{}(", operation);
    for (size_t i = 0; i < _countFilterRules.size(); i++) {
        if (i > 0) {
            result += "; ";
        }
        result += std::format("{}: {}", _countFilterRules[i].column, _countFilterRules[i].query);
    }
    return result + ')';
}

bool ComputeColumnsView::computeSystemColumn(const std::string& name,
                                              const std::string& description)
{
    if (_dataViewer.hostPlanetGroups().empty()) {
        _errorMessage = "System aggregates require a host-name column";
        return false;
    }

    struct CompiledCountFilter {
        const CountFilterRule& rule;
        ColumnFilter filter;
    };

    std::vector<CompiledCountFilter> countFilters;
    if (_aggregateOperation == AggregateOperation::Count) {
        if (_countFilterRules.empty()) {
            _errorMessage = "Count requires at least one filter";
            return false;
        }

        std::unordered_set<ColumnKey> filteredColumns;
        for (const CountFilterRule& rule : _countFilterRules) {
            if (rule.column.empty() || rule.query.empty()) {
                _errorMessage = "Every count filter needs a column and query";
                return false;
            }
            if (!_dataViewer.hasColumn(rule.column) ||
                !filteredColumns.insert(rule.column).second)
            {
                _errorMessage = std::format("Invalid or duplicate filter column: '{}'", rule.column);
                return false;
            }
            const ColumnFilter::Type type = _dataViewer.isNumericColumn(rule.column) ?
                ColumnFilter::Type::Numeric : ColumnFilter::Type::Text;

            countFilters.push_back({ rule, ColumnFilter(rule.query, type) });

            if (!countFilters.back().filter.isValid()) {
                _errorMessage = std::format("Invalid filter for column '{}'", rule.column);
                return false;
            }
        }
    }

    if (_aggregateOperation != AggregateOperation::Count &&
        (!_dataViewer.hasColumn(_aggregateColumn) ||
        !_dataViewer.isNumericColumn(_aggregateColumn)))
    {
        _errorMessage = "Select a numeric column for the aggregate";
        return false;
    }

    std::vector<float> result(
        _dataViewer.data().size(),
        std::numeric_limits<float>::quiet_NaN()
    );

    for (const auto& group : _dataViewer.hostPlanetGroups()) {
        const std::vector<size_t>& rows = group.second;
        std::vector<float> values;
        size_t matchingPlanets = 0;
        if (_aggregateOperation != AggregateOperation::Count) {
            values.reserve(rows.size());
        }
        for (size_t rowIndex : rows) {
            if (_aggregateOperation == AggregateOperation::Count) {
                bool passesAllFilters = true;
                for (const CompiledCountFilter& countFilter : countFilters) {
                    bool passesFilter = countFilter.filter.passFilter(
                        _dataViewer.columnValue(countFilter.rule.column, rowIndex)
                    );
                    if (!passesFilter) {
                        passesAllFilters = false;
                        break;
                    }
                }
                if (passesAllFilters) {
                    matchingPlanets++;
                }
            }
            else {
                const float value = std::get<float>(
                    _dataViewer.columnValue(_aggregateColumn, rowIndex)
                );
                if (!std::isnan(value)) {
                    values.push_back(value);
                }
            }
        }

        const float aggregate = _aggregateOperation == AggregateOperation::Count ?
            static_cast<float>(matchingPlanets) : aggregateValues(values);

        for (size_t rowIndex : rows) {
            result[rowIndex] = aggregate;
        }
    }

    const std::string expressionText = systemExpressionDescription();
    if (!_dataViewer.addComputedColumn(name, expressionText, description, std::move(result))) {
        _errorMessage = std::format("Column name '{}' is already in use", name);
        return false;
    }
    rememberQuery(name, expressionText, description);
    _errorMessage.clear();
    return true;
}

void ComputeColumnsView::renderSystemAggregateControls() {
    int operation = static_cast<int>(_aggregateOperation);
    ImGui::SetNextItemWidth(180.f);
    if (ImGui::Combo(
        "Aggregator",
        &operation,
        aggregateOperationNames().data(),
        static_cast<int>(aggregateOperationNames().size())
    )) {
        _aggregateOperation = static_cast<AggregateOperation>(operation);
        _errorMessage.clear();
    }

    if (_aggregateOperation != AggregateOperation::Count) {
        const char* preview = _aggregateColumn.empty() ? "Select numeric column" :
            _dataViewer.columnName(_aggregateColumn);
        ImGui::SetNextItemWidth(260.f);
        if (ImGui::BeginCombo("Numeric column", preview)) {
            static ImGuiTextFilter columnFilter;
            if (ImGui::IsWindowAppearing()) {
                ImGui::SetKeyboardFocusHere();
                columnFilter.Clear();
            }
            columnFilter.Draw("##Filter");
            for (const ColumnKey& column : _dataViewer.columns()) {
                if (!_dataViewer.isNumericColumn(column) ||
                    !columnFilter.PassFilter(_dataViewer.columnName(column)))
                {
                    continue;
                }
                if (ImGui::Selectable(
                    _dataViewer.columnName(column),
                    _aggregateColumn == column
                )) {
                    _aggregateColumn = column;
                    _errorMessage.clear();
                }
                if (_dataViewer.hasColumnDescription(column)) {
                    ImGui::SetItemTooltip("%s", _dataViewer.columnDescription(column));
                }
            }
            ImGui::EndCombo();
        }
        view::helper::renderHelpMarker(
            "The selected statistic is calculated across the numeric values in each host "
            "system. Missing values are ignored."
        );
        return;
    }

    ImGui::TextUnformatted("Count planets that match all filters");
    for (size_t i = 0; i < _countFilterRules.size(); i++) {
        CountFilterRule& rule = _countFilterRules[i];

        ImGui::PushID(static_cast<int>(i));

        const char* preview = rule.column.empty() ? "Select column" :
            _dataViewer.columnName(rule.column);

        ImGui::SetNextItemWidth(180.f);
        if (ImGui::BeginCombo("##Column", preview)) {
            for (const ColumnKey& column : _dataViewer.columns()) {
                if (ImGui::Selectable(
                    _dataViewer.columnName(column),
                    rule.column == column
                )) {
                    rule.column = column;
                    _errorMessage.clear();
                }
            }
            ImGui::EndCombo();
        }
        ImGui::SameLine();
        std::array<char, 256> query = {};
        std::strncpy(query.data(), rule.query.c_str(), query.size() - 1);
        ImGui::SetNextItemWidth(-80.f);

        bool queryChanged = ImGui::InputTextWithHint(
            "##Query",
            "Filter query",
            query.data(),
            query.size()
        );

        if (queryChanged) {
            rule.query = query.data();
            _errorMessage.clear();
        }

        ImGui::SameLine();

        if (ImGui::SmallButton("Remove")) {
            _countFilterRules.erase(_countFilterRules.begin() + i);
            ImGui::PopID();
            break;
        }

        ImGui::PopID();
    }
    if (ImGui::Button("+ Add filter")) {
        _countFilterRules.push_back({});
    }
    ImGui::SameLine();
    view::helper::renderHelpMarker(
        "Each filter uses the same syntax as FilteringView. Numeric examples: '> 1', "
        "'null'. Text examples: 'Transit|Radial Velocity', '-candidate'."
    );
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

    constexpr std::array<const char*, 2> ModeNames = { "Per planet", "Within system" };
    int mode = static_cast<int>(_computeMode);
    ImGui::SetNextItemWidth(140.f);
    if (ImGui::Combo(
        "Calculation scope",
        &mode,
        ModeNames.data(),
        static_cast<int>(ModeNames.size())
    )) {
        _computeMode = static_cast<ComputeMode>(mode);
        _errorMessage.clear();
    }

    if (_computeMode == ComputeMode::PerPlanet) {
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
    }
    else {
        renderSystemAggregateControls();
    }

    if (ImGui::Button("Compute")) {
        if (computeColumn(_nameBuffer, _expressionBuffer, _descriptionBuffer)) {
            _nameBuffer[0] = '\0';
            _descriptionBuffer[0] = '\0';
            _expressionBuffer[0] = '\0';
            _aggregateColumn.clear();
            _countFilterRules.clear();
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
