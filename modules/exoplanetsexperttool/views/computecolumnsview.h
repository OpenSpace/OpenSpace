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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___COMPUTECOLUMNSVIEW___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___COMPUTECOLUMNSVIEW___H__

#include <modules/exoplanetsexperttool/datastructures.h>
#include <filesystem>
#include <string>
#include <string_view>
#include <vector>

namespace openspace::exoplanets {

class DataViewer;
struct DataSettings;
class Expression;

class ComputeColumnsView {
public:
    ComputeColumnsView(DataViewer& dataViewer,
        const DataSettings& dataSettings);

    void render(bool* open);

private:
    enum class ComputeMode {
        PerPlanet,
        WithinSystem
    };

    enum class AggregateOperation {
        Count,
        Sum,
        Mean,
        Minimum,
        Maximum,
        StandardDeviation,
        Variance,
        Median,
        Range
    };

    struct CountFilterRule {
        ColumnKey column;
        std::string query;
    };

    bool isNameTaken(const std::string& name) const;
    bool appendToExpression(std::string_view text);
    void renderColumnBrowser();
    void renderConstantBrowser();
    void renderSystemAggregateControls();
    void renderHistory();
    void loadHistory();
    void saveHistory() const;
    void rememberQuery(const std::string& name, const std::string& expression,
        const std::string& description);

    // Parses and evaluates `expressionText` for every data row, and on success registers
    // the result in the DataViewer under `name`. Returns whether it succeeded; on failure,
    // `_errorMessage` is set instead.
    bool computeColumn(const std::string& name, const std::string& expressionText,
        const std::string& description);
    bool computePerPlanetColumn(const std::string& name, const std::string& expressionText,
        const std::string& description);
    bool computeSystemColumn(const std::string& name, const std::string& description);
    bool evaluateExpressionForRow(const Expression& expression, size_t rowIndex,
        float& value);
    float aggregateValues(std::vector<float>& values) const;
    static const std::array<const char*, 9>& aggregateOperationNames();
    std::string systemExpressionDescription() const;

    DataViewer& _dataViewer;

    char _nameBuffer[128] = "";
    char _descriptionBuffer[512] = "";
    char _expressionBuffer[1024] = "";
    bool _showColumnBrowser = false;
    bool _focusColumnBrowser = false;
    bool _showConstantBrowser = false;
    bool _focusConstantBrowser = false;
    ComputeMode _computeMode = ComputeMode::PerPlanet;
    AggregateOperation _aggregateOperation = AggregateOperation::Count;
    ColumnKey _aggregateColumn;
    std::vector<CountFilterRule> _countFilterRules;

    struct HistoryEntry {
        std::string name;
        std::string expression;
        std::string description;
        ComputeMode mode = ComputeMode::PerPlanet;
        AggregateOperation operation = AggregateOperation::Count;
        ColumnKey aggregateColumn;
        std::vector<CountFilterRule> countFilters;
    };
    std::vector<HistoryEntry> _history;
    std::filesystem::path _historyFile;

    std::string _errorMessage;
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___COMPUTECOLUMNSVIEW___H__
