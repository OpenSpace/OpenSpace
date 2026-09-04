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

#include <modules/exoplanetsexperttool/columnfilter.h>

#include <modules/exoplanetsexperttool/datahelper.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/stringhelper.h>
#include <string_view>

namespace {
    constexpr std::string_view _loggerCat = "ColumnFilter";

    constexpr char Separator = ',';
    constexpr char OrOperator = '|';
    constexpr char GreaterOperator = '>';
    constexpr char LessOperator = '<';
    constexpr char EqualsOperator = '=';
    constexpr char NotOperator = '!';
    constexpr const char* NullOperator = "null";

    bool contains(const char* op, const std::string& str, size_t& pos) {
        pos = str.find(op);
        return (pos != std::string::npos);
    }

    bool contains(const char op, const std::string& str, size_t& pos) {
        pos = str.find(op);
        return (pos != std::string::npos);
    }

    void removeWhitespaces(std::string& str) {
        str.erase(remove_if(str.begin(), str.end(), isspace), str.end());
    }

    // Splits `str` on `delimiter`, but ignores delimiters that occur inside a
    // "quoted section" or inside parentheses, so `(A, B) | C` splits into
    // `(A, B)` and `C` when delimiter is '|', and `"a, b", c` splits into
    // `"a, b"` and `c` when delimiter is ','.
    std::vector<std::string> splitTopLevel(const std::string& str, char delimiter) {
        std::vector<std::string> tokens;
        std::string current;
        bool inQuotes = false;
        int parenDepth = 0;

        for (char c : str) {
            if (c == '"') {
                inQuotes = !inQuotes;
                current += c;
            }
            else if (!inQuotes && c == '(') {
                parenDepth++;
                current += c;
            }
            else if (!inQuotes && c == ')') {
                parenDepth = std::max(0, parenDepth - 1);
                current += c;
            }
            else if (!inQuotes && parenDepth == 0 && c == delimiter) {
                tokens.push_back(current);
                current.clear();
            }
            else {
                current += c;
            }
        }
        tokens.push_back(current);
        return tokens;
    }

    std::string stripOuterParens(std::string group) {
        ghoul::trimWhitespace(group);
        if (group.size() >= 2 && group.front() == '(' && group.back() == ')') {
            group = group.substr(1, group.size() - 2);
            ghoul::trimWhitespace(group);
        }
        return group;
    }
} // namespace

namespace openspace::exoplanets {

const char* ColumnFilter::TextFilterDescriptionShort = "incl, -excl, \"exact\"";

const char* ColumnFilter::TextFilterDescription =
    "Text filter. Start with '-' for exclusive check, otherwise an "
    "inclusive check against the string is performed. \n \n"
    "Combine multiple conditions with comma (AND). Combine groups of "
    "conditions with '|' for OR, e.g. '(A, B) | C'. \n \n"
    "Wrap a term in quotes to match it exactly, including leading/trailing "
    "whitespace, e.g. \"k2-18 \"";

const char* ColumnFilter::NumericFilterDescriptionShort =
    ">, >=, <, <=, =, null, !null";

const char* ColumnFilter::NumericFilterDescription =
    "Numeric filter. Supported operators are: "
    "\t >, >=, <, <=, =, null, !null. \n \nNo input => check is not null. \n \n"
    "Combine multiple conditions with comma (AND). Combine groups of "
    "conditions with '|' for OR, e.g. '(> 30, < 100) | = 0'";

ColumnFilter::ColumnFilter(std::string query, Type type)
    : _type(type), _query(query)
{
    _subqueries = parseQuery(_query, _type);

    // Validate numeric filter query (text filters cannot be invalid)
    if (_type == ColumnFilter::Type::Numeric) {
        try {
            passFilter(0.f); // dummy value
        }
        catch (const ghoul::RuntimeError& e) {
            _valid = false;
            LWARNING(std::format("Failed creating numeric filter. {}", e.message));
        }
    }
}

std::vector<ColumnFilter::AndGroup> ColumnFilter::parseQuery(
    const std::string& query, Type type)
{
    std::vector<AndGroup> result;

    std::vector<std::string> orParts = splitTopLevel(query, OrOperator);
    for (std::string& part : orParts) {
        part = stripOuterParens(part);

        std::vector<std::string> rawTerms = splitTopLevel(part, Separator);
        AndGroup group;
        group.reserve(rawTerms.size());
        for (std::string& raw : rawTerms) {
            group.push_back(parseTerm(std::move(raw), type));
        }
        result.push_back(std::move(group));
    }

    return result;
}

ColumnFilter::SubQuery ColumnFilter::parseTerm(std::string raw, Type type) {
    ghoul::trimWhitespace(raw);

    SubQuery sq;

    // Only text filters support exclusion prefixes and quoted exact matches
    if (type == Type::Text) {
        if (!raw.empty() && raw.front() == '-') {
            sq.exclude = true;
            raw = raw.substr(1);
            ghoul::trimWhitespace(raw);
        }

        if (raw.size() >= 2 && raw.front() == '"' && raw.back() == '"') {
            sq.exact = true;
            // Keep interior whitespace exactly as written, including leading
            // and trailing spaces, e.g. "k2-18 " -> `k2-18 ` (with trailing space)
            raw = raw.substr(1, raw.size() - 2);
        }
    }

    sq.text = std::move(raw);
    return sq;
}

std::string ColumnFilter::query() const {
    return _query;
}

bool ColumnFilter::isValid() const {
    return _valid;
}

bool ColumnFilter::isNumeric() const {
    return _type == Type::Numeric;
}

bool ColumnFilter::passFilter(std::variant<const char*, float> value) const {
    if (std::holds_alternative<float>(value) && _type == Type::Numeric) {
        float val = std::get<float>(value);
        return passFilter(val);
    }
    else if (std::holds_alternative<const char*>(value) && _type == Type::Text) {
        const char* val = std::get<const char*>(value);
        return passFilter(std::string(val));
    }
    else {
        throw ghoul::RuntimeError("Mismatching value and filter type!");
    }
}

bool ColumnFilter::passFilter(float value) const {
    if (_type != Type::Numeric) {
        throw ghoul::RuntimeError("Can only pass numbers to numeric filters");
    }

    if (!isValid()) {
        return true; // let everything through
    }

    // Special case: a single, single-term, empty group => check against
    // non-existing value
    if (_subqueries.size() == 1 && _subqueries.front().size() == 1 &&
        _subqueries.front().front().text.empty())
    {
        return !std::isnan(value);
    }

    bool anyGroupPassed = false;

    // OR across groups, AND within each group
    for (const AndGroup& group : _subqueries) {
        bool passGroup = true;

        for (const SubQuery& sub : group) {
            std::string q = sub.text;
            removeWhitespaces(q);

            if (q.empty()) {
                continue;
            }

            bool passSubquery = false;
            size_t pos; // contains position of first character on match

            // Is null
            if (contains(NullOperator, q, pos)) {
                bool isNot = contains(NotOperator, q, pos);
                passSubquery = isNot ? !std::isnan(value) : std::isnan(value);
            }
            // Greater than
            else if (contains(GreaterOperator, q, pos)) {
                bool equals = q[pos + 1] == '=';
                std::string right = equals ? q.substr(pos + 2) : q.substr(pos + 1);

                if (right.empty()) {
                    throw ghoul::RuntimeError("Right side of query is empty");
                }

                const float rVal = data::parseFloatData(right);
                if (std::isnan(rVal)) {
                    throw ghoul::RuntimeError(
                        "Right side of query '" + q + "' is not a valid number"
                    );
                }
                else {
                    passSubquery = equals ? (value >= rVal) : (value > rVal);
                }
            }
            // Less than
            else if (contains(LessOperator, q, pos)) {
                bool equals = q[pos + 1] == '=';
                std::string right = equals ? q.substr(pos + 2) : q.substr(pos + 1);

                if (right.empty()) {
                    throw ghoul::RuntimeError("Right side of query is empty");
                }

                const float rVal = data::parseFloatData(right);
                if (std::isnan(rVal)) {
                    throw ghoul::RuntimeError(
                        "Right side of query '" + q + "' is not a valid number"
                    );
                }
                else {
                    passSubquery = equals ? (value <= rVal) : (value < rVal);
                }
            }
            // Equals
            else if (contains(EqualsOperator, q, pos)) {
                std::string right = q.substr(pos + 1);

                if (right.empty()) {
                    throw ghoul::RuntimeError("Right side of query is empty");
                }

                const float rVal = data::parseFloatData(right);
                if (std::isnan(rVal)) {
                    throw ghoul::RuntimeError(
                        "Right side of query '" + q + "' is not a valid number"
                    );
                }
                else {
                    passSubquery = value == rVal;
                }
            }
            else {
                throw ghoul::RuntimeError(std::format("Invalid filter query '{}'", q));
            }

            passGroup = passGroup && passSubquery;
        }

        anyGroupPassed = anyGroupPassed || passGroup;
    }

    return anyGroupPassed;
}

bool ColumnFilter::passFilter(const std::string& value) const {
    if (_type != Type::Text) {
        throw ghoul::RuntimeError("Can only pass text to text based filters");
    }

    std::string lowercaseValue = value;
    std::transform(lowercaseValue.begin(), lowercaseValue.end(), lowercaseValue.begin(),
        [](unsigned char c) { return std::tolower(c); });

    ghoul::trimWhitespace(lowercaseValue);

    // Special case: a single, single-term, empty, non-exact group => check
    // against non-existing value
    if (_subqueries.size() == 1 && _subqueries.front().size() == 1 &&
        _subqueries.front().front().text.empty() && !_subqueries.front().front().exact)
    {
        return !value.empty();
    }

    bool anyGroupPassed = false;

    // OR across groups, AND within each group
    for (const AndGroup& group : _subqueries) {
        bool passGroup = true;

        for (const SubQuery& sub : group) {
            if (sub.text.empty() && !sub.exact) {
                continue;
            }

            std::string term = sub.text;
            std::transform(term.begin(), term.end(), term.begin(),
                [](unsigned char c) { return std::tolower(c); });

            // Quoted terms keep their leading/trailing whitespace exactly as
            // written (e.g. "k2-18 " won't also match "K2-180"). Unquoted
            // terms behave as before
            bool found = (lowercaseValue.find(term) != std::string::npos);
            bool passSubquery = sub.exclude ? !found : found;

            passGroup = passGroup && passSubquery;
        }

        anyGroupPassed = anyGroupPassed || passGroup;
    }

    return anyGroupPassed;
}

} // namespace openspace::exoplanets
