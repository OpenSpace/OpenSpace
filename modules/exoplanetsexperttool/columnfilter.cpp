/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

namespace {
    constexpr const char _loggerCat[] = "ColumnFilter";

    constexpr const char Separator = ',';
    constexpr const char GreaterOperator = '>';
    constexpr const char LessOperator = '<';
    constexpr const char EqualsOperator = '=';
    constexpr const char NotOperator = '!';
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
} // namespace

namespace openspace::exoplanets {

const char* ColumnFilter::TextFilterDescriptionShort = "incl, -excl";

const char* ColumnFilter::TextFilterDescription =
    "Text filter. Start with '-' for exclusive check, otherwise an "
    "inclusive check against the string is performed. \n \n"
    "Combine multiple conditions with comma";

const char* ColumnFilter::NumericFilterDescriptionShort =
    ">, >=, <, <=, =, null, !null";

const char* ColumnFilter::NumericFilterDescription =
    "Numeric filter. Supported operators are: "
    "\t >, >=, <, <=, =, null, !null. \n \nNo input => check is not null. \n \n"
    "Combine multiple conditions with comma. Ex: '> 30, !null'";

ColumnFilter::ColumnFilter(std::string query, Type type)
    : _query(query), _type(type)
{
    _subqueries = ghoul::tokenizeString(_query, Separator);

    for (std::string& s : _subqueries) {
        ghoul::trimWhitespace(s);
    }

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

std::string ColumnFilter::query() const {
    return _query;
}

bool ColumnFilter::isValid() const {
    return _valid;
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

    bool pass = true;

    // Special case when we only have one subquery and an empty string
    // => check against non existing value
    if (_subqueries.size() == 1 && _subqueries.front().empty()) {
        return !std::isnan(value);
    }

    // Test against each subquery
    for (std::string q : _subqueries) {
        bool passSubquery = false;

        removeWhitespaces(q);

        if (q.empty()) {
            continue;
        }

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

        pass &= passSubquery; // true only if both are true
    }

    return pass;
}

bool ColumnFilter::passFilter(const std::string& value) const {
    if (_type != Type::Text) {
        throw ghoul::RuntimeError("Can only pass text to text based filters");
    }

    bool pass = true;

    std::string lowercaseValue = value;
    std::transform(lowercaseValue.begin(), lowercaseValue.end(), lowercaseValue.begin(),
        [](unsigned char c) { return std::tolower(c); });

    ghoul::trimWhitespace(lowercaseValue);

    // Special case when we only have one subquery and an empty string
    // => check against non existing value
    if (_subqueries.size() == 1 && _subqueries.front().empty()) {
        return !value.empty();
    }

    // Test against each subquery
    for (const std::string& q : _subqueries) {
        bool passSubquery = false;

        if (q.empty()) {
            continue;
        }

        std::string query = q;
        std::transform(query.begin(), query.end(), query.begin(),
            [](unsigned char c) { return std::tolower(c); });

        if (query[0] == '-') {
            // Subtract
            std::string str = query.substr(1);
            ghoul::trimWhitespace(str);
            passSubquery = (lowercaseValue.find(str) == std::string::npos);
        }
        else {
            // Full search
            passSubquery = (lowercaseValue.find(query) != std::string::npos);
        }

        pass &= passSubquery; // true only if both are true
    }

    return pass;
}

} // namespace openspace::exoplanets
