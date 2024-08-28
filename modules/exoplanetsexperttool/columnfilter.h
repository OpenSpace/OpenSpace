/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___COLUMNFILTER___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___COLUMNFILTER___H__

#include <string>
#include <vector>

namespace openspace::exoplanets {

class ColumnFilter {
public:
    enum class Type {
        Numeric,
        Text
    };

    static const char* TextFilterDescriptionShort;
    static const char* TextFilterDescription;
    static const char* NumericFilterDescriptionShort;
    static const char* NumericFilterDescription;

    ColumnFilter(std::string query, Type type);

    std::string query() const;
    bool isValid() const;
    bool isNumeric() const;

    bool passFilter(std::variant<const char*, float> value) const;
    bool passFilter(float value) const;
    bool passFilter(const std::string& value) const;

private:
    Type _type;
    std::string _query;
    std::vector<std::string> _subqueries;

    bool _valid = true;
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___COLUMNFILTER___H__
