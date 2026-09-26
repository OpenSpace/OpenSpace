/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#ifndef __GHOUL___CSVREADER___H__
#define __GHOUL___CSVREADER___H__

#include <filesystem>
#include <string>
#include <vector>

namespace ghoul {

/**
 * Loads a comma-separated value (CSV) file from the provided \p fileName and returns all
 * the specified columns. In the return value, each element of the outer vector is a data
 * point that contains a list of values as read from the file. If \p includeFirstLine is
 * `true`, the first line of the CSV file is included in the return value. Ignoring this
 * line might be necessary depending on the format of the CSV file as some might contain
 * a description of the column names at this position.
 *
 * \param fileName The location of the CSV file that is to be loaded
 * \param includeFirstLine If `true`, the first line of the CSV file is included;
 *        otherwise it is ignored
 * \return A list of set of data values extracted from the CSV file
 *
 * \pre fileName must not be empty
 */
std::vector<std::vector<std::string>> loadCSVFile(const std::filesystem::path& fileName,
    bool includeFirstLine = false);

/**
 * Loads a comma-separated value (CSV) file from the provided \p fileName and returns all
 * the specified columns. The data values of the first line in the CSV are used as names
 * for the columns, which are specified to the \p columns parameter. If the first line
 * does not contain unique names, the behavior of this function is undefined. In the
 * return value, each element of the outer vector is a data point that contains a list of
 * values as read from the file. If \p includeFirstLine is `true`, the first line of the
 * CSV file is included in the return value.
 *
 * \param fileName The location of the CSV file that is to be loaded
 * \param columns The name of the columns that should be extracted from the CSV file. The
 *        values of the first line are used as the names for the columns
 * \param includeFirstLine If `true`, the first line of the CSV file is included;
 *        otherwise it is ignored
 * \return A list of set of data values extracted from the CSV file

 * \throw ghoul::RuntimeError if one of the \p columns does not exist in the provided CSV
 * \pre fileName must not be empty
 * \pre columns must not be empty
 * \post `return.size() == columns.size()`
 */
std::vector<std::vector<std::string>> loadCSVFile(const std::filesystem::path& fileName,
    const std::vector<std::string>& columns, bool includeFirstLine = false);

/**
 * Loads a comma-separated value (CSV) file from the provided \p fileName and returns all
 * the specified columns. Only the specified \p columns (as a 0-based index) are extracted
 * from the CSV file. In the return value, each element of the outer vector is a data
 * point that contains a list of values as read from the file. If \p includeFirstLine is
 * `true`, the first line of the CSV file is included in the return value.
 *
 * \param fileName The location of the CSV file that is to be loaded
 * \param columns The indices of the columns that should be extracted from the CSV file
 * \param includeFirstLine If `true`, the first line of the CSV file is included;
 *        otherwise it is ignored
 * \return A list of set of data values extracted from the CSV file

 * \throw ghoul::RuntimeError if one of the indices is larger than the number of columns
 *        in the CSV file
 * \pre fileName must not be empty
 * \pre columns must not be empty
 * \post `return.size() == columns.size()`
 */
std::vector<std::vector<std::string>> loadCSVFile(const std::filesystem::path& fileName,
    const std::vector<int>& columns, bool includeFirstLine = false);

} // namespace ghoul

#endif // __GHOUL___CSVREADER___H__
