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

#include <ghoul/misc/csvreader.h>

#include <ghoul/format.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/stringhelper.h>
#include <algorithm>
#include <fstream>
#include <iterator>
#include <utility>

namespace {
    using namespace ghoul;

    std::string readFirstValidLine(std::ifstream& file) {
        std::string line;
        while (getline(file, line)) {
            trimWhitespace(line);
            if (!line.empty() && !line.starts_with("#")) {
                break;
            }
        }
        return line;
    }

    std::vector<std::vector<std::string>> internalLoadCSV(std::ifstream& file,
                                                          bool includeFirstLine,
                                                          const std::vector<int>& indices)
    {
        ghoul_assert(file.good(), "File handle should be good");

        std::vector<std::vector<std::string>> result;

        bool hasFoundFirstValidLine = false;
        std::string line;
        while (getline(file, line)) {
            trimWhitespace(line);

            // Skip comments and empty lines
            if (line.empty() || line.starts_with("#")) {
                continue;
            }

            if (!hasFoundFirstValidLine) {
                hasFoundFirstValidLine = true;
                if (!includeFirstLine) {
                    // Skip the first line containing the column names
                    continue;
                }
            }

            std::vector<std::string> lineValues = tokenizeString(line, ',');

            // The user might have needed to use a , inside a value and needed to escape
            // it by surrounding the value with "..." The tokenizeString function will rip
            // those apart, and we need to reassemble it here

            for (size_t i = 0; i < lineValues.size(); i++) {
                trimWhitespace(lineValues[i]);

                if (lineValues[i].empty()) {
                    continue;
                }

                // If a value starts with " we need to continue until we find the first
                // one that ends with " and join them
                if (lineValues[i].front() != '"') {
                    continue;
                }

                // First check if the " is terminated in the same value
                if (const size_t p = lineValues[i].find('"', 1);
                    p != std::string::npos)
                {
                    // It is terminated here, so we can advance the i counter as there is
                    // nothing to be done. This was just a ".." pair without a ,
                    continue;
                }

                std::string totalValue = lineValues[i];
                for (size_t j = i + 1; j < lineValues.size(); j++) {
                    const size_t p = lineValues[j].find('"');
                    if (p == std::string::npos) {
                        // No " found, so accumualate the text and continue next j
                        totalValue += lineValues[j];
                        lineValues.erase(lineValues.begin() + j);
                        j--;
                        continue;
                    }

                    // There is a "
                    if (p != lineValues[j].size() - 1) {
                        // it's not at the last position, so something was malformed
                        throw RuntimeError(
                            "Malformed CSV file. Mismatching \" to escape , in value"
                        );
                    }

                    // And it is at the last position so we have our winner
                    lineValues[i] = totalValue + ", " + lineValues[j];
                    // Remove the beginning and end "
                    ghoul_assert(lineValues[i].front() == '"', "Unexpected line");
                    ghoul_assert(lineValues[i].back() == '"', "Unexpected line");
                    lineValues[i].erase(lineValues[i].begin());
                    lineValues[i].pop_back();
                    lineValues.erase(lineValues.begin() + j);
                    break;
                }
            }


            // If indices have been specified, we use those to reorganize and filter the
            // line values
            if (!indices.empty()) {
                std::vector<std::string> copy = lineValues;
                lineValues.clear();
                for (const int idx : indices) {
                    lineValues.push_back(copy[idx]);
                }
            }
            result.push_back(std::move(lineValues));
        }

        return result;
    }
} // namespace

namespace ghoul {

std::vector<std::vector<std::string>> loadCSVFile(const std::filesystem::path& fileName,
                                                  bool includeFirstLine)
{
    ghoul_assert(!fileName.empty(), "fileName must not be empty");

    std::ifstream file;
    file.exceptions(std::ifstream::badbit);
    file.open(fileName);

    return internalLoadCSV(file, includeFirstLine, std::vector<int>());
}

std::vector<std::vector<std::string>> loadCSVFile(const std::filesystem::path& fileName,
                                                  const std::vector<std::string>& columns,
                                                  bool includeFirstLine)
{
    ghoul_assert(!fileName.empty(), "fileName must not be empty");
    ghoul_assert(!columns.empty(), "columns must not be empty");

    std::ifstream file;
    file.exceptions(std::ifstream::badbit);
    file.open(fileName);

    // Get the file line that contains the column names (the first one)
    const std::string line = readFirstValidLine(file);

    const std::vector<std::string> elements = tokenizeString(line, ',');
    if (elements.empty()) {
        throw RuntimeError(std::format(
            "CSV file '{}' did not contain any lines", fileName
        ));
    }

    std::vector<int> indices(columns.size());

    std::transform(
        columns.cbegin(),
        columns.cend(),
        indices.begin(),
        [elements, &fileName](const std::string& column) {
            const auto it = std::find(elements.cbegin(), elements.cend(), column);
            if (it == elements.cend()) {
                throw RuntimeError(std::format(
                    "CSV file '{}' did not contain the requested key {}", fileName, column
                ));
            }

            return static_cast<int>(std::distance(elements.cbegin(), it));
        }
    );

    // Reset the file stream. The internalLoadCSV function will hop over invalid lines
    // again. If we conditionally seek, there was a case where we'd skip over the first
    // valid line while trying to find the first valid line
    file.seekg(0);

    return internalLoadCSV(file, includeFirstLine, indices);
}

std::vector<std::vector<std::string>> loadCSVFile(const std::filesystem::path& fileName,
                                                  const std::vector<int>& columns,
                                                  bool includeFirstLine)
{
    ghoul_assert(!fileName.empty(), "fileName must not be empty");
    ghoul_assert(!columns.empty(), "columns must not be empty");

    std::ifstream file;
    file.exceptions(std::ifstream::badbit);
    file.open(fileName);

    return internalLoadCSV(file, includeFirstLine, columns);
}

} // namespace ghoul
