/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TEMPLATED_STATS_COLLECTOR___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TEMPLATED_STATS_COLLECTOR___H__

#include <ghoul/misc/boolean.h>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

namespace openspace::globebrowsing {

template <typename T>
class TemplatedStatsCollector {
public:
    BooleanType(Enabled);

    TemplatedStatsCollector(Enabled enabled, std::string delimiter);
    ~TemplatedStatsCollector() = default;

    void startNewRecord();

    T& operator[](const std::string& name);
    T previous(const std::string& name) const;

    void setEnabled(bool enabled);
    bool enabled() const;

    bool hasHeaders() const;
    bool hasRecordsToWrite() const;

    void reset();

    void writeHeader(std::ostream& os);
    void writeNextRecord(std::ostream& os);

private:
    template <typename U>
    using StatsRecord = std::unordered_map<std::string, U>;

    template <typename U>
    struct StatsCollection : public std::vector<StatsRecord<U>> {
        std::set<std::string> keys;
    };

    StatsCollection<T> _data;
    T _dummy; // used when disabled
    bool _enabled;

    size_t _writePos = 0;
    std::string _delimiter;
};

} // namespace openspace::globebrowsing

#include "templatedstatscollector.inl"

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TEMPLATED_STATS_COLLECTOR___H__
