/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___STATS_TRACKER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___STATS_TRACKER___H__

#include <ghoul/misc/boolean.h>

#include <set>
#include <string>
#include <unordered_map>
#include <vector>

namespace openspace {
namespace globebrowsing {    

template <typename T>
using StatsRecord = std::unordered_map<std::string, T>;

template <typename T>
struct StatsCollection : public std::vector<StatsRecord<T>> {
    std::set<std::string> keys;
};

template <typename T>
class TemplatedStatsCollector {
public:
    TemplatedStatsCollector(bool& enabled, const std::string& delimiter);

    ~TemplatedStatsCollector() = default;

    void startNewRecord();

    T& operator[](const std::string& name);

    T previous(const std::string& name);

    bool hasHeaders();

    bool hasRecordsToWrite();

    void reset();

    void writeHeader(std::ostream& os);

    void writeNextRecord(std::ostream& os);

private:
    StatsCollection<T> _data;
    T _dummy; // used when disabled
    bool& _enabled;

    size_t _writePos;
    std::string _delimiter;
};

class StatsCollector {
public:
    StatsCollector() = delete;

    using Enabled = ghoul::Boolean;

    StatsCollector(const std::string& filename, int dumpEveryXRecord,
        Enabled enabled = Enabled::Yes, const std::string& delimiter = ",");

    ~StatsCollector();
        
    void startNewRecord();

    void setEnabled(bool enabled);

    void disable();

    void enable();

    int hasHeaders();

    void dumpToDisk();

    TemplatedStatsCollector<long long> i;
    TemplatedStatsCollector<double> d;

private:
    void writeHeader();

    void writeData();

    std::string _filename;
    std::string _delimiter;

    int _dumpEveryXRecord;
    int _recordsSinceLastDump;

    bool _enabled;
    bool _hasWrittenHeader;
};

} // namespace globebrowsing
} // namespace openspace

#include "statscollector.inl"

#endif  // __OPENSPACE_MODULE_GLOBEBROWSING___STATS_TRACKER___H__
