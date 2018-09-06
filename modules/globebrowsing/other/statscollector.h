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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___STATS_COLLECTOR___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___STATS_COLLECTOR___H__

#include <modules/globebrowsing/other/templatedstatscollector.h>

#include <ghoul/misc/boolean.h>
#include <string>

namespace openspace::globebrowsing {

class StatsCollector {
public:
    BooleanType(Enabled);

    StatsCollector() = delete;

    StatsCollector(std::string filename, int dumpEveryXRecord,
        Enabled enabled = Enabled::Yes, std::string delimiter = ",");

    ~StatsCollector();

    void startNewRecord();

    void setEnabled(bool enabled);

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
    int _recordsSinceLastDump = 0;

    bool _hasWrittenHeader = false;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___STATS_COLLECTOR___H__
