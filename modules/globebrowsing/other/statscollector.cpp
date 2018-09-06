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

#include <modules/globebrowsing/other/statscollector.h>

#include <ghoul/misc/assert.h>
#include <fstream>
#include <iomanip>

namespace openspace::globebrowsing {

StatsCollector::StatsCollector(std::string filename, int dumpEveryXRecord,
                               Enabled enabled, std::string delimiter)
    : i(TemplatedStatsCollector<long long>(
        TemplatedStatsCollector<long long>::Enabled(enabled),
        delimiter
    ))
    , d(TemplatedStatsCollector<double>(
        TemplatedStatsCollector<double>::Enabled(enabled),
        delimiter
    ))
    , _filename(std::move(filename))
    , _delimiter(std::move(delimiter))
    , _dumpEveryXRecord(dumpEveryXRecord)
{
}

StatsCollector::~StatsCollector() {
    dumpToDisk();
}

void StatsCollector::startNewRecord() {
    ghoul_assert(i.enabled() == d.enabled(), "Both Statscollector have to be synced");

    if (i.enabled()) {
        if (_dumpEveryXRecord && (++_recordsSinceLastDump >= _dumpEveryXRecord)) {
            dumpToDisk();
            _recordsSinceLastDump = 0;
        }

        i.startNewRecord();
        d.startNewRecord();
    }
}

void StatsCollector::setEnabled(bool enabled) {
    i.setEnabled(enabled);
    d.setEnabled(enabled);
}

int StatsCollector::hasHeaders() {
    return i.hasHeaders() || d.hasHeaders();
}

void StatsCollector::dumpToDisk() {
    ghoul_assert(i.enabled() == d.enabled(), "Both Statscollector have to be synced");

    if (i.enabled() && hasHeaders()) {
        if (!_hasWrittenHeader) {
            writeHeader();
        }
        writeData();
    }
}

void StatsCollector::writeHeader() {
    std::ofstream ofs(_filename);
    if (i.hasHeaders()) {
        i.writeHeader(ofs);
        if (d.hasHeaders()) {
            ofs << _delimiter;
            d.writeHeader(ofs);
        }
    }
    else {
        d.writeHeader(ofs);
    }
    _hasWrittenHeader = true;
    ofs << std::endl;
    ofs.close();
}

void StatsCollector::writeData() {
    std::ofstream ofs(_filename, std::ofstream::out | std::ofstream::app);
    ofs << std::setprecision(32);
    while (i.hasRecordsToWrite() || d.hasRecordsToWrite()) {
        if (i.hasHeaders() && d.hasHeaders()) {
            i.writeNextRecord(ofs); ofs << _delimiter; d.writeNextRecord(ofs);
        }
        else {
            i.writeNextRecord(ofs); d.writeNextRecord(ofs);
        }
        ofs << std::endl;
    }
    i.reset();
    d.reset();
    ofs.close();
}

} // namespace openspace::globebrowsing
