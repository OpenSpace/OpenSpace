/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <fstream>
#include <iomanip>
#include <string>

namespace openspace {
namespace globebrowsing {

StatsCollector::StatsCollector(const std::string& filename, int dumpEveryXRecord,
                               Enabled enabled, const std::string & delimiter)
    : i(TemplatedStatsCollector<long long>(_enabled, delimiter))
    , d(TemplatedStatsCollector<double>(_enabled, delimiter))
    , _filename(filename)
    , _delimiter(delimiter)
    , _dumpEveryXRecord(dumpEveryXRecord)
    , _recordsSinceLastDump(0)
    , _enabled(enabled)
    , _hasWrittenHeader(false)
{}

StatsCollector::~StatsCollector() {
    dumpToDisk();
}

void StatsCollector::startNewRecord() {
    if (_enabled) {
        if (_dumpEveryXRecord && ++_recordsSinceLastDump >= _dumpEveryXRecord) {
            dumpToDisk();
            _recordsSinceLastDump = 0;
        }

        i.startNewRecord();
        d.startNewRecord();
    }
}

void StatsCollector::setEnabled(bool enabled) {
    _enabled = enabled;
}

void StatsCollector::disable() {
    _enabled = false;
}

void StatsCollector::enable() {
    _enabled = true;
}

int StatsCollector::hasHeaders() {
    return i.hasHeaders() || d.hasHeaders();
}

void StatsCollector::dumpToDisk() {
    if (_enabled && hasHeaders()) {
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

} // namespace globebrowsing
} // namespace openspace
