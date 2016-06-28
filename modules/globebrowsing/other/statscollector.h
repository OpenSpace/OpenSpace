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
#ifndef __STATS_TRACKER_H__
#define __STATS_TRACKER_H__

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem>


#include <fstream>
#include <unordered_map>
#include <vector>
#include <set>
#include <memory>



namespace openspace {
    

    template <typename T> 
    struct StatsRecord : public std::unordered_map<std::string, T> {

    };

    template <typename T>
    struct StatsCollection : public std::vector<StatsRecord<T>> {
        std::set<std::string> keys;
    };


    template <typename T> class TemplatedStatsCollector{
    public:

        TemplatedStatsCollector(bool& enabled, const std::string& delimiter) 
            : _enabled(enabled) 
            , _delimiter(delimiter)
            , _writePos(0) { };

        ~TemplatedStatsCollector() { };

        void startNewRecord() {
            if(_enabled)
                _data.push_back(StatsRecord<T>());
        }

        T& operator[](const std::string& name) {
            if (_enabled) {
                _data.keys.insert(name);
                return _data.back()[name];
            }
            else return _dummy;
        }

        T previous(const std::string& name) {
            if (_data.size() > 1) {
                return _data[_data.size() - 2][name];   
            }
            return T();
        }

        bool hasHeaders() {
            return _data.keys.size() > 0;
        }

        bool hasRecordsToWrite() {
            return _writePos < _data.size() - 1;
        }

        void reset() {
            // copy last, i.e. current record
            StatsRecord<T> lastRecord = _data.back();
            _data.clear();
            // add it again after cleared the vector
            _data.push_back(lastRecord);
            _writePos = 0;
            
        }

        void writeHeader(std::ostream& os) {
            auto keyIt = _data.keys.begin();
            os << *keyIt;
            while (++keyIt != _data.keys.end()) {
                os << _delimiter << *keyIt;
            }
        }

        void writeNextRecord(std::ostream& os) {
            if (hasRecordsToWrite()) {
                // output line by line
                StatsRecord<T>& record = _data[_writePos];

                // Access every key. Records with no entry will get a default value
                auto keyIt = _data.keys.begin();
                if (keyIt != _data.keys.end()) {
                    os << record[(*keyIt)];
                    while (++keyIt != _data.keys.end()) {
                        os << _delimiter << record[(*keyIt)];
                    }
                }

                _writePos++;
            }
        }


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

        StatsCollector(const std::string& filename, int dumpEveryXRecord, const std::string& delimiter = ",", bool enabled = true)
            : _filename(filename)
            , _dumpEveryXRecord(dumpEveryXRecord)
            , _recordsSinceLastDump(0)
            , _enabled(enabled)
            , _delimiter(delimiter)
            , _hasWrittenHeader(false)
            , i(TemplatedStatsCollector<int>(_enabled, delimiter))
            , d(TemplatedStatsCollector<double>(_enabled, delimiter))
        {

        };

        ~StatsCollector() {
            dumpToDisk();
        }
        
        void startNewRecord() {
            if (_enabled) {
                if (_dumpEveryXRecord && ++_recordsSinceLastDump >= _dumpEveryXRecord) {
                    dumpToDisk();
                    _recordsSinceLastDump = 0;
                }

                i.startNewRecord();
                d.startNewRecord();
            }
        }

        void disable() {
            _enabled = false;
        }

        void enable() {
            _enabled = true;
        }

        int hasHeaders() {
            return i.hasHeaders() || d.hasHeaders();
        }

        void dumpToDisk() {
            if (_enabled && hasHeaders()) {
                if (!_hasWrittenHeader) {
                    writeHeader();
                }
                writeData();
            }
        }

        TemplatedStatsCollector<int> i;
        TemplatedStatsCollector<double> d;

    private:
        void writeHeader() {
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

        void writeData() {
            std::ofstream ofs(_filename, std::ofstream::out | std::ofstream::app);
            while (i.hasRecordsToWrite() || d.hasRecordsToWrite()) {
                if (i.hasHeaders() && d.hasHeaders()) {
                    i.writeNextRecord(ofs); ofs << _delimiter; d.writeNextRecord(ofs);
                }
                else {
                    i.writeNextRecord(ofs); d.writeNextRecord(ofs);
                }
                ofs << std::endl;
            }
            i.reset(); d.reset();
            ofs.close();
        }

        std::string _filename;
        std::string _delimiter;

        int _dumpEveryXRecord;
        int _recordsSinceLastDump;

        bool _enabled;
        bool _hasWrittenHeader;

    };

    /*

    template <typename T>
    struct StatsCsvWriter {
        StatsCsvWriter(const std::string& delimiter)
            : _delimiter(delimiter) { };

        virtual void write(const StatsCollection<T>& stats, const std::string& filename) {
            std::ofstream ofs(filename);
            
            // output headers
            auto keyIt = stats.keys.begin();
            ofs << *keyIt;
            while (keyIt++ != stats.keys.end()) {
                ofs << _delimiter << *keyIt;
            }
            ofs << std::endl;

            // output line by line
            for (const StatsRecord<T>& record : stats) {
                // Access every key. Records with no entry will get a default value
                auto keyIt = stats.keys.begin();
                ofs << record[*keyIt];
                while (keyIt++ != stats.keys.end()) {
                    ofs << _delimiter << record[*keyIt];
                }
                ofs << std::endl;
            }
            ofs.close();
        }

    private:
        std::string _delimiter;
    };
    */


} // namespace openspace





#endif  // __STATS_TRACKER_H__