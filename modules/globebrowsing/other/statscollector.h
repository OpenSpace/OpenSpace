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


    template <typename T> class StatsCollector{
    public:

        StatsCollector() = delete;

        StatsCollector(const std::string& filename, bool enabled = true)
            : _filename(filename)
            //, _writer(writer)
            , _enabled(enabled)
        {
            
        };

        ~StatsCollector() {
            if (_data.keys.size() > 0) {
                std::cout << "Saving stats to: " << _filename << std::endl;
                write();
            }
        };

        void disable() {
            _enabled = false;
        }

        void enable() {
            _enabled = true;
        }

        void startNewRecord() {
            if (_enabled) {
                _data.push_back(StatsRecord<T>());
            }
        }

        T& operator[](const std::string& name) {
            if (_enabled) {
                _data.keys.insert(name);
                return _data.back()[name];
            }
            else return _dummy;
        }

        T previous(const std::string& name) {
            if (_enabled && _data.size() > 1) {
                return _data[_data.size() - 2][name];   
            }
            return T();
        }


    private:

        void write() {
            std::string _delimiter = ",";
            std::ofstream ofs(_filename);

            // output headers
            auto keyIt = _data.keys.begin();
            ofs << *keyIt;
            while (++keyIt != _data.keys.end()) {
                ofs << _delimiter << *keyIt;
            }
            ofs << std::endl;

            // output line by line
            for (StatsRecord<T>& record : _data) {
                // Access every key. Records with no entry will get a default value
                keyIt = _data.keys.begin();
                ofs << record[(*keyIt)];
                while (++keyIt != _data.keys.end()) {
                    ofs << _delimiter << record[(*keyIt)];
                }
                ofs << std::endl;
            }
            ofs.close();
        }

    private:
        

        StatsCollection<T> _data;
        const std::string _filename;

        bool _enabled;
        T _dummy; // used when disabled
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