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

namespace openspace::globebrowsing {

template <typename T>
TemplatedStatsCollector<T>::TemplatedStatsCollector(Enabled enabled,
                                                    std::string delimiter)
    : _enabled(enabled)
    , _delimiter(std::move(delimiter))
{}

template <typename T>
void TemplatedStatsCollector<T>::startNewRecord() {
    if (_enabled) {
        _data.push_back(StatsRecord<T>());
    }
}

template <typename T>
T& TemplatedStatsCollector<T>::operator[](const std::string& name) {
    if (_enabled) {
        _data.keys.insert(name);
        return _data.back()[name];
    }
    else {
        return _dummy;
    }
}

template<typename T>
T TemplatedStatsCollector<T>::previous(const std::string& name) const {
    if (_data.size() > 1) {
        return _data[_data.size() - 2][name];
    }
    return T();
}

template<typename T>
void TemplatedStatsCollector<T>::setEnabled(bool enabled) {
    _enabled = enabled;
}

template<typename T>
bool TemplatedStatsCollector<T>::enabled() const {
    return _enabled;
}

template<typename T>
bool TemplatedStatsCollector<T>::hasHeaders() const {
    return !_data.keys.empty();
}

template<typename T>
bool TemplatedStatsCollector<T>::hasRecordsToWrite() const {
    return _writePos < (_data.size() - 1);
}

template<typename T>
void TemplatedStatsCollector<T>::reset() {
    // copy last, i.e. current record
    StatsRecord<T> lastRecord = _data.back();
    _data.clear();
    // add it again after cleared the vector
    _data.push_back(std::move(lastRecord));
    _writePos = 0;
}

template<typename T>
void TemplatedStatsCollector<T>::writeHeader(std::ostream& os) {
    auto keyIt = _data.keys.begin();
    os << *keyIt;
    while (++keyIt != _data.keys.end()) {
        os << _delimiter << *keyIt;
    }
}

template<typename T>
void TemplatedStatsCollector<T>::writeNextRecord(std::ostream& os) {
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

} // namespace openspace::globebrowsing
