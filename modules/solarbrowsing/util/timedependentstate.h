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

#ifndef __OPENSPACE_MODULE_SOLARBROWSING___TIMEDEPENDENTSTATE___H__
#define __OPENSPACE_MODULE_SOLARBROWSING___TIMEDEPENDENTSTATE___H__

namespace openspace {

template<typename T>
class TimedependentState {
public:
    TimedependentState(std::shared_ptr<T> stateData, const double& timeObserved,
                       const std::string id = "")
        : _stateData(stateData)
        , _timeObserved(timeObserved)
        , _id(id)
    {}

    const std::string& id() const { return _id; }
    const double& timeObserved() const { return _timeObserved; }
    std::shared_ptr<T> contents() const { return _stateData; };

    bool operator<(const double val) const {
        return _timeObserved < val;
    }

private:
    double _timeObserved;
    std::string _id;
    std::shared_ptr<T> _stateData;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOLARBROWSING___TIMEDEPENDENTSTATE___H__
