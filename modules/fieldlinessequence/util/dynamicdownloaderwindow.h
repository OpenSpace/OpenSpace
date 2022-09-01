/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___DYNAMICDOWNLOADERWINDOW___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___DYNAMICDOWNLOADERWINDOW___H__

#include <ghoul/filesystem/filesystem.h>
#include <vector>
#include <string>

namespace openspace {

struct SlidingWindow {
    std::vector<std::pair<double, std::string>> triggerTimes;
    int activeTriggerTime;
    int windowSize;
};

class DynamicDownloaderWindow {
public:
    void updateWindow(const double time, const double deltaTime, std::filesystem::path& p);
private:
    SlidingWindow _slidingWindow;

    // maybe edge mode
    // maybe a worker class but preferably not
    // maybe pointers to the renderable? Sounds more like a manager thing to own
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___DYNAMICDOWNLOADERWINDOW___H__
