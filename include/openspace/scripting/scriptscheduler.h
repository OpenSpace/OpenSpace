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

#ifndef __SCRIPTSCHEDULER_H__
#define __SCRIPTSCHEDULER_H__

#include <ghoul/misc/dictionary.h>

#include <openspace/scripting/lualibrary.h>

#include <queue>
#include <vector>

namespace openspace {

namespace scripting {



struct ReversibleLuaScript {
    std::string forwardScript;
    std::string backwardScript;
};

struct ScheduledScript {
    ScheduledScript() : time(-DBL_MAX) { }
    ScheduledScript(const ghoul::Dictionary& dict);

    double time;
    ReversibleLuaScript script;
};


/**
 * Maintains an ordered list of \code ScheduledScripts.
 */
class ScriptScheduler {
public:

    void loadScripts(const std::string& filename);
    void loadScripts(const ghoul::Dictionary& dict);

    void skipTo(double time);
    void skipTo(const std::string& timeStr);

    std::queue<std::string> scheduledScripts(double newTime);
    std::queue<std::string> scheduledScripts(const std::string& timeStr);

    const std::vector<ScheduledScript>& allScripts() const { return _scheduledScripts; };

    static LuaLibrary luaLibrary();

private:

    std::vector<ScheduledScript> _scheduledScripts;
    
    size_t _currentIndex = 0;
    double _lastTime;

};

} // namespace scripting
} // namespace openspace

#endif // __SCRIPTSCHEDULER_H__
