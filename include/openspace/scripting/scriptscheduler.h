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

    static bool CompareByTime(const ScheduledScript& s1, const ScheduledScript& s2);
};


/**
 * Maintains an ordered list of <code>ScheduledScript</code>s and provides a simple 
 * interface for retrieveing scheduled scripts
 */
class ScriptScheduler {
public:

    /**
    * Load a schedule from a Lua-file
    * \param filename Lua file to load
    * \param L an optional lua_State defining variables that may be used
    * in the Lua-file.
    */
    void loadScripts(const std::string& filename, lua_State* L = nullptr);

    /**
    * Load a schedule from a <code>ghoul::Dictionary</code>
    * \param dict Dictionary to read
    */
    void loadScripts(const ghoul::Dictionary& dict);


    /**
    * Rewinds the script scheduler to the first scheduled script.
    */
    void rewind();

    /**
    * Removes all scripts for the schedule.
    */
    void clearSchedule();

    /**
    * Progresses the script schedulers time and returns all scripts that has been 
    * scheduled to run between \param newTime and the time provided in the last invocation 
    * of this method.
    *
    * \param newTime A j2000 time value specifying the new time stamp that
    * the script scheduler should progress to.
    *
    * \returns the ordered queue of scripts .
    */
    std::queue<std::string> progressTo(double newTime);

    /**
    * See <code>progressTo(double newTime)</code>.
    *
    * \param timeStr A string specifying the a new time stamp that the
    * scripts scheduler should progress to.
    */
    std::queue<std::string> progressTo(const std::string& timeStr);



    /**
    * Returns the the j2000 time value that the script scheduler is currently at 
    */
    double currentTime() const;

    /**
    * \returns a vector of all scripts that has been loaded
    */
    const std::vector<ScheduledScript>& allScripts() const;


    static LuaLibrary luaLibrary();

private:

    std::vector<ScheduledScript> _scheduledScripts;

    size_t _currentIndex = 0;
    double _currentTime;

};

} // namespace scripting
} // namespace openspace

#endif // __SCRIPTSCHEDULER_H__
