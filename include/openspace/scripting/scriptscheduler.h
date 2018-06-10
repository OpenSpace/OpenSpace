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

#ifndef __OPENSPACE_CORE___SCRIPTSCHEDULER___H__
#define __OPENSPACE_CORE___SCRIPTSCHEDULER___H__

#include <string>
#include <vector>

namespace ghoul { class Dictionary; }
namespace openspace::documentation { struct Documentation; }

namespace openspace::scripting {

struct LuaLibrary;

/**
 * Maintains an ordered list of <code>ScheduledScript</code>s and provides a simple
 * interface for retrieveing scheduled scripts
 */
class ScriptScheduler {
public:
    struct ScheduledScript {
        ScheduledScript() = default;
        ScheduledScript(const ghoul::Dictionary& dictionary);

        double time = -std::numeric_limits<double>::max();
        std::string forwardScript;
        std::string backwardScript;
    };

    /**
     * Load a schedule from a ghoul::Dictionary \p dictionary and adds the
     * ScheduledScript%s to the list of stored scripts.
     * \param dictionary Dictionary to read
     * \throw SpecificationError If the dictionary does not adhere to the Documentation as
     * specified in the openspace::Documentation function
     */
    void loadScripts(const ghoul::Dictionary& dictionary);


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
     * scheduled to run between \param newTime and the time provided in the last
     * invocation of this method.
     *
     * \param newTime A j2000 time value specifying the new time stamp that
     * the script scheduler should progress to.
     *
     * \returns the ordered queue of scripts .
     */
//    std::queue<std::string> progressTo(double newTime);

    /**
     * See <code>progressTo(double newTime)</code>.
     *
     * \param timeStr A string specifying the a new time stamp that the
     * scripts scheduler should progress to.
     */
//    std::queue<std::string> progressTo(const std::string& timeStr);

    using ScriptIt = std::vector<std::string>::const_iterator;
    std::pair<ScriptIt, ScriptIt> progressTo(double newTime);

    /**
     * Returns the the j2000 time value that the script scheduler is currently at
     */
    double currentTime() const;

    /**
     * \returns a vector of all scripts that has been loaded
     */
    std::vector<ScheduledScript> allScripts() const;


    static LuaLibrary luaLibrary();

    static documentation::Documentation Documentation();

private:
    std::vector<double> _timings;
    std::vector<std::string> _forwardScripts;
    std::vector<std::string> _backwardScripts;

    int _currentIndex = 0;
    double _currentTime = 0;
};

} // namespace openspace::scripting

#endif // __OPENSPACE_CORE___SCRIPTSCHEDULER___H__
