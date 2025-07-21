/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <openspace/properties/propertyowner.h>

#include <openspace/navigation/keyframenavigator.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/scripting/lualibrary.h>

#include <functional>
#include <optional>
#include <queue>
#include <string>
#include <vector>

namespace ghoul { class Dictionary; }
namespace openspace::documentation { struct Documentation; }

namespace openspace::scripting {

struct LuaLibrary;

/**
 * Maintains an ordered list of `ScheduledScript`s and provides a simple interface for
 * retrieveing scheduled scripts.
 */
class ScriptScheduler : public properties::PropertyOwner {
public:
    ScriptScheduler();

    struct ScheduledScript {
        ScheduledScript() = default;
        explicit ScheduledScript(const ghoul::Dictionary& dict);

        double time = -std::numeric_limits<double>::max();
        std::string forwardScript;
        std::string backwardScript;
        std::string universalScript;

        int group = 0;
    };

    /**
     * Load a schedule from a ghoul::Dictionary \p scheduledScripts and adds the
     * ScheduledScript%s to the list of stored scripts.
     *
     * \param scheduledScripts The scripts that should be loaded
     *
     * \throw SpecificationError If the dictionary does not adhere to the Documentation as
     *        specified in the openspace::Documentation function
     */
    void loadScripts(std::vector<ScheduledScript> scheduledScripts);


    /**
     * Rewinds the script scheduler to the first scheduled script.
     */
    void rewind();

    /**
     * Removes all scripts for the schedule.
     *
     * \param group An int that specifies which group to clear. If none given then all
     *              scripts are cleared from the schedule
     */
    void clearSchedule(std::optional<int> group = std::nullopt);

    /**
     * Progresses the script schedulers time and returns all scripts that has been
     * scheduled to run between \p newTime and the time provided in the last invocation
     * of this method.
     *
     * \param newTime A j2000 time value specifying the new time stamp that the script
     *        scheduler should progress to.
     * \return vector with the scheduled scripts that should be run from begining to end
     */
    std::vector<std::string> progressTo(double newTime);

    /**
     * Returns the the j2000 time value that the script scheduler is currently at.
     */
    double currentTime() const;

    /**
     * Updates the current time to the given J2000 time value.
     */
    void setCurrentTime(double time);

    /**
     * Function that returns all scripts currently loaded in the script scheduler.
     *
     * \param group An int specifying which group to return, if empty all scripts
     *              will be returned
     * \return a vector of all scripts that has been loaded
     */
    std::vector<ScheduledScript> allScripts(
        std::optional<int> group = std::nullopt) const;

    static LuaLibrary luaLibrary();

    static documentation::Documentation Documentation();

private:
    properties::BoolProperty _enabled;
    properties::BoolProperty _shouldRunAllTimeJump;
    std::vector<ScheduledScript> _scripts;

    int _currentIndex = 0;
    double _currentTime = 0;
};

} // namespace openspace::scripting

#endif // __OPENSPACE_CORE___SCRIPTSCHEDULER___H__
