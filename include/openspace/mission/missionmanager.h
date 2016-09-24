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

#ifndef __MISSIONPHASEEQUENCER_H__
#define __MISSIONPHASEEQUENCER_H__

#include <openspace/mission/mission.h>

#include <map>
#include <string>

namespace openspace {

namespace scripting { struct LuaLibrary; }

/**
* Singleton class keeping track of space missions. 
*/
class MissionManager {
public:
    static MissionManager& ref();
    
    static void initialize();
    static void deinitialize();

    /**
    * Reads a mission from file and maps the mission name to the Mission object. If
    * this is the first mission to be loaded, the mission will also be set as the 
    * current active mission.
    */
    void loadMission(const std::string& fileName);

    /**
    * Sets the mission with the name <missionName> as the current mission. The current
    * mission is what is return by `currentMission()`.
    */
    void setCurrentMission(const std::string& missionName);

    /**
    * Returns true if a current mission exists
    */
    bool hasCurrentMission() const;

    /**
    * Returns the latest mission specified to `setCurrentMission()`. If no mission has 
    * been specified, the first mission loaded will be returned. If no mission has been 
    * loaded, a warning will be printed and a dummy mission will be returned.
    */
    const Mission& currentMission();
    
private:
    static scripting::LuaLibrary luaLibrary();
    static MissionManager* _instance;

    //typedef std::map<std::string, Mission> MissionMap;
    using MissionMap = std::map<std::string, Mission>;
    MissionMap _missionMap;

    MissionMap::iterator _currentMission;

    // Singleton
    MissionManager();
};

} // namespace openspace


#endif // __MISSIONPHASEEQUENCER_H__

