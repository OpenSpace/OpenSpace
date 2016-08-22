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


#include <vector>
#include <string>
#include <unordered_map>
#include <modules/newhorizons/util/timerange.h>

#include <ghoul/misc/dictionary.h>
#include <ghoul/lua/ghoul_lua.h>


namespace openspace {


/**
* Used to represent a named period of time within a mission. Allows nested phases, i.e.
* phases within phases. Designed for WORM usage (Write Once, Read Multiple), and therefor
* has only accessors.
*/
class MissionPhase {
public:
    MissionPhase() {};
    MissionPhase(const ghoul::Dictionary& dict);

    const std::string& name() const { return _name; }

    const TimeRange timeRange() const { return _timeRange; };

    /**
    * Returns all subphases sorted by start time
    */
    const std::vector<MissionPhase>& phases() const { return _subphases; }

    /**
    * Returns the i:th subphase, sorted by start time
    */
    const MissionPhase& phase(size_t i) const { return _subphases[i]; }

protected:

    std::string _name;
    TimeRange _timeRange;
    std::vector<MissionPhase> _subphases;
};



class Mission : public MissionPhase {
public:
    Mission() {};
    Mission(std::string filename);

private:
    static ghoul::Dictionary readDictFromFile(std::string filepath);
    std::string _filepath;
};

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
    void setCurrentMission(const std::string missionName);

    /**
    * Returns the latest mission specified to `setCurrentMission()`. If no mission has 
    * been specified, the first mission loaded will be returned. If no mission has been 
    * loaded, a warning will be printed and a dummy mission will be returned.
    */
    const Mission& currentMission();

    
private:

    static scripting::LuaLibrary luaLibrary();
    static MissionManager* _instance;

    typedef std::unordered_map<std::string, Mission> MissionMap;
    MissionMap _missionMap;
    MissionMap::iterator _currentMissionIter;

    // Singleton
    MissionManager() : _currentMissionIter(_missionMap.end()) { };
};

} // namespace openspace


#endif // __MISSIONPHASEEQUENCER_H__

