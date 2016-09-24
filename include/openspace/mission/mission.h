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

#ifndef __MISSION_H__
#define __MISSION_H__

#include <openspace/util/timerange.h>

#include <list>
#include <string>
#include <vector>

namespace ghoul { class Dictionary; }

namespace openspace {

/**
* Used to represent a named period of time within a mission. Allows nested phases, i.e.
* phases within phases. Designed for WORM usage (Write Once, Read Multiple), and therefor
* has only accessors.
*/
class MissionPhase {
public:
    MissionPhase() = default;
    MissionPhase(const ghoul::Dictionary& dict);

    const std::string& name() const;

    const TimeRange& timeRange() const;

    const std::string& description() const;

    /**
    * Returns all subphases sorted by start time
    */
    const std::vector<MissionPhase>& phases() const;

    /**
    * Returns the i:th subphase, sorted by start time
    */
    const MissionPhase& phase(size_t i) const;

    std::vector<const MissionPhase*> phaseTrace(double time, int maxDepth = -1) const;

protected:
    bool phaseTrace(double time, std::vector<const MissionPhase*>& trace, int maxDepth) const;
    
    std::string _name;
    std::string _description;
    TimeRange _timeRange;
    std::vector<MissionPhase> _subphases;
};


class Mission : public MissionPhase {
public:
    Mission() = default;
    Mission(std::string filename);

private:
    static ghoul::Dictionary readDictFromFile(std::string filepath);
    std::string _filepath;
};

} // namespace openspace


#endif // __MISSION_H__
