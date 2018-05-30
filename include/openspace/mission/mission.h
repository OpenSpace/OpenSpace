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

#ifndef __OPENSPACE_CORE___MISSION___H__
#define __OPENSPACE_CORE___MISSION___H__

#include <openspace/util/timerange.h>

#include <string>
#include <vector>

namespace ghoul { class Dictionary; }

namespace openspace {

namespace documentation {  struct Documentation; }

/**
 * Used to represent a named period of time within a mission. Allows nested phases, i.e.
 * phases within phases. Designed for WORM usage (Write Once, Read Multiple), and,
 * therefore, has only accessors.
 *
 * Each MissionPhase is characterized by its MissionPhase::name, a TimeRange, an
 * optional MissionPhase::description, and optional subphases.
 */
class MissionPhase {
public:
    /**
     * Constructs a MissionPhase from the information provided in the \p dictionary. See
     * the MissionPhase::Documentation for accepted ghoul::Dictionary values.
     *
     * \param dictionary The ghoul::Dictionary that contains information about the current
     *        MissionPhase
     *
     * \throw SpecificationError If the \p dictionary does not adhere to the Documentation
     * \throw RuntimeError If the time range of subphases is smaller than the specified
     *        time range
     * \throw RuntimeError If neither subphases or a time range is specified
     */
    MissionPhase(const ghoul::Dictionary& dictionary);

    /**
     * Returns the name of the MissionPhase.
     *
     * \return The name of the MissionPhase
     */
    const std::string& name() const;

    /**
     * Returns the TimeRange of the MissionPhase.
     *
     * \return The TimeRange of the MissionPhase
     */
    const TimeRange& timeRange() const;

    /**
     * Returns the description of the MissionPhase.
     *
     * \return The description of the MissionPhase
     */
    const std::string& description() const;

    /**
     * Returns all subphases sorted by start time.
     *
     * \return All subphases sorted by start time
     */
    const std::vector<MissionPhase>& phases() const;

    using Trace = std::vector<std::reference_wrapper<const MissionPhase>>;

    /**
     * Returns all MissionPhase%s whose MissionPhase::timeRange includes the provided
     * \p time, up to a maximum subphase depth of \p maxDepth.
     *
     * \param time The time in which the subphases have to be active in order to be
     *        included
     * \param maxDepth The maximum levels of subphases that will be considered. If this
     *        value is equal to <code>-1</code>, an infinite depth will be considered.
     * \return A list of MissionPhases that cover the provided \p time
     */
    Trace phaseTrace(double time, int maxDepth = -1) const;

    /**
     * Returns the Documentation that describes the ghoul::Dictionarty that this
     * MissionPhase can be constructed from.
     * \return The Documentation that describes the required structure for a Dictionary
     */
    static documentation::Documentation Documentation();

protected:
    /**
     * Recursive function that walks the subphases and adds the MissionPhase%s that cover
     * the provided \p time and adds these to the list of \p trace%s. Each recursive call
     * will decrease the \p maxDepth counter until it reaches 0.
     *
     * \param time The time which the subphases have to cover to be added to the \p trace
     * \param trace The list of MissionPhase%s that are active during the time \p time
     * \param maxDepth The maximum depth of levels that will be considered
     *
     * \pre maxDepth must not be negative
     */
    void phaseTrace(double time, Trace& trace, int maxDepth) const;

    /// The name of the MissionPhase
    std::string _name;
    /// The description of the MissionPhase
    std::string _description;
    /// The range in time that is covered by this MissionPhase
    TimeRange _timeRange;
    /// A list of subphases into which this MissionPhase is separated
    std::vector<MissionPhase> _subphases;
};

/**
 * A Mission is a list of MissionPhases that has a name, an optional description, a
 * TimeRange for which the Mission is active, and a potential list of subphases.
 */
using Mission = MissionPhase;

/**
 * This function constructs a Mission from the provided \p filename. The file must be a
 * Lua table that describes the Mission according to MissionPhase::Documentation.
 *
 * \param filename The file that is used to create the Mission
 * \return The constructed Mission
 *
 * \pre \p filename must not be empty
 * \pre \p filename must not contain tokens
 * \pre \p filename must exist
 */
Mission missionFromFile(const std::string& filename);

} // namespace openspace

#endif // __OPENSPACE_CORE___MISSION___H__
