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

#ifndef __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___IMAGESEQUENCER___H__
#define __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___IMAGESEQUENCER___H__

#include <modules/spacecraftinstruments/util/sequenceparser.h>

#include <map>
#include <string>
#include <utility>
#include <vector>

namespace openspace {

class Time;
class SequenceParser;

/**
* The ImageSequencer singleton main function is to manage the timekeeping and distribution
* of large image data-sets across all openspace renderable instances, both for past and
* future unmanned-spacecraft missions. To load the instance with data the client must
* provide a parser inherited from the abstract base class SequenceParser. Hence, there is
* no restriction imposed on data input, whether its data in the form of existing images or
* in the form of a planned observation schedule. Notably, in order for the sequencer to
* function the client must provide or write a parser that fills the ImageSequencers
* private members.
*
* \see SequenceParser
* \see ImageSequencer::runSequenceParser(SequenceParser* parser)
* std::map<std::string, bool>
*/
class ImageSequencer {
public:
    /**
     * Singelton instantiation
     */
    static ImageSequencer* _instance;
    /**
     * Returns the reference to the singleton ImageSequencer object that must have been
     * initialized by a call to the initialize method earlier.
     *
     * \return The ImageSequencer singleton
     */
    static ImageSequencer& ref();
    /**
     * Initializer that initializes the static member.
     */
    static void initialize();
    /**
     * Deinitializes that deinitializes the static member.
     */
    static void deinitialize();
    /**
     * Returns true if sequencer has been loaded with data.
     */
    bool isReady();

    /**
     * Runs parser and recieves the datastructures filled by it.
     * \see SequenceParser
     */
    void runSequenceParser(SequenceParser& parser);

    /**
     * Retrieves the next upcoming target in time.
     */
    std::pair<double, std::string> nextTarget(double time);

    /**
     * Retrieves the most current target in time.
     */
    std::pair<double, std::string> currentTarget(double time);

    /**
     * Retrieves current target and (in the list) adjacent targets, the number to retrieve
     * is user set
     */
    std::pair<double, std::vector<std::string>> incidentTargetList(double time,
                                                                   int range = 2);

    /**
     * Retrieves the next upcoming time of image capture.
     */
    double nextCaptureTime(double time);

    /**
     * Retrieves the time interval length between the current time and an upcoming
     * capture.
     */
    double intervalLength(double time);

    /**
     * Returns a vector with key instrument names whose value indicate whether an
     * instrument is active or not.
     */
    std::vector<std::pair<std::string, bool>> activeInstruments(double time);

    /**
     * Retrieves the relevant data from a specific subset based on the what instance
     * makes the request. If an instance is not registered in the class then the singleton
     * returns false and no projections will occur.
     */
    bool imagePaths(std::vector<Image>& captures, const std::string& projectee,
        const std::string& instrumentRequest, double time, double sinceTime);

    /**
     * returns true if instrumentID is within a capture range.
     */
    bool isInstrumentActive(double time, const std::string& instrumentID);

    float instrumentActiveTime(double time, const std::string& instrumentID) const;

    /**
     * returns latest captured image
     */
    Image latestImageForInstrument(const std::string& instrumentID);

private:
    void sortData();

    /**
     * _fileTranslation handles any types of ambiguities between the data and
     * spice/openspace -calls. This map is composed of a key that is a string in
     * the data to be translated and a Decoder that holds the corresponding
     * translation provided through a modfile.
     * \see Decoder
     * \see (projection mod files)
     */
    std::map<std::string, std::unique_ptr<Decoder>> _fileTranslation;

    /**
     * This is the main container of image data. The key is the target name,
     * the value is a subset of images.
     * \see SequenceParser
     */
    std::map<std::string, ImageSubset> _subsetMap;

    /**
     * In order for the simulation to know when to turn on/off any instrument within
     * all instruments in the spacecraft payload, the key is the data-file given
     * instrument name.
     */
    std::vector<std::pair<std::string, bool>> _switchingMap;

    /**
     * This datastructure holds the specific times when the spacecraft switches from
     * observing one inertial body to the next. This happens a lot in such missions
     * and the coupling of target with specific time is usually therefore not 1:1.
     */
    std::vector<std::pair<double, std::string>> _targetTimes;

    /**
     * Holds the time ranges of each instruments on and off periods. An instrument
     * rendering class may ask the ImageSequencer whether or not it
     */
    std::vector<std::pair<std::string, TimeRange>> _instrumentTimes;

    /**
     * Each consecutive images capture time, for easier traversal.
     */
    std::vector<double> _captureProgression;

    // time between current simulation time and an upcoming capture
    double _intervalLength = 0.0;
    // next consecutive capture in time
    double _nextCapture = 0.0;
    // default capture image
    std::string _defaultCaptureImage;

    std::map<std::string, Image> _latestImages;
    // if no data, no run
    bool _hasData = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___IMAGESEQUENCER___H__
