/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014                                                                    *
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

#ifndef __ImageSequencer2_H__
#define __ImageSequencer2_H__

// open space includes
#include <ghoul/opengl/ghoul_gl.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/powerscaledscalar.h>
#include <unordered_map>
#include <map>
#include <vector>

#include <openspace/util/sequenceparser.h>

namespace openspace {
	/**
	* The ImageSequencer singleton main function is to manage the timekeeping and
	* distribution of large image data-sets across all openspace renderable instances, 
	* both for past and future unmanned-spacecraft missions. To load the instance with 
	* data the client must provide a parser inherited from the abstract base class 
	* SequenceParser. Hence, there is no restriction imposed on data input, whether its
	* data in the form of existing images or in the form of a planned observation schedule. 
	* Notably, in order for the sequencer to function the client must provide or write a 
	* parser that fills the ImageSequencers private members.
	* \see SequenceParser
	* \see ImageSequencer2::runSequenceParser(SequenceParser* parser)
	* std::map<std::string, bool>
	*/
class ImageSequencer2 {
public:
    ImageSequencer2();
	/**
	* Singelton instantiation
	*/
	static ImageSequencer2* _instance;
	/**
	* Returns the reference to the singleton ImageSequencer object that must have been
	* initialized by a call to the initialize method earlier.
	* \return The ImageSequencer singleton
	*/
	static ImageSequencer2& ref();
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
	 * Updates sequencer with current <code>time</code>. This is used internally for keeping
	 * track of both current simulation time and the time of the previously rendered frame.  
	 */	
	void updateSequencer(double time);
	/**
	* Runs parser and recieves the datastructures filled by it. 
	* \see SequenceParser
	*/
	void runSequenceParser(SequenceParser* parser);

	/**
	 * Retrieves the next upcoming target in time.  
	 */
	std::pair<double, std::string> getNextTarget();

	/**
	 * Retrieves the most current target in time.
	 */
	std::pair<double, std::string> getCurrentTarget();

	/**
	 * Retrieves current target and (in the list) adjacent targets, the number to retrieve is user set
	 */
	std::pair<double, std::vector<std::string>> getIncidentTargetList(int range = 2);

	/**
	* Retrieves the next upcoming time of image capture. 
	*/
	double getNextCaptureTime();

	/**
	* Retrieves the time interval length between the current time and an upcoming capture. 
	*/
	double getIntervalLength();

	/*
	 * Returns a map with key instrument names whose value indicate whether
	 * an instrument is active or not. 
	 */
	std::map<std::string, bool> getActiveInstruments();

	/*
	 * Retrieves the relevant data from a specific subset based on the what instance
	 * makes the request. If an instance is not registered in the class then the singleton
	 * returns false and no projections will occur. 
	 */
	bool getImagePaths(std::vector<Image>& captures, 
		                                std::string projectee, 
										std::string instrumentID);

	bool getImagePaths(std::vector<Image>& captures,
		                                std::string projectee);

	/*
	 * returns true if instrumentID is within a capture range. 
	 */
	bool instumentActive(std::string instrumentID);

    float instrumentActiveTime(const std::string& instrumentID) const;

	/*
	* returns latest captured image
	*/
	const Image* getLatestImageForInstrument(const std::string _instrumentID);
private:
	void sortData();
	
	/*
	 * _fileTranslation handles any types of ambiguities between the data and 
	 * spice/openspace -calls. This map is composed of a key that is a string in 
	 * the data to be translated and a Decoder that holds the corresponding 
	 * translation provided through a modfile.
	 * \see Decoder
	 * \see (projection mod files)
	 */
	std::map<std::string, Decoder*> _fileTranslation;

	/*
	 * This is the main container of image data. The key is the target name, 
	 * the value is a subset of images. 
	 * \see SequenceParser
	 */
	std::map<std::string, ImageSubset> _subsetMap;

	/*
	 * In order for the simulation to know when to turn on/off any instrument within
	 * all instruments in the spacecraft payload, the key is the data-file given
	 * instrument name.
	 */ 
	std::map<std::string, bool> _switchingMap;

	/*
	 * This datastructure holds the specific times when the spacecraft switches from 
	 * observing one inertial body to the next. This happens a lot in such missions 
	 * and the coupling of target with specific time is usually therefore not 1:1. 
	 */
	std::vector<std::pair<double, std::string>> _targetTimes;

	/*
	 * Holds the time ranges of each instruments on and off periods. An instrument
	 * rendering class may ask the ImageSequencer whether or not it 
	 */
	std::vector<std::pair<std::string, TimeRange>> _instrumentTimes;

	/*
	* Each consecutive images capture time, for easier traversal. 
	*/
	std::vector<double> _captureProgression;

	// current simulation time
	double _currentTime;
	// simulation time of previous frame
	double _previousTime;
	// time between current simulation time and an upcoming capture
	double _intervalLength;
	// next consecutive capture in time
	double _nextCapture;
	// default capture image
	std::string _defaultCaptureImage;

	Image* _latestImage;
	// if no data, no run 
	bool _hasData;
};

} // namespace openspace


#endif // __ImageSequencer2_H__

