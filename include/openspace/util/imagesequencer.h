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

#ifndef __IMAGESEQUENCER_H__
#define __IMAGESEQUENCER_H__

// open space includes
#include <ghoul/opengl/ghoul_gl.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/powerscaledscalar.h>
#include <map>
#include <vector>

namespace openspace {

class ImageSequencer {
public:
    ImageSequencer();
	/**
	* Singelton instantiation
	*/
	static ImageSequencer& ref();
	static void initialize();
	static void deinitialize();

	/**
	* Updates current time and initializes the previous time member
	*/
	void update(double initTime);

	/**
	* When the a projectable class loads its sequence it also has to request an ID
	* which is set by reference and returned to the projectable. This ID is later
	* used to access whatever data ImageSequencer loads. 
	*/
	void setSequenceId(int& id);

	//bool sequenceReset();

	/**
	* Based on sequenceID and unique projectee name, the ImageSequencer determines which subset of data is to be filled to _imageTimes
	* which can be used in a projecable class for projections. 
	*/
	bool getImagePath(std::vector<std::pair<double, std::string>>& _imageTimes, int sequenceID, std::string projectee, bool withinFOV);

	/*
	* Returns the time until next capture in seconds.
	* 
	*/
	double getNextCaptureTime();

	/*
	* Returns the time until next capture in seconds.
	*/
	double getIntervalLength(){ return _intervalLength; };

	/*
	* Returns next active instrument 
	*/
	std::string& getActiveInstrument(){ return _activeInstrument; };

	/*
	* Performs search to find next consective instrument thats active
	*/
	std::string findActiveInstrument(double time, int sequenceID);

	/*
	* Performs search to find next consecutive projection image.
	*/
	double nextCaptureTime(double _time, int sequenceID);

	/*
	* Load (from *.fit converted) jpg image sequence based on corresponding *.lbl header files 
	*/
	bool loadSequence(const std::string& dir, int& sequenceID);
	/*
	* Load sequence file of either *.csv type (excel) or preparsed *.txt type
	*/
	bool parsePlaybook(const std::string& dir, const std::string& type, std::string year = "2015");
	bool parsePlaybookFile(const std::string& fileName, int& sequenceID, std::string year = "2015");
	/*
	* These three methods augment the playbook
	*/
	void augumentSequenceWithTargets(int sequenceID);
	void addSequenceObserver(int sequenceID, std::string name, std::vector<std::string> payload);
	void registerTargets(std::vector<std::string>& potential);


	static ImageSequencer* _sequencer;

protected:

	bool getMultipleImages(std::vector<std::pair<double, std::string>>& _imageTimes, int sequenceID, std::string projectee);
	bool getSingleImage(std::vector<std::pair<double, std::string>>& _imageTimes, int sequenceID, std::string projectee);

private:

	double getMissionElapsedTime(std::string timestr);

	std::map<std::string, double> _projectableTargets;
	std::map <int, std::string> _observers;
	std::map <std::string, std::vector<std::string>> _instruments;

	double _nextCapture;
	double _intervalLength;
	double _metRef = 299180517;
	double _currentTime;
	int _sequenceIDs;

    std::string _defaultCaptureImage;
	std::string _activeInstrument;

	bool _targetsAdded;

};

} // namespace openspace

#endif // __IMAGESEQUENCER_H__