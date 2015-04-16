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
class ImageSequencer2 {
public:
    ImageSequencer2();
	/**
	* Singelton instantiation
	*/
	static ImageSequencer2* _instance;

	static ImageSequencer2& ref();
	static void initialize();
	static void deinitialize();

	bool isReady();

	//provides the sequencer with current time
	void updateSequencer(double time);
	void runSequenceParser(SequenceParser* parser);

	//translates playbook ambiguous namesetting to spice calls, augments each observation with targets and 
	//stores all images in its own subset (see _subsetMap member)
	bool parsePlaybookFile(const std::string& fileName, 
		                         std::string spacecraft, 
								 std::map<std::string, std::vector<std::string>> acronymDictionary,
								 std::vector<std::string> potentialTargets);

	// returns upcoming target
	std::pair<double, std::string> getNextTarget();
	// returns next target 
	std::pair<double, std::string> getCurrentTarget();
	// returns current target and (in the list) adjacent targets, the number to retrieve is user set
	std::pair<double, std::vector<std::string>> getIncidentTargetList(int range = 2);

	double getNextCaptureTime();
	double getIntervalLength();
	std::vector<std::pair<std::string, bool>> getActiveInstruments();
	bool ImageSequencer2::getImagePaths(std::vector<std::pair<double, std::string>>& captures, std::string projectee, std::string instrumentID);
	bool ImageSequencer2::getImagePaths(std::vector<std::pair<double, std::string>>& captures, std::string projectee);

	bool instumentActive(std::string instrumentID);

	const Image* findLatestImageInSubset( ImageSubset& subset);
private:
	//default comparison function
	bool imageComparer(const Image &a, const Image &b);

	//binary find O(log n) always
	std::vector<Image>::iterator binary_find(std::vector<Image>::iterator begin,
		std::vector<Image>::iterator end,
		const Image &val,
		bool(*imageComparer)(const Image &a, const Image &b));

	//var
	double _currentTime;
	double _previousTime;
	double _intervalLength;
	double _nextCapture;

    std::string _defaultCaptureImage;
	std::vector<std::pair<std::string, bool>> _instrumentOnOff;

	std::map<std::string, ImageSubset> _subsetMap;
	std::vector<std::pair<double, std::string>> _targetTimes;
	std::vector<std::pair<std::string, TimeRange>> _instrumentTimes;
	std::vector<double> _captureProgression;
	std::map<std::string, Decoder*> _fileTranslation;

	bool _hasData;
};

} // namespace openspace


#endif // __ImageSequencer2_H__

