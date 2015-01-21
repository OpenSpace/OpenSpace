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
	static ImageSequencer& ref();
	bool loadSequence(const std::string dir);

	void testStartTimeMap();

	static void initialize();
	static void deinitialize();
	bool sequenceReset();

	bool getImagePath(double& _currentTime, std::string& path, bool closedInterval = false);
	bool getImagePath(std::string _currentTime, std::string& path, bool closedInterval = false);
	double getNextCaptureTime();
	double getIntervalLength(){ return _intervalLength; };

	static ImageSequencer* _sequencer;

private:
	double nextCaptureTime(double _time);

	void createImage(double t1, double t2, std::string path = "dummypath");
	
	double _nextCapture;
	double _intervalLength;
};

} // namespace openspace

#endif // __IMAGESEQUENCER_H__