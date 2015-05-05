/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2015                                                               *
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


#ifndef __SEQUENCEPARSER_H__
#define __SEQUENCEPARSER_H__
#include <openspace/util/decoder.h>


#include <map>
#include <string>
#include <vector>

namespace openspace {
	struct Image{
		double startTime;
		double stopTime;
		std::string path;
		std::vector<std::string> activeInstruments; 
		std::string target;
		bool projected;
	};
	struct TimeRange{
		TimeRange() : _min(-1), _max(-1){};
		void setRange(double val){
			if (_min > val) _min = val;
			if (_max < val) _max = val;
		};
		bool inRange(double min, double max){
			return (min >= _min && max <= _max);
		}
		bool inRange(double val){
			return (val >= _min && val <= _max);
		}
		double _min;
		double _max;
	};
	struct ImageSubset{
		TimeRange _range;
		std::vector < Image > _subset;
	};

class SequenceParser {
public:
	virtual void create() = 0;
	virtual std::map<std::string, ImageSubset> getSubsetMap();
	virtual std::vector<std::pair<std::string, TimeRange>> getIstrumentTimes();
	virtual std::vector<std::pair<double, std::string>> getTargetTimes();
	virtual std::map<std::string, Decoder*> getTranslation() = 0;
	virtual std::vector<double> getCaptureProgression() ;

protected:
    void sendPlaybookInformation();

    std::map<std::string, ImageSubset> _subsetMap;
    std::vector<std::pair<std::string, TimeRange>> _instrumentTimes;
    std::vector<std::pair<double, std::string>> _targetTimes;
    std::vector<double> _captureProgression;

};
}
#endif //__SEQUENCEPARSER_H__