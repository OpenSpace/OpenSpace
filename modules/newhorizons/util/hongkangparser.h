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

#ifndef __HONGKANGPARSER_H__
#define __HONGKANGPARSER_H__

#include <modules/newhorizons/util/imagesequencer2.h>
#include <modules/newhorizons/util/sequenceparser.h>

#include <map>
#include <string>
#include <vector>

namespace openspace {

class HongKangParser : public SequenceParser {
public:
	HongKangParser();
	HongKangParser(const std::string& fileName,
			        std::string spacecraft,
					ghoul::Dictionary dictionary,
			        std::vector<std::string> potentialTargets);
	virtual void create();

	// temporary need to figure this out
	virtual std::map<std::string, Decoder*> getTranslation(){ return _fileTranslation; };

private:
	double getMetFromET(double et);
	double getETfromMet(std::string timestr);
	double getETfromMet(double met);

	void createImage(Image& image,
			            double startTime,
			            double stopTime,
			            std::vector<std::string> instr,
			            std::string targ,
			            std::string pot);

	bool augmentWithSpice(Image& image, 
			                std::string spacecraft, 
							std::vector<std::string> payload, 
							std::vector<std::string> potentialTargets);

	std::string _defaultCaptureImage;
	double _metRef = 299180517;

	std::string _fileName;
	std::string _spacecraft;
	std::map<std::string, Decoder*> _fileTranslation;
	std::vector<std::string> _potentialTargets;
};

}
#endif //__HONGKANGPARSER_H__