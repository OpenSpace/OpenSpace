/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2017                                                               *
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

#include <modules/globebrowsing/models/multimodelprovider.h>

#include <ghoul/logging/logmanager.h>

namespace {
	const std::string _loggerCat = "MultiModelProvider";
}

namespace openspace {
namespace globebrowsing {
MultiModelProvider::MultiModelProvider(const ghoul::Dictionary& dictionary) 
	: ModelProvider(dictionary) {
}

std::vector<std::shared_ptr<Subsite>> MultiModelProvider::calculate(const std::vector<std::vector<std::shared_ptr<Subsite>>> subsites, const RenderData& data) {
	std::vector<std::shared_ptr<Subsite>> ss;
	for (auto subSites : subsites) {
		if (subSites.size() > 0) {
			ss.push_back(subSites.at(0));
		}
	}
	return ss;
}

bool MultiModelProvider::initialize() {
	return true;
}
}
}