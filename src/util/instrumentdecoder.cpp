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

#include <openspace/util/instrumentdecoder.h>

namespace {
    const std::string _loggerCat  = "InstrumentDecoder";
	const std::string keyDetector = "DetectorType";
	const std::string keySpice    = "Spice";
}

namespace openspace {
   
InstrumentDecoder::InstrumentDecoder(const ghoul::Dictionary& dictionary)
{
	bool success = dictionary.getValue(keyDetector, _type);
	ghoul_assert(success, "Instrument has not provided detector type");
	for_each(_type.begin(), _type.end(), [](char& in){ in = ::toupper(in); });

	std::vector<std::string> spice;
	ghoul::Dictionary spiceDictionary;
	success = dictionary.getValue(keySpice, spiceDictionary);
	ghoul_assert(success, "Instrument did not provide spice ids");


	_spiceIDs.resize(spiceDictionary.size());
	for (int i = 0; i < _spiceIDs.size(); ++i) {
		std::string id;
		spiceDictionary.getValue(std::to_string(i + 1), id);
		_spiceIDs[i] = id;
	}
}

std::string InstrumentDecoder::getDecoderType(){
	return _type;
}

std::vector<std::string> InstrumentDecoder::getTranslation(){
	return _spiceIDs;
}

} // namespace openspace