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

#include <openspace/util/ImageSequencer2.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/directory.h>
#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>
#include <fstream>
#include <iterator>
#include <iomanip>
#include <limits>

#include <openspace/util/labelparser.h>

namespace {
	const std::string _loggerCat = "LabelParser";
	const std::string keySpecs   = "Read";
	const std::string keyConvert = "Convert";
}

namespace openspace {
LabelParser::LabelParser(const std::string& fileName,
				         ghoul::Dictionary translationDictionary)
{
	_fileName          = fileName;
	//_fileTranslation   = fileTranslation;

	//get the different instrument types
	const std::vector<std::string>& decoders = translationDictionary.keys();
	//for each decoder (assuming might have more if hong makes changes)
	for (int i = 0; i < decoders.size(); i++){
		ghoul::Dictionary typeDictionary;
		translationDictionary.getValue(decoders[i], typeDictionary);

		//create dictionary containing all {playbookKeys , spice IDs}
		if (decoders[i] == "Instrument"){
			//for each playbook call -> create a Decoder object
			const std::vector<std::string>& keys = typeDictionary.keys();
			for (int j = 0; j < keys.size(); j++){
				std::string currentKey = decoders[i] + "." + keys[j];

				ghoul::Dictionary decoderDictionary;
				translationDictionary.getValue(currentKey, decoderDictionary);

				Decoder *decoder = Decoder::createFromDictionary(decoderDictionary, decoders[i]);
				//insert decoder to map - this will be used in the parser to determine
				//behavioral characteristics of each instrument
				_fileTranslation[keys[j]] = decoder;
			}
		}
		if (decoders[i] == "Target"){
			ghoul::Dictionary specsOfInterestDictionary;
			typeDictionary.getValue(keySpecs, specsOfInterestDictionary);

			_specsOfInterest.resize(specsOfInterestDictionary.size());
			for (int i = 0; i < _specsOfInterest.size(); ++i) {
				std::string readMe;
				specsOfInterestDictionary.getValue(std::to_string(i + 1), readMe);
				_specsOfInterest[i] = readMe;
			}
			ghoul::Dictionary convertDictionary;
			typeDictionary.getValue(keyConvert, convertDictionary);

			const std::vector<std::string>& keys = convertDictionary.keys();
			for (int j = 0; j < keys.size(); j++){
				ghoul::Dictionary itemDictionary;
				convertDictionary.getValue(keys[j], itemDictionary);
				Decoder *decoder = Decoder::createFromDictionary(itemDictionary, decoders[i]);
				//insert decoder to map - this will be used in the parser to determine
				//behavioral characteristics of each instrument
				_fileTranslation[keys[j]] = decoder;
			};
		}
	}
	for (auto t : _fileTranslation){
		std::cout << t.first << std::endl;
		for (auto b : t.second->getTranslation()){
			std::cout << "   " << b << std::endl;
		}
	}

}
void LabelParser::create(){

	std::vector<Image> tmp;


	ghoul::filesystem::Directory sequenceDir(_fileName, true);
	std::vector<std::string> sequencePaths = sequenceDir.read(true, false); // check inputs 
	for (auto path : sequencePaths){
		if (size_t position = path.find_last_of(".") + 1){
			if (position != std::string::npos){
				ghoul::filesystem::File currentFile(path);
				std::string extension = currentFile.fileExtension();

				if (extension == "lbl"){ // discovered header file 		
					std::ifstream file(currentFile.path());

					if (!file.good()) LERROR("Failed to open label file '" << currentFile.path() << "'");
					
					// open up label files
					std::string line = "";
					double timestamp = 0.0;
					bool found = false;
					do {
						std::getline(file, line);
						for (auto spec : _specsOfInterest){
							auto pos = line.find(spec);
							if (pos != std::string::npos){
								if (line.substr(0, line.find_first_of(" ")) == "TARGET_NAME"){
								/*	std::string target = line.substr(line.find("=") + 2, line.find(" "));
									target.erase(std::remove(target.begin(), target.end(), '"'), target.end());
									for (auto t : _fileTranslation[target]->getTranslation()){
										std::cout << t << std::endl;
									}*/

								}


							}
							/*if (timestamp != 0.0){
							found = true;
							std::string ext = "jpg";
							path.replace(path.begin() + position, path.end(), ext);
							bool fileExists = FileSys.fileExists(path);
							if (fileExists) {
							//	createImage(tmp, timestamp, "NH_LORRI", "", path); /// fix active instrument!
							//	std::sort(tmp.begin(), tmp.end(), imageComparer);
							}
							}*/
						}
					} while (!file.eof() && found == false);
				}
			}
		}
	}
	int pause;
	std::cin >> pause;
}

void LabelParser::createImage(Image& image, double startTime, double stopTime, std::vector<std::string> instr, std::string targ, std::string pot) {
	image.startTime = startTime;
	image.stopTime = stopTime;
	image.path = pot;
	for (int i = 0; i < instr.size(); i++){
		image.activeInstruments.push_back(instr[i]);
	}
	image.target = targ;
	image.projected = false;
}


std::map<std::string, ImageSubset> LabelParser::getSubsetMap(){
	return _subsetMap;
}
std::vector<std::pair<std::string, TimeRange>> LabelParser::getIstrumentTimes(){
	return _instrumentTimes;
}
std::vector<std::pair<double, std::string>> LabelParser::getTargetTimes(){
	return _targetTimes;
}
}