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

#include <ghoul\logging\logmanager.h>
#include <ghoul\filesystem\filesystem.h>
#include <modules\globebrowsing\tasks\meshgenerationtask.h>

#include <modules/base/rendering/modelgeometry.h>
#include <fstream>
#include <algorithm>
#include <urlmon.h>
#pragma comment(lib, "urlmon.lib")

#include <modules/globebrowsing/tasks/meshgeneration.h>

namespace {
	const std::string _loggerCat = "Meshgeneration";
	const std::string _solname = "Name";
	const std::string _inputPath = "InputPath";
	const std::string _urlToPDS = "https://pds-imaging.jpl.nasa.gov/data/msl/MSLNAV_1XXX/DATA/";
}

namespace openspace {
namespace globebrowsing {
	MeshGenerationTask::MeshGenerationTask(const ghoul::Dictionary & dictionary) {
		ghoul_assert(dictionary.getValue(_solname, _inputSol), "Need to specify Name");
		ghoul_assert(dictionary.getValue(_inputPath, _root), "Need to specify path to models");

		std::transform(_inputSol.begin(), _inputSol.end(), _inputSol.begin(), ::tolower);

		_inputTxt = _root + "/binaryfiles/" + _inputSol + "/" + _inputSol + ".txt";
			
		_filenames = readFilenames(_inputTxt);
		
		_outputPath = _root + "/models/" + _inputSol;

	}

	std::string MeshGenerationTask::description() {
		return "DESCRIPTION";
	}

	void MeshGenerationTask::perform(const Task::ProgressCallback &progressCallback) {
		const clock_t begin_time = clock();
		progressCallback(0.0f);
		int k = 0;
		for (auto f : _filenames) {
			std::string binary_path = _root + "/binaryfiles/" + _inputSol + "/" + f + ".IMG";
			std::string output_path = _root + "models/";
			
			if (!FileSys.fileExists(binary_path)) {
				LINFO("Downloading file " << f);

				downloadBinaryFile(f, binary_path);
			}
			else {
				LINFO("File found locally, using that one instead: " << f);
			}

			MeshGeneration::generateMeshFromBinary(binary_path, output_path);

			//modelgeometry::ModelGeometry::createFromDictionary(model_dic);
			k++;
			float test = k / _filenames.size();
			progressCallback(test);
		}
		progressCallback(1.0f);
		LINFO("FINISHED IN : " << float(clock() - begin_time) / CLOCKS_PER_SEC);
	}

	std::vector<std::string> MeshGenerationTask::readFilenames(const std::string filename) {
		ghoul_assert(!filename.empty(), "Filename not present");
		std::vector<std::string> filenames;
		std::ifstream fs(filename.c_str());
		std::string line;
		if (!fs.is_open()) {
			throw std::runtime_error(std::string("File " + filename + " could not be opened"));
		}
		else {
			while (std::getline(fs, line)) {
				filenames.push_back(line);
			}
		}
		return filenames;
	}

	bool MeshGenerationTask::downloadBinaryFile(std::string filename, std::string binary_path) {
		std::string upper_case_solname = _inputSol;

		std::transform(upper_case_solname.begin(), upper_case_solname.end(), upper_case_solname.begin(), ::toupper);

		std::string url_with_filename = _urlToPDS + upper_case_solname + "/" + filename + ".IMG";
		
		TCHAR *url = new TCHAR[url_with_filename.size() + 1];
		url[url_with_filename.size()] = 0;
		std::copy(url_with_filename.begin(), url_with_filename.end(), url);

		TCHAR *param = new TCHAR[binary_path.size() + 1];
		param[binary_path.size()] = 0;
		std::copy(binary_path.begin(), binary_path.end(), param);

		HRESULT hr = URLDownloadToFile(
			NULL,   // A pointer to the controlling IUnknown interface (not needed here)
			url,
			param,
			0,      // Reserved. Must be set to 0.
			NULL); // status callback interface (not needed for basic use)

		ghoul_assert(SUCCEEDED(hr), "The file " + filename + " was not found locally and could not be downloaded from url: " + url_with_filename);

		return SUCCEEDED(hr);
	}
}
}
