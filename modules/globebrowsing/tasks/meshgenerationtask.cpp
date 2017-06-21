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

#include <openspace/documentation/verifier.h>

#include <modules/base/rendering/modelgeometry.h>
#include <fstream>
#include <algorithm>
#include <urlmon.h>
#pragma comment(lib, "urlmon.lib")

#include <modules/globebrowsing/tasks/meshgeneration.h>
#include <modules/globebrowsing/tasks/collapsedmeshgeneration.h>

namespace {
	const std::string _loggerCat = "MeshgenerationTask";
	const std::string _solname = "Name";
	const std::string _inputPath = "InputPath";
	const std::string _level = "Level";
	const std::string _urlToPDS = "https://pds-imaging.jpl.nasa.gov/data/msl/MSLNAV_1XXX/DATA/";
}

namespace openspace {
namespace globebrowsing {
	MeshGenerationTask::MeshGenerationTask(const ghoul::Dictionary & dictionary) {
		dictionary.getValue(_solname, _inputSol);
		dictionary.getValue(_inputPath, _rootPath);
		dictionary.getValue(_level, _levelOfDetail);
		dictionary.getValue("GenerationParameters", _generationParams);
		dictionary.getValue("OutputPath", _pathToModelsFolder);

		_levelOfDetail = "level" + _levelOfDetail;

		ghoul::Dictionary filterParams;

		_generationParams.getValue("Filters", filterParams);

		std::transform(_inputSol.begin(), _inputSol.end(), _inputSol.begin(), ::tolower);

		_inputTxt = _rootPath + _inputSol + "/" + _inputSol + ".txt";
		
		_filenames = readFilenames(_inputTxt);
	}

	std::string MeshGenerationTask::description() {
		return "desc";
	}

	void MeshGenerationTask::perform(const Task::ProgressCallback &progressCallback) {
		
		if (_filenames.empty()) {
			LERROR("FILENAMES IS EMPTY");
			return;
		}

		const clock_t begin_time = clock();
		progressCallback(0.0f);
		int k = 0;
		ghoul::Dictionary extensions;
		std::string extension;
		std::string solNr = _inputSol.substr(3,_inputSol.size());
		// NASA changed the fileformat from jpg to png around 450 sols
		extension = std::stoi(solNr) > 450 ? ".png" : ".jpg";

		extensions.setValue("TextureExtension", extension);
		extensions.setValue("ModelFile", ".obj");
		extensions.setValue("BinaryFile", ".IMG");

		int counter = 0;
		ghoul::Dictionary allSites;
		int counter2 = 0;

		//Loop to fill dictionary allSites with structure:
		// Mesh0 = {
		//	Level = "1",
		//	OutputFile = "solxxxxxx.obj",
		//  InputFilename = "binaryfiles/NLB_..",
		//	PointCloud0 = "NLB_..",
		//	PointCloud1 = "NLB_..",
		// },
		// Mesh1 = {
		//	Level = "1",
		//	OutputFile = "solxxxxxx.obj",
		//  InputFilename = "binaryfiles/NLB_..",
		//	PointCloud0 = "NLB_..",
		//	PointCloud1 = "NLB_..",
		// },

		for (auto f : _filenames) {
			//Iterate over all binaries in txt in SOLxxxx folder
			bool Exists = false;
			for (size_t i = 0; i < allSites.size(); ++i) {
				//Iterate over all existing meshes inside dictionary
				std::string temp;
				ghoul::Dictionary existingMesh;
				allSites.getValue("Mesh" + std::to_string(i), existingMesh);
				existingMesh.getValue("OutputFile", temp);

				if (temp == f.outputFilename) {
					// If the Output filenamem, in the already existing entry,
					// is the same as the outputFilename, this means that we should add
					// this one too already existing
					ghoul::Dictionary newPointCloud;
					std::string newEntry = "PointCloud" + std::to_string(existingMesh.size());

					existingMesh.setValue(newEntry, f.filename);
					allSites.setValue("Mesh" + std::to_string(i), existingMesh);
					Exists = true;
				}
			}

			if (!Exists) {
				ghoul::Dictionary newMesh;
				newMesh.setValue("Level", _levelOfDetail);
				newMesh.setValue("OutputFile", f.outputFilename);
				newMesh.setValue("InputFilename", f.filename);
				newMesh.setValue("BinaryPath", _rootPath + _inputSol + "/");
				newMesh.setValue("OutputPath", _pathToModelsFolder);
				newMesh.setValue("Filters", _generationParams);
				ghoul::Dictionary firstPointCloud;
				std::string testare = f.filename;
				newMesh.setValue("PointCloud0", testare);

				std::string newMeshStr = "Mesh" + std::to_string(counter2);

				allSites.setValue(newMeshStr, newMesh);
				counter2++;
			}

		}

		for (size_t i = 0; i < allSites.size(); ++i) {
			//Iterate over all sites again, now ordered as above,

			std::string temp = "nothing";
			ghoul::Dictionary inputDic;
			allSites.getValue("Mesh" + std::to_string(i), inputDic);
			inputDic.getValue("OutputFile", temp);
			LERROR(temp);
			inputDic.getValue("InputFilename", temp);
			LERROR(temp);
			inputDic.setValue("Filters", _generationParams);

			//Generate mesh for each point cloud
			CollapsedMeshGeneration::generateMeshFromBinary(inputDic);

			k++;
			float test = k / _filenames.size();
			progressCallback(test);
		}

		progressCallback(1.0f);
		LINFO("FINISHED IN : " << float(clock() - begin_time) / CLOCKS_PER_SEC);
	}

	std::vector<MeshGenerationTask::File> MeshGenerationTask::readFilenames(const std::string filename) {
		ghoul_assert(!filename.empty(), "Filename not present");

		std::vector<File> filenames;
		std::ifstream fs(filename.c_str());
		std::string line;
		if (!fs.is_open()) {
			//throw std::runtime_error(std::string("File " + filename + " could not be opened"));
			return filenames;
		}
		else {
			while (std::getline(fs, line)) {
				std::string outputFilename = extractOutputFilename(line);

				File f;
				f.outputFilename = outputFilename;
				f.filename = line;
				filenames.push_back(f);
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

	std::string MeshGenerationTask::extractOutputFilename(std::string filename) {

		// Create filename without path and extension
		std::string fileNameStripped = filename.substr(filename.find_last_of("_F") + 1, 7);

		std::string siteNumberString = fileNameStripped.substr(0, 3);
		std::string driveNumberString = fileNameStripped.substr(3, 7);

		return _inputSol + siteNumberString + driveNumberString;
	}
}
}
