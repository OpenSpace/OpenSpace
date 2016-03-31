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
#include <modules/iswa/util/iswamanager.h>
#include <modules/iswa/rendering/iswacygnet.h>
#include <ghoul/filesystem/filesystem>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <modules/iswa/rendering/dataplane.h>
#include <modules/iswa/rendering/textureplane.h>
#include <openspace/util/time.h>

namespace openspace{
	ISWAManager::ISWAManager(){
		_month["JAN"] = "01";
	   	_month["FEB"] = "02";
	   	_month["MAR"] = "03";
	   	_month["APR"] = "04";
	   	_month["MAY"] = "05";
	   	_month["JUN"] = "06";
	   	_month["JUL"] = "07";
	   	_month["AUG"] = "08";
	   	_month["SEP"] = "09";
	   	_month["OCT"] = "10";
	   	_month["NOV"] = "11";
	   	_month["DEC"] = "12";
	}

	ISWAManager::~ISWAManager(){}

	std::shared_ptr<ISWACygnet> ISWAManager::createISWACygnet(std::string path){
		if(FileSys.fileExists(absPath(path))) {
			const std::string& extension = ghoul::filesystem::File(absPath(path)).fileExtension();
			std::shared_ptr<ISWACygnet> cygnet;

			if(extension == "cdf"){
				std::shared_ptr<KameleonWrapper> kw = std::make_shared<KameleonWrapper>(absPath(path));
				cygnet = std::make_shared<DataPlane>(kw, path);
			} else {
				cygnet = std::make_shared<TexturePlane>(path);
			}
			
			cygnet->initialize();
			return cygnet;
			// _iSWACygnets.push_back(cygnet);
		}else{
			std::cout << "file does not exist";
		}
		return nullptr;
	}

	DownloadManager::FileFuture* ISWAManager::downloadImage(int id, std::string path){
		return 	DlManager.downloadFile(
					iSWAurl(id),
					path,
					true,
					[](const DownloadManager::FileFuture& f){
						std::cout<<"download finished"<<std::endl;
					}
				);

	}

	std::string ISWAManager::iSWAurl(int id){
		std::string url = "http://iswa2.ccmc.gsfc.nasa.gov/IswaSystemWebApp/iSWACygnetStreamer?timestamp=";
		std::string t = Time::ref().currentTimeUTC(); 

		std::stringstream ss(t);
		std::string token;

		std::getline(ss, token, ' ');
		url += token + "-"; 
		std::getline(ss, token, ' ');
		url += _month[token] + "-";
		std::getline(ss, token, 'T');
		url += token + "%20";
		std::getline(ss, token, '.');
		url += token;

		url += "&window=-1&cygnetId=";
		url += std::to_string(id);

		std::cout << url <<  std::endl;

		return url;
	}

}// namsepace openspace