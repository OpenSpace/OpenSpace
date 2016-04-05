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
#include <modules/iswa/rendering/iswacontainer.h>
#include <modules/iswa/rendering/screenspacecygnet.h>

namespace openspace{
	ISWAManager::ISWAManager()
		:_container(nullptr)
	{
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

	void ISWAManager::addCygnet(std::string info){
		std::string token;
		std::stringstream ss(info);
		getline(ss,token,',');
		int cygnetId = std::stoi(token);

		getline(ss,token,',');
		std::string data = token;

		if(cygnetId != 0)
			_container->addISWACygnet(cygnetId, data);
		else
			_container->addISWACygnet("${OPENSPACE_DATA}/"+data);
	}	

	std::shared_ptr<ISWACygnet> ISWAManager::createISWACygnet(std::shared_ptr<Metadata> metadata){
		std::cout << "createISWACygnet " << metadata->id << std::endl;
		if(metadata->path != ""){
			const std::string& extension = ghoul::filesystem::File(absPath(metadata->path)).fileExtension();
			std::shared_ptr<ISWACygnet> cygnet;

			if(extension == "cdf"){
				std::shared_ptr<KameleonWrapper> kw = std::make_shared<KameleonWrapper>(absPath(metadata->path));

				metadata->scale  = kw->getModelScaleScaled();
				metadata->offset = kw->getModelBarycenterOffsetScaled();
				metadata->parent = kw->getParent();
				metadata->frame  = kw->getFrame();

				cygnet = std::make_shared<DataPlane>(kw, metadata);
			}else {
				auto node = OsEng.renderEngine().scene()->sceneGraphNode(metadata->parent);
				if(node){
					metadata->scale = glm::vec4(3, 3, 3, 10);
					metadata->offset = glm::vec4(0, 0, 0, 1);
					metadata->frame  = "GALACTIC";

					cygnet = std::make_shared<TexturePlane>(metadata);
				}else{
					OsEng.renderEngine().registerScreenSpaceRenderable(std::make_shared<ScreenSpaceCygnet>(metadata));
					return nullptr;
				} 
			}

			cygnet->initialize();
			return cygnet;
		} else {
			return nullptr;
		}
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

	void ISWAManager::downloadData(){}

	std::shared_ptr<ExtensionFuture> ISWAManager::fileExtension(int id){

		std::shared_ptr<ExtensionFuture> extFuture = std::make_shared<ExtensionFuture>();
		extFuture->isFinished = false;
		extFuture->id = id;
		std::cout << "extension id: "<< id << std::endl;
		DlManager.getFileExtension(
				iSWAurl(id),
				[extFuture](std::string extension){
					std::stringstream ss(extension);
					std::string token;
					std::getline(ss, token, '/');
					std::getline(ss, token);

					if(token == "jpeg"){
						token = "jpg";
					}

					std::string ext = "."+token;
					extFuture->extension = ext;
					extFuture->isFinished = true;
				}
			);

		return extFuture;
	}

	void ISWAManager::setContainer(ISWAContainer* container){
		_container = container;
	}

	std::shared_ptr<ISWACygnet> ISWAManager::iSWACygnet(std::string name){
		if(_container)
			return _container->iSWACygnet(name);
		return nullptr;
	}

	void ISWAManager::deleteCygnet(std::string name){
		_container->deleteCygnet(name);
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

		//std::cout << url <<  std::endl;

		return url;
	}

}// namsepace openspace