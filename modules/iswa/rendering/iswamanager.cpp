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
#include <modules/iswa/rendering/iswamanager.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <ghoul/filesystem/filesystem>

#include <modules/iswa/rendering/dataplane.h>
#include <modules/iswa/rendering/textureplane.h>


namespace openspace{
ISWAManager::ISWAManager(const ghoul::Dictionary& dictionary)
	:Renderable(dictionary)
{
	std::cout << "Created ISWAManager" << std::endl;
}

ISWAManager::~ISWAManager(){
	deinitialize();
}

bool ISWAManager::initialize(){
	std::cout << "Initialized ISWAManager" << std::endl;

	addISWACygnet("${OPENSPACE_DATA}/BATSRUS.cdf");
	// addISWACygnet("${OPENSPACE_DATA}/ENLIL.cdf");
	addISWACygnet("${OPENSPACE_DATA}/test.png");

	return true;
}

bool ISWAManager::deinitialize(){
	for(auto iSWACygnet : _iSWACygnets)
		iSWACygnet->deinitialize();

	return true;
}

bool ISWAManager::isReady() const { return true; }

void ISWAManager::render(const RenderData& data){
	for(auto iSWACygnet : _iSWACygnets){
		if(iSWACygnet->enabled()){
			iSWACygnet->render();
		}
	}
} 

void ISWAManager::update(const UpdateData& data){
	for(auto iSWACygnet : _iSWACygnets)
		iSWACygnet->update();
}

void ISWAManager::addISWACygnet(std::string path){
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
		_iSWACygnets.push_back(cygnet);
	}else{
		std::cout << "file does not exist";
	}
}


std::shared_ptr<ISWACygnet> ISWAManager::iSWACygnet(std::string name){
	for(auto cygnet : _iSWACygnets){
		if(cygnet->name() == name){
			return cygnet;
		}
	}
	return nullptr;
}
}