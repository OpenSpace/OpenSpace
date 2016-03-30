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
	std::cout << "Created datasurface container" << std::endl;
}

ISWAManager::~ISWAManager(){
	deinitialize();
}

bool ISWAManager::initialize(){
	std::cout << "Initialized datasurface container" << std::endl;

	addDataSurface("${OPENSPACE_DATA}/BATSRUS.cdf");
	// addDataSurface("${OPENSPACE_DATA}/ENLIL.cdf");
	addTextureSurface("${OPENSPACE_DATA}/test.png");
}
bool ISWAManager::deinitialize(){
	for(dataSurface : _dataSurfaces)
		dataSurface->deinitialize();
}
bool ISWAManager::isReady() const {}

void ISWAManager::render(const RenderData& data){
	for(dataSurface : _dataSurfaces){
		if(dataSurface->enabled()){
			dataSurface->render();
		}
	}
} 

void ISWAManager::update(const UpdateData& data){
	for(dataSurface : _dataSurfaces)
		dataSurface->update();
}

void ISWAManager::addDataSurface(std::string path){
	if(FileSys.fileExists(absPath(path))) {
		std::shared_ptr<ISWACygnet> ds;
		std::shared_ptr<KameleonWrapper> kw = std::make_shared<KameleonWrapper>(absPath(path));

		//find out what class to create
		ds = std::make_shared<DataPlane>(kw, path);


		ds->initialize();
		_dataSurfaces.push_back(ds);
	}else{
		std::cout << "file does not exist";
	}
}

void ISWAManager::addTextureSurface(std::string path){
	if(FileSys.fileExists(absPath(path))) {
		std::shared_ptr<ISWACygnet> ts;

		//find out what class to create
		ts = std::make_shared<TexturePlane>(path);
		std::cout<<"before initialize"<<std::endl;
		ts->initialize();
		_dataSurfaces.push_back(ts);
	}else{
		std::cout << "file does not exist";
	}
}

std::shared_ptr<ISWACygnet> ISWAManager::dataSurface(std::string name){
	for(auto ds : _dataSurfaces){
		if(ds->name() == name){
			return ds;
		}
	}
	return nullptr;
}
}