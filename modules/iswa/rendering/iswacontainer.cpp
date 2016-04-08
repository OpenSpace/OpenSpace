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
#include <modules/iswa/rendering/iswacontainer.h>
#include <ghoul/filesystem/filesystem>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <modules/iswa/rendering/dataplane.h>
#include <modules/iswa/rendering/textureplane.h>
#include <modules/iswa/rendering/screenspacecygnet.h>
#include <modules/iswa/util/iswamanager.h>
#include <openspace/rendering/renderable.h>

namespace openspace{

ISWAContainer::ISWAContainer(const ghoul::Dictionary& dictionary)
	:Renderable(dictionary)
{}

ISWAContainer::~ISWAContainer(){}

bool ISWAContainer::initialize(){
	ISWAManager::initialize();
	ISWAManager::ref().setContainer(this);

	// ISWAManager::ref().addISWACygnet(0, "BATSRUS.cdf");
	ISWAManager::ref().addISWACygnet(5, "Screen");
	ISWAManager::ref().addISWACygnet(7, "Sun");

	return true;
}

bool ISWAContainer::deinitialize(){
	for(auto iSWACygnet : _iSWACygnets)
		iSWACygnet->deinitialize();

	return true;
}

bool ISWAContainer::isReady() const { return true; }

void ISWAContainer::render(const RenderData& data){
	// for(auto iSWACygnet : _iSWACygnets){
	// 	if(iSWACygnet->isEnabled() && iSWACygnet->isReady()){
	// 		iSWACygnet->render(data);
	// 	}
	// }
} 

void ISWAContainer::update(const UpdateData& data){

	// // Create iSWAcygnets if a ExtensionFuture in _extFutures is finished loading.
	// // Then remove it from the _extFutures vector.
	// for (auto it = _extFutures.begin(); it != _extFutures.end(); )
	// {
	// 	if ((*it)->isFinished) {
	// 		std::string path = "${OPENSPACE_DATA}/scene/iswa/" + std::to_string((*it)->id) + (*it)->extension;
			
	// 		std::shared_ptr<Metadata> data = std::make_shared<Metadata>();
	// 		data->id = (*it)->id;
	// 		data->path = path;
	// 		data->parent = (*it)->parent;

	// 		std::shared_ptr<ISWACygnet> cygnet = ISWAManager::ref().createISWACygnet(data);
	// 		if(cygnet){
	// 			_iSWACygnets.push_back(cygnet);
	// 		}
	// 		it = _extFutures.erase( it );
	// 	}
	//     else {
	//     	++it;
	//     }
	// }

	// if(!_deletedCygnets.empty())
	// 	_deletedCygnets.clear();

	// for(auto& iSWACygnet : _iSWACygnets)
	// 	iSWACygnet->update(data);
}

void ISWAContainer::addISWACygnet(std::string path){
	// std::shared_ptr<Metadata> data = std::make_shared<Metadata>();
	// data->id = 0;
	// data->path = path;

	// std::shared_ptr<ISWACygnet> cygnet = ISWAManager::ref().createISWACygnet(data);
	// if(cygnet){
	// 	_iSWACygnets.push_back(cygnet);
	// }

}

void ISWAContainer::addISWACygnet(int id, std::string data){
	// std::shared_ptr<ExtensionFuture> extFuture = ISWAManager::ref().fileExtension(id);
	// extFuture->parent = data;
	// _extFutures.push_back(extFuture);
}

void ISWAContainer::addISWACygnet(std::shared_ptr<ISWACygnet> cygnet){
	// if(cygnet){
	// 	_iSWACygnets.push_back(cygnet);
	// }
}

void ISWAContainer::deleteISWACygnet(std::string name){
	// std::shared_ptr<ISWACygnet> c = iSWACygnet(name);

	// auto it = std::find(
	// 	_iSWACygnets.begin(),
	// 	_iSWACygnets.end(),
	// 	c
	// );

	// if (it != _iSWACygnets.end()) {
	// 	c->deinitialize();
	// 	_deletedCygnets.push_back(c);
	// 	_iSWACygnets.erase(it);
	// }
}

std::shared_ptr<ISWACygnet> ISWAContainer::iSWACygnet(std::string name){
	for(auto cygnet : _iSWACygnets){
		if(cygnet->name() == name){
			return cygnet;
		}
	}
	return nullptr;
}
}