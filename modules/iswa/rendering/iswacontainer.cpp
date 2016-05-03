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
#include <modules/iswa/ext/json/json.hpp>


namespace {
    using json = nlohmann::json;
    const std::string _loggerCat = "ISWAContainer";
}

namespace openspace{

ISWAContainer::ISWAContainer(const ghoul::Dictionary& dictionary)
    :Renderable(dictionary)
{
    // std::string textureCygnets;
    // std::string dataCygnets;
    // dictionary.getValue("TextureCygnets", textureCygnets);
    // dictionary.getValue("DataCygnets", dataCygnets);
    // std::cout << textureCygnets << std::endl;
    // std::cout << dataCygnets << std::endl;
    
    // if(textureCygnets != ""){
    //     json j = json::parse(textureCygnets);
    //     for (auto& id : j) {
    //         ISWAManager::ref().addISWACygnet(id, "TEXTURE");
    //     }
    // }
    
    // if(dataCygnets != ""){
    //     json j = json::parse(dataCygnets);
    //     for (auto& id : j) {
    //         ISWAManager::ref().addISWACygnet(id, "DATA");
    //     }
    // }
}

ISWAContainer::~ISWAContainer(){}
bool ISWAContainer::initialize(){return true;}
bool ISWAContainer::deinitialize(){return true;}
bool ISWAContainer::isReady() const { return true; }
void ISWAContainer::render(const RenderData& data){} 

void ISWAContainer::update(const UpdateData& data){}

}