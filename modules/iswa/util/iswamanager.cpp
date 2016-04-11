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
#include <modules/iswa/ext/json/json.hpp>
#include <fstream>

namespace {
    using json = nlohmann::json;
    const std::string _loggerCat = "ISWAManager";
}

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

    std::shared_ptr<ISWACygnet> ISWAManager::createISWACygnet(std::shared_ptr<Metadata> metadata){
        LDEBUG("Creating ISWACygnet with id " << metadata->id);
        if(metadata->path != ""){
            const std::string& extension = ghoul::filesystem::File(absPath(metadata->path)).fileExtension();
            std::shared_ptr<ISWACygnet> cygnet;

            if(extension == "plain"){
                LWARNING("This cygnet image does not exist");
                return nullptr;
            }else if(extension == "cdf"){

                if(!FileSys.fileExists(absPath(metadata->path))) {
                    LERROR("Could not find cdf file:  " << absPath(metadata->path));
                    return nullptr;
                }

                std::shared_ptr<KameleonWrapper> kw = std::make_shared<KameleonWrapper>(absPath(metadata->path));
                auto parentNode = OsEng.renderEngine().scene()->sceneGraphNode(kw->getParent());

                if(parentNode){
                    ghoul::Dictionary metadataDic = 
                    {
                        // {std::string("Name"),        std::string("DataPlane")},
                        {std::string("Type"),       std::string("DataPlane")},
                        {std::string("StartTime"),  std::string("")},
                        {std::string("EndTime"),    std::string("")},
                        {std::string("Id"),         metadata->id},
                        {std::string("Path"),       metadata->path},
                        {std::string("Scale"),      std::make_shared<glm::vec4>(kw->getModelScaleScaled())},
                        {std::string("Offset"),     std::make_shared<glm::vec4>(kw->getModelBarycenterOffsetScaled())},
                        {std::string("Parent"),     kw->getParent()},
                        {std::string("Frame"),      kw->getFrame()},
                        {std::string("KW"),         kw}
                    };

                    
                    ghoul::Dictionary nodeDic = 
                    {
                        {std::string("Name"),       std::string("DataPlane")},
                        {std::string("Parent"),     kw->getParent()},
                        {std::string("Renderable"), metadataDic}
                    };
                    SceneGraphNode* cygnetNode = SceneGraphNode::createFromDictionary(nodeDic);
                    cygnetNode->setParent(parentNode);
                    parentNode->addChild(cygnetNode);
                    OsEng.renderEngine().scene()->addSceneGraphNode(cygnetNode);
                    cygnetNode->initialize();

                }
                // cygnet = std::make_shared<DataPlane>(metadataDic);               
            }else {
                auto parentNode = OsEng.renderEngine().scene()->sceneGraphNode(metadata->parent);
                if(parentNode){
                    ghoul::Dictionary metadataDic = 
                    {
                        // {std::string("Name"),        std::string("TexturePlane")},
                        {std::string("Type"),       std::string("TexturePlane")},   
                        {std::string("StartTime"),  std::string("")},
                        {std::string("EndTime"),    std::string("")},
                        {std::string("Id"),         metadata->id},
                        {std::string("Path"),       metadata->path},
                        {std::string("Frame"),      std::string("GALACTIC")},
                        {std::string("Parent"),     metadata->parent},
                        {std::string("Scale"),      std::make_shared<glm::vec4>(3,3,3,10)},
                        {std::string("Offset"),     std::make_shared<glm::vec4>(0,0,0,1)}
                    };

                    ghoul::Dictionary nodeDic = 
                    {
                        {std::string("Name"),       std::string("TexturePlane")},
                        {std::string("Parent"),     metadata->parent},
                        {std::string("Renderable"), metadataDic}
                    };
                    // SceneGraphNode*

                    SceneGraphNode* cygnetNode = SceneGraphNode::createFromDictionary(nodeDic);
                    cygnetNode->setParent(parentNode);
                    parentNode->addChild(cygnetNode);
                    OsEng.renderEngine().scene()->addSceneGraphNode(cygnetNode);
                    cygnetNode->initialize();

                }else{
                    OsEng.renderEngine().registerScreenSpaceRenderable(std::make_shared<ScreenSpaceCygnet>(metadata));
                    return nullptr;
                } 
            }
            // cygnet->initialize();
            // return cygnet;
        }

        return nullptr;
    }

    void ISWAManager::addISWACygnet(std::string info){
        std::string token;
        std::stringstream ss(info);
        getline(ss,token,',');
        int cygnetId = std::stoi(token);

        getline(ss,token,',');
        std::string data = token;
        addISWACygnet(cygnetId, data);
    }

    void ISWAManager::addISWACygnet(int id, std::string info){
        getDictionaryTable(absPath("${OPENSPACE_DATA}/GM_openspace_X0_info.txt"), id);

        if(id != 0){
            std::shared_ptr<ExtensionFuture> extFuture = fileExtension(id);
            extFuture->parent = info;
            _extFutures.push_back(extFuture);
            // _container->addISWACygnet(cygnetId, data);
        }
        else{
            std::shared_ptr<Metadata> mdata = std::make_shared<Metadata>();
            mdata->id = 0;
            mdata->path = absPath("${OPENSPACE_DATA}/"+info);
            createISWACygnet(mdata);
        }
    }

    void ISWAManager::deleteISWACygnet(std::string name){
        _container->deleteISWACygnet(name);
    }

    std::shared_ptr<DownloadManager::FileFuture> ISWAManager::downloadImage(int id, std::string path){
        return  DlManager.downloadFile(
                    iSWAurl(id),
                    path,
                    true,
                    [path](const DownloadManager::FileFuture& f){
                        LDEBUG("Download finished: " << path);
                    }
                );
    }

    std::shared_ptr<DownloadManager::FileFuture> ISWAManager::downloadImageToMemory(int id, std::string& buffer){
        return  DlManager.downloadToMemory(
                    iSWAurl(id),
                    buffer,
                    [](const DownloadManager::FileFuture& f){
                        LDEBUG("Download to memory finished");
                    }
                );
    }

    void ISWAManager::downloadData(){}

    std::shared_ptr<ExtensionFuture> ISWAManager::fileExtension(int id){
        std::shared_ptr<ExtensionFuture> extFuture = std::make_shared<ExtensionFuture>();
        extFuture->isFinished = false;
        extFuture->id = id;
        DlManager.getFileExtension(
                iSWAurl(id),
                [extFuture](std::string extension){
                    std::stringstream ss(extension);
                    std::string token;
                    std::getline(ss, token, '/');
                    std::getline(ss, token);

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

    void ISWAManager::update(){
        for (auto it = _extFutures.begin(); it != _extFutures.end(); )
        {
            if ((*it)->isFinished) {
                std::string path = "${OPENSPACE_DATA}/scene/iswa/" + std::to_string((*it)->id) + (*it)->extension;
                
                std::shared_ptr<Metadata> data = std::make_shared<Metadata>();
                data->id = (*it)->id;
                data->path = path;
                data->parent = (*it)->parent;

                createISWACygnet(data);
                // if(cygnet){
                //     _iSWACygnets.push_back(cygnet);
                // }
                it = _extFutures.erase( it );
            }
            else {
                ++it;
            }
        }
    }

    std::string ISWAManager::getDictionaryTable(std::string path, int id){
        json j;
        std::ifstream file(path);
        if(file.is_open()){
            j = json::parse(file);
        }

        // std::cout << j << std::endl;

        std::string parent = j["Central Body"];
        int xmax = j["Plot XMAX"];
        int ymax = j["Plot YMAX"];
        int zmax = j["Plot ZMAX"];
        int xmin = j["Plot XMIN"];
        int ymin = j["Plot YMIN"];
        int zmin = j["Plot ZMIN"];

        //If Spatial scale is R_E
        std::string scale = "{" 
                                + std::to_string(6.371f*(xmax-xmin)) + ","
                                + std::to_string(6.371f*(ymax-ymin)) + ","
                                + std::to_string(6.371f*(ymax-ymin)) + ","
                                + std::to_string(6) +
                            "}";

        std::string offset ="{"
                                + std::to_string(6.371f*(xmin + (std::abs(xmin)+std::abs(xmax))/2.0f)) + "," 
                                + std::to_string(6.371f*(ymin + (std::abs(ymin)+std::abs(ymax))/2.0f)) + ","
                                + std::to_string(6.371f*(zmin + (std::abs(zmin)+std::abs(zmax))/2.0f)) + ","
                                + std::to_string(6) +
                            "}";

        // std::cout << scale << std::endl;
        // std::cout << offset << std::endl;

        std::string table = "{"
        "Name : 'TexturePlane' , "
        "Parent : '" + parent + "', "
        "Renderable = {"
            "Type = 'TexturePlane', "
            "Id = " + std::to_string(id) + ", "
            "Frame = 'GALACTIC' , "
            "Scale = " + scale + ", "
            "Offset = " + offset + 
            "}"
        "}"
        ;

        std::cout << table << std::endl;
        // ghoul::Dictionary dic;
        return table;
    }
}// namsepace openspace