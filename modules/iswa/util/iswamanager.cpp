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

    void ISWAManager::addISWACygnet(std::string info){
        std::string token;
        std::stringstream ss(info);
        getline(ss,token,',');
        int cygnetId = std::stoi(token);
        
        if(!ss.eof()){
            getline(ss,token,',');
            std::string data = token;
            addISWACygnet(cygnetId, data);
        } else{
            addISWACygnet(cygnetId);
        }
        /*if(data == "")
        else*/
            
    }

    void ISWAManager::addISWACygnet(int id, std::string info){
        if(id > 0){
            createScreenSpace(id);
        }else if(id < 0){
            //download metadata to texture plane
            std::shared_ptr<MetadataFuture> metaFuture = downloadMetadata(id);
            metaFuture->type = info;
            metaFuture->id = id;
            _metaFutures.push_back(metaFuture);
        }
    }

    void ISWAManager::deleteISWACygnet(std::string name){
        OsEng.scriptEngine().queueScript("openspace.removeSceneGraphNode('" + name + "')");
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
                iSWAurl(id, "image"),
                buffer,
                [](const DownloadManager::FileFuture& f){
                    LDEBUG("Download to memory finished");
                }
            );
    }

    std::shared_ptr<DownloadManager::FileFuture> ISWAManager::downloadDataToMemory(int id, std::string& buffer){
        return DlManager.downloadToMemory(
                iSWAurl(id, "data"),
                buffer,
                [](const DownloadManager::FileFuture& f){
                    LDEBUG("Download data finished");
                }
            );
    }

    std::shared_ptr<MetadataFuture> ISWAManager::downloadMetadata(int id){
        std::shared_ptr<MetadataFuture> metaFuture = std::make_shared<MetadataFuture>();

        metaFuture->id = id;
        
        DlManager.downloadToMemory(
                    "http://128.183.168.116:3000/" + std::to_string(-id),
                    metaFuture->json,
                    [metaFuture](const DownloadManager::FileFuture& f){
                        LDEBUG("Download to memory finished");
                        metaFuture->isFinished = true;
                    }
                );
        return metaFuture;
    }

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

    std::string ISWAManager::iSWAurl(int id, std::string type){
        std::string url;
        if(id < 0){
            url = "http://128.183.168.116:3000/"+type+"/" + std::to_string(-id) + "/";
        } else{
            url = "http://iswa2.ccmc.gsfc.nasa.gov/IswaSystemWebApp/iSWACygnetStreamer?window=-1&cygnetId="+ std::to_string(id) +"&timestamp=";
        }        

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

        return url;
    }

    void ISWAManager::update(){
        for (auto it = _metaFutures.begin(); it != _metaFutures.end(); ){
            if((*it)->isFinished) {
                if((*it)->type == "TEXTURE"){
                    createPlane((*it)->id,(*it)->json,std::string("TexturePlane"));
                }else if ((*it)->type == "DATA"){
                    createPlane((*it)->id,(*it)->json,std::string("DataPlane"));
                } else {
                    LERROR("\""+ (*it)->type + "\" is not a valid type");
                }
                it = _metaFutures.erase( it );
            }else{
                ++it;
            }
        }    
    }

    std::string ISWAManager::parseJSONToLuaTable(int id, std::string jsonString, std::string type){
        if(jsonString != ""){
            json j = json::parse(jsonString);

            std::string parent = j["Central Body"];
            std::string frame = j["Coordinates"];
            int updateTime = j["ISWA_UPDATE_SECONDS"];

            glm::vec3 max(
                j["Plot XMAX"],
                j["Plot YMAX"],
                j["Plot ZMAX"]
            );

            glm::vec3 min(
                j["Plot XMIN"],
                j["Plot YMIN"],
                j["Plot ZMIN"]
            );

            glm::vec2 spatialScale(1, 10);
            std::string spatial = j["Spatial Scale (Custom)"];
            if(spatial == "R_E"){
                spatialScale.x = 6.371f;
                spatialScale.y = 6;
            }

            std::string table = "{"
            "Name = '" + type + std::to_string(id) +"' , "
            "Parent = '" + parent + "', "
            "Renderable = {"    
                "Type = '" + type + "', "
                "Id = " + std::to_string(id) + ", "
                "Frame = '" + frame + "' , "
                "Min = " + std::to_string(min) + ", "
                "Max = " + std::to_string(max) + ", "
                "SpatialScale = " + std::to_string(spatialScale) + ", "
                "UpdateTime = " + std::to_string(updateTime) + ", "
                "}"
            "}"
            ;

            // std::cout << table << std::endl;
            return table;
        }
        return "";
    }

    // std::string ISWAManager::parseKWToLuaTable(std::string kwPath){
    //     //NEED TO REWRITE IF USED AGAIN
    //     if(kwPath != ""){
    //         const std::string& extension = ghoul::filesystem::File(absPath(kwPath)).fileExtension();
    //         if(extension == "cdf"){
    //             KameleonWrapper kw = KameleonWrapper(absPath(kwPath));
         
    //             std::string parent  = kw.getParent();
    //             std::string frame   = kw.getFrame();
    //             glm::vec4 scale     = kw.getModelScaleScaled();
    //             glm::vec4 offset    = kw.getModelBarycenterOffsetScaled();

    //             std::string table = "{"
    //                 "Name = 'DataPlane',"
    //                 "Parent = '" + parent + "', "
    //                 "Renderable = {"    
    //                     "Type = 'DataPlane', "
    //                     "Id = 0 ,"
    //                     "Frame = '" + frame + "' , "
    //                     "Scale = " + std::to_string(scale) + ", "
    //                     "Offset = " + std::to_string(offset) + ", "
    //                     "kwPath = '" + kwPath + "'" 
    //                     "}"
    //                 "}"
    //                 ;
    //             // std::cout << table << std::endl;    
    //             return table;
    //         }
    //     }
    //     return "";
    // }

    //Create KameleonPlane?
    // void ISWAManager::createDataPlane(std::string kwPath){
    //     std::string luaTable = parseKWToLuaTable(kwPath);
    //     if(luaTable != ""){
    //         std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
    //         OsEng.scriptEngine().queueScript(script);
    //     }
    // }


    // void ISWAManager::createTexturePlane(int id, std::string json){
    //     std::string luaTable = parseJSONToLuaTable(id, json);
    //     if(luaTable != ""){
    //         std::string script = "openspace.addSceneGraphNode(" + parseJSONToLuaTable(id, json) + ");";
    //         OsEng.scriptEngine().queueScript(script);
    //     }
    // }

    void ISWAManager::createPlane(int id, std::string json, std::string type){

        // check if this plane already exist
        std::string name = type + std::to_string(id); 
        if( OsEng.renderEngine().scene()->sceneGraphNode(name) ){
            LERROR("A node with name \"" + name +"\" already exist");
            return;
        }

        std::string luaTable = parseJSONToLuaTable(id, json, type);
        if(luaTable != ""){
            std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
            OsEng.scriptEngine().queueScript(script);
        }
    }

    void ISWAManager::createScreenSpace(int id){
        OsEng.renderEngine().registerScreenSpaceRenderable(std::make_shared<ScreenSpaceCygnet>(id));
    }
}// namsepace openspace