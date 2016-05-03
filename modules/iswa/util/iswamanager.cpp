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

#include <modules/iswa/rendering/dataplane.h>
#include <modules/iswa/rendering/textureplane.h>
#include <modules/iswa/rendering/datasphere.h>
#include <modules/iswa/rendering/screenspacecygnet.h>
#include <modules/iswa/rendering/iswacygnet.h>
#include <modules/iswa/rendering/iswagroup.h>

#include <fstream>
#include <ghoul/filesystem/filesystem>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/util/time.h>
#include <modules/iswa/ext/json/json.hpp>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scripting/script_helper.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>

#include "iswamanager_lua.inl";

namespace {
    using json = nlohmann::json;
    const std::string _loggerCat = "ISWAManager";
}

namespace openspace{

ISWAManager::ISWAManager()
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

    _type[CygnetType::Texture] = "Texture";
    _type[CygnetType::Data] = "Data";
    _type[CygnetType::Kameleon] = "Kameleon";

    _geom[CygnetGeometry::Plane] = "Plane";
    _geom[CygnetGeometry::Sphere] = "Sphere";
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
        
        if(!ss.eof()){
            getline(ss, token, ',');
            int group = std::stoi(token);
            addISWACygnet(cygnetId, data, group);
            return;
        }  

        addISWACygnet(cygnetId, data);
        return;
    }

    addISWACygnet(cygnetId);
    
    /*if(data == "")
    else*/
        
}

void ISWAManager::addISWACygnet(int id, std::string info, int group){
    if(id > 0){
        createScreenSpace(id);
    }else if(id < 0){
        std::shared_ptr<MetadataFuture> metaFuture = std::make_shared<MetadataFuture>();
        metaFuture->id = id;
        metaFuture->group = group;
        if(info == _type[CygnetType::Texture]){
            metaFuture->type = CygnetType::Texture;
            metaFuture->geom  = CygnetGeometry::Plane;
        } else if (info  == _type[CygnetType::Data]) {
            metaFuture->type = CygnetType::Data;
            metaFuture->geom  = CygnetGeometry::Plane;
        } else {
            LERROR("\""+ info + "\" is not a valid type");
            return;
        }

        auto metadataCallback = 
        [this, metaFuture](const DownloadManager::FileFuture& f){
            LDEBUG("Download to memory finished");
            metaFuture->isFinished = true;
            createPlane(metaFuture);
        };

        // Download metadata
        DlManager.downloadToMemory(
            "http://128.183.168.116:3000/" + std::to_string(-id),
            // "http://10.0.0.76:3000/" + std::to_string(-id),
            metaFuture->json,
            metadataCallback
        );
    }else{ 
        //create kameleonplane
        createKameleonPlane(info);
    } 
}

void ISWAManager::deleteISWACygnet(std::string name){
    OsEng.scriptEngine().queueScript("openspace.removeSceneGraphNode('" + name + "')");
}

// std::shared_ptr<DownloadManager::FileFuture> ISWAManager::downloadImage(int id, std::string path){
//     return  DlManager.downloadFile(
//                 iSWAurl(id),
//                 path,
//                 true,
//                 [path](const DownloadManager::FileFuture& f){
//                     LDEBUG("Download finished: " << path);
//                 }
//             );
// }

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


std::string ISWAManager::iSWAurl(int id, std::string type){
    std::string url;
    if(id < 0){
        url = "http://128.183.168.116:3000/"+type+"/" + std::to_string(-id) + "/";
        // url = "http://10.0.0.76:3000/"+type+"/" + std::to_string(-id) + "/";
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

std::shared_ptr<MetadataFuture> ISWAManager::downloadMetadata(int id){
    std::shared_ptr<MetadataFuture> metaFuture = std::make_shared<MetadataFuture>();

    metaFuture->id = id;
    DlManager.downloadToMemory(
                "http://128.183.168.116:3000/" + std::to_string(-id),
                // "http://10.0.0.76:3000/" + std::to_string(-id),
                metaFuture->json,
                [metaFuture](const DownloadManager::FileFuture& f){
                    LDEBUG("Download to memory finished");
                    metaFuture->isFinished = true;
                }
            );
    return metaFuture;
}

void ISWAManager::createScreenSpace(int id){
    std::string luaTable = "{ Type='ScreenSpaceCygnet', CygnetId = "+std::to_string(id)+"}";
    std::string script = "openspace.registerScreenSpaceRenderable(" + luaTable + ");";
    OsEng.scriptEngine().queueScript(script);
}

void ISWAManager::createPlane(std::shared_ptr<MetadataFuture> data){
    // check if this plane already exist
    std::string name = _type[data->type] + _geom[data->geom] + std::to_string(data->id); 
    if(data->group > 0)
        name += "_Group" + std::to_string(data->group);

    data->name = name;

    if( OsEng.renderEngine().scene()->sceneGraphNode(name) ){
        LERROR("A node with name \"" + name +"\" already exist");
        return;
    }

    std::string luaTable = parseJSONToLuaTable(data);
    if(luaTable != ""){
        std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
        OsEng.scriptEngine().queueScript(script);
    }
}

std::string ISWAManager::parseJSONToLuaTable(std::shared_ptr<MetadataFuture> data){
    if(data->json != ""){
        json j = json::parse(data->json);

        std::string parent = j["Central Body"];
        std::string frame = j["Coordinates"];
        std::string coordinateType = j["Coordinate Type"];
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

        glm::vec4 spatialScale(1, 1, 1, 10);
        std::string spatial = j["Spatial Scale (Custom)"];
        if(spatial == "R_E"){
            spatialScale.x = 6.371f;
            spatialScale.y = 6.371f;
            spatialScale.z = 6.371f;
            spatialScale.w = 6;
        }


        std::string table = "{"
        "Name = '" + data->name +"' , "
        "Parent = '" + parent + "', "
        "Renderable = {"    
            "Type = '" + _type[data->type] + _geom[data->geom] + "', "
            "Id = " + std::to_string(data->id) + ", "
            "Frame = '" + frame + "' , "
            "GridMin = " + std::to_string(min) + ", "
            "GridMax = " + std::to_string(max) + ", "
            "SpatialScale = " + std::to_string(spatialScale) + ", "
            "UpdateTime = " + std::to_string(updateTime) + ", "
            "CoordinateType = '" + coordinateType + "', "
            "Group = "+ std::to_string(data->group) + " ,"
            "}"
        "}";
        
        return table;
    }
    return "";
}

void ISWAManager::createKameleonPlane(std::string kwPath, int group){
    kwPath = "${OPENSPACE_DATA}/" + kwPath;
    const std::string& extension = ghoul::filesystem::File(absPath(kwPath)).fileExtension();

    if(FileSys.fileExists(absPath(kwPath)) && extension == "cdf"){
        std::string luaTable = parseKWToLuaTable(kwPath, group);
        if(!luaTable.empty()){
            std::cout << luaTable << std::endl;
            std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
            OsEng.scriptEngine().queueScript(script);
        }
    }else{
        LWARNING( kwPath + " is not a cdf file or can't be found.");
    }
}

std::string ISWAManager::parseKWToLuaTable(std::string kwPath, int group){
    if(kwPath != ""){
        const std::string& extension = ghoul::filesystem::File(absPath(kwPath)).fileExtension();
        if(extension == "cdf"){
            KameleonWrapper kw = KameleonWrapper(absPath(kwPath));
     
            std::string parent  = kw.getParent();
            std::string frame   = kw.getFrame();
            glm::vec3   min     = kw.getGridMin();
            glm::vec3   max     = kw.getGridMax();

            glm::vec4 spatialScale;
            std::string coordinateType;

            std::tuple < std::string, std::string, std::string > gridUnits = kw.getGridUnits();
            if (std::get<0>(gridUnits) == "R" && std::get<1>(gridUnits) == "R" && std::get<2>(gridUnits) == "R") {
                spatialScale.x = 6.371f;
                spatialScale.y = 6.371f;
                spatialScale.z = 6.371f;
                spatialScale.w = 6;

                coordinateType = "Cartesian";
            }else{
                spatialScale = glm::vec4(1.0);
                spatialScale.w = 1; //-log10(1.0f/max.x);

                coordinateType = "Polar";
            }
            std::string table = "{"
                "Name = 'KameleonPlane0',"
                // "Parent = 'Earth', "
                "Parent = '" + parent + "', " 
                "Renderable = {"    
                    "Type = 'KameleonPlane', "
                    "Id = 0 ,"
                    "Frame = '" + frame + "' , "
                    "GridMin = " + std::to_string(min) + ", "
                    "GridMax = " + std::to_string(max) + ", "
                    "SpatialScale = " + std::to_string(spatialScale) + ", "
                    "UpdateTime = 0, "
                    "kwPath = '" + kwPath + "' ," 
                    "axisCut = 'y' ,"
                    "CoordinateType = '" + coordinateType + "', "
                    "Group = "+ std::to_string(group) + " ,"
                    "}"
                "}"
                ;
            // std::cout << table << std::endl;    
            return table;
        }
    }
    return "";
}

void ISWAManager::registerToGroup(int id, ISWACygnet* cygnet, CygnetType type){
    if(_groups.find(id) != _groups.end()){
        _groups[id]->registerCygnet(cygnet, type);
    }else{
        _groups.insert(std::pair<int, std::shared_ptr<ISWAGroup>>(id, std::make_shared<ISWAGroup>(id, type)));
        _groups[id]->registerCygnet(cygnet, type);
    }

}

void ISWAManager::unregisterFromGroup(int id, ISWACygnet* cygnet){
    if(_groups.find(id) != _groups.end()){
        _groups[id]->unregisterCygnet(cygnet);
    }
}

void ISWAManager::registerOptionsToGroup(int id, const std::vector<properties::SelectionProperty::Option>& options){
    if(_groups.find(id) != _groups.end()){
        _groups[id]->registerOptions(options);
    }
}

std::shared_ptr<ISWAGroup> ISWAManager::iSWAGroup(std::string name){
    for(auto group : _groups){
        if(group.second->name() == name){
            return group.second;
        }
    }

    return nullptr;
}

scripting::ScriptEngine::LuaLibrary ISWAManager::luaLibrary() {
    return {
        "iswa",
        {
            {
                "addCygnet",
                &luascriptfunctions::iswa_addCygnet,
                "string",
                "Sets a property identified by the URI in "
                "the first argument. The second argument can be any type, but it has to "
                " agree with the type that the property expects",
                true
            }
        }
    };
}

}// namsepace openspace