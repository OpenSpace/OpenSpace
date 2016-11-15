/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <modules/iswa/rendering/iswabasegroup.h>
#include <modules/iswa/rendering/iswadatagroup.h>
#include <modules/iswa/rendering/iswakameleongroup.h>

#include <modules/iswa/util/dataprocessortext.h>
#include <modules/iswa/util/dataprocessorjson.h>
#include <modules/iswa/util/dataprocessorkameleon.h>
#include <modules/iswa/ext/json/json.hpp>

#include <fstream>
#include <memory> // for shared_pointer

#include <thread>
#include <random>
#include <chrono>

#include <ghoul/filesystem/filesystem>
#include <openspace/scene/scene.h>
#include <openspace/util/spicemanager.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>

#include "iswamanager_lua.inl";

namespace {
    using json = nlohmann::json;
    const std::string _loggerCat = "IswaManager";
    std::string baseUrl = "http://iswa-demo-server.herokuapp.com/";
    //const std::string baseUrl = "http://128.183.168.116:3000/";
}

namespace openspace{

IswaManager::IswaManager()
    : _luaConverter()
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

    _resourceType[ResourceType::Texture] = "Texture";
    _resourceType[ResourceType::Json] = "Json";
    _resourceType[ResourceType::Text] = "Text";
    _resourceType[ResourceType::Cdf] = "Kameleon";

    _cygnetType[CygnetType::TexturePlane] = "TexturePlane";
    _cygnetType[CygnetType::DataPlane] = "DataPlane";
    _cygnetType[CygnetType::KameleonPlane] = "KameleonPlane";
    _cygnetType[CygnetType::DataSphere] = "DataSphere";

    _geometryType[GeometryType::Plane] = "Plane";
    _geometryType[GeometryType::Sphere] = "Sphere";

    OsEng.downloadManager().fetchFile(
        "http://iswa.ccmc.gsfc.nasa.gov/IswaSystemWebApp/CygnetHealthServlet",
        [this](const DownloadManager::MemoryFile& file){
            fillCygnetInfo(std::string(file.buffer));
        },
        [](const std::string& err){
            LWARNING("Download to memory was aborted: " + err);
        }
    );
}

IswaManager::~IswaManager(){
    _groups.clear();
    _cygnetInformation.clear();
}

void IswaManager::addIswaCygnet(int id, ResourceType resourceType, std::string group){
    if(id > 0){

        createScreenSpace(id);

    }else if(id < 0){
        // create metadata object and assign group, id and resourceType
        std::shared_ptr<MetadataFuture> meta = std::make_shared<MetadataFuture>();
        meta->id = id;
        meta->group = group;
        meta->resourceType = resourceType;

        auto metadataCallback = 
        [this, meta](const DownloadManager::MemoryFile& file){
            std::string metadata(file.buffer, file.size);
            meta->json = metadata;
            createIswaCygnet(meta);
            LDEBUG("Download to memory finished");
        };

        // std::mt19937_64 eng{std::random_device{}()};
        // std::uniform_int_distribution<> dist{10, 100};
        // std::this_thread::sleep_for(std::chrono::milliseconds{dist(eng)});

        // Download metadataCallback
        OsEng.downloadManager().fetchFile(
            baseUrl + std::to_string(-id),
            metadataCallback,
            [id](const std::string& err){
                LDEBUG("Download to memory was aborted for data cygnet with id "+ std::to_string(id)+": " + err);
            }
        );
    }
}


void IswaManager::addKameleonCdf(std::string groupName, int pos){
    // auto info = _cdfInformation[group][pos];
    auto group = iswaGroup(groupName);
    if(group){
        std::dynamic_pointer_cast<IswaKameleonGroup>(group)->changeCdf(_cdfInformation[groupName][pos].path);
        return;
    }

    createKameleonPlane(_cdfInformation[groupName][pos], "z");
    createKameleonPlane(_cdfInformation[groupName][pos], "y");
    createKameleonPlane(_cdfInformation[groupName][pos], "x");
    clearGroupBuildData(groupName);
}

bool IswaManager::getResourceType(const std::string& type, ResourceType& enumType){
    for(int i = 0; i <= ResourceType::NoResourceType; i++){
        if(i == ResourceType::NoResourceType){
            return false;           
        } else if (type == _resourceType.at(i)) {
            enumType = static_cast<ResourceType>(i);
            return true;
        }  
    }
}

bool IswaManager::getCygnetType(const std::string& type, CygnetType& enumType){

    for(int i = 0; i <= CygnetType::NoCygnetType; i++) {
         if(i == CygnetType::NoCygnetType){
            return false;           
        } else if (type == _cygnetType.at(i)) {
            enumType = static_cast<CygnetType>(i);
            return true;
        }
    }
}

std::string IswaManager::cygnetType(int i){
    return _cygnetType[static_cast<CygnetType>(i)];
}

std::future<DownloadManager::MemoryFile> IswaManager::fetchImageCygnet(int id, double timestamp) {
    return std::move(OsEng.downloadManager().fetchFile(
            iswaUrl(id, timestamp, "image"),
            [id](const DownloadManager::MemoryFile& file){
                LDEBUG("Download to memory finished for image cygnet with id: " + std::to_string(id));
            },
            [id](const std::string& err){
                LDEBUG("Download to memory was aborted for image cygnet with id "+ std::to_string(id)+": " + err);
            }
        ) );   
}

std::future<DownloadManager::MemoryFile> IswaManager::fetchDataCygnet(int id, double timestamp) {
    return std::move(OsEng.downloadManager().fetchFile(
            iswaUrl(id, timestamp, "data"),
            [id](const DownloadManager::MemoryFile& file){
                LDEBUG("Download to memory finished for data cygnet with id: " + std::to_string(id));
            },
            [id](const std::string& err){
                LDEBUG("Download to memory was aborted for data cygnet with id "+ std::to_string(id)+": " + err);
            }
        ) );   
}

std::string IswaManager::iswaUrl(int id, double timestamp, std::string type){
    std::string url;
    if(id < 0){
        url = baseUrl+type+"/" + std::to_string(-id) + "/";
    } else{
        url = "http://iswa.ccmc.gsfc.nasa.gov/IswaSystemWebApp/iSWACygnetStreamer?window=-1&cygnetId="+ std::to_string(id) +"&timestamp=";
    }        

    std::string t = SpiceManager::ref().dateFromEphemerisTime(timestamp);
    std::stringstream ss(t);
    std::string token;


    //get the url with the right time format
    std::getline(ss, token, ' ');
    url += token + "-"; 
    std::getline(ss, token, ' ');
    url += _month.at(token) + "-";
    std::getline(ss, token, 'T');
    url += token + "%20";
    std::getline(ss, token, '.');
    url += token;

    return url;
}

std::shared_ptr<IswaBaseGroup> IswaManager::registerGroup(std::string groupName, CygnetType cygnetType, ResourceType resourceType){
    if(_groups.find(groupName) == _groups.end()){

        // choose the right dataprocessor depending on resourcetype
        std::shared_ptr<DataProcessor> dataProcessor;
        if (ResourceType::Json == resourceType)
            dataProcessor = std::make_shared<DataProcessorJson>();
        else if (ResourceType::Text == resourceType)
            dataProcessor = std::make_shared<DataProcessorText>();
        else if (ResourceType::Cdf == resourceType)
            dataProcessor = std::make_shared<DataProcessorKameleon>();

        // Factory. Create the right cygnet depending on cygnetType
        if(cygnetType == CygnetType::TexturePlane){
            std::shared_ptr<IswaBaseGroup> group = std::make_shared<IswaBaseGroup>(groupName, cygnetType);
            _groups.insert( std::pair<std::string, std::shared_ptr<IswaBaseGroup> >(groupName, group));
        } else if(cygnetType == CygnetType::KameleonPlane){
           std::shared_ptr<IswaBaseGroup> group = std::make_shared<IswaKameleonGroup>(groupName, cygnetType, dataProcessor);
            _groups.insert( std::pair<std::string, std::shared_ptr<IswaBaseGroup> >(groupName, group));
        } else if(cygnetType == CygnetType::DataSphere || cygnetType == CygnetType::DataPlane){
           std::shared_ptr<IswaBaseGroup> group = std::make_shared<IswaDataGroup>(groupName, cygnetType, dataProcessor);
            _groups.insert( std::pair<std::string, std::shared_ptr<IswaBaseGroup> >(groupName, group));
        }

        return _groups[groupName];
    } else {
        LWARNING("Trying to add Group with name: '" + groupName + "' but it already exist.");
        return nullptr;
    }
}

void IswaManager::unregisterGroup(std::string groupName){
    if(_groups.find(groupName) != _groups.end()){
        _groups.erase(groupName);
    } else {
        LWARNING("Trying to erase Group with name: '" + groupName + "' but it does not exist.");
    }
}

std::shared_ptr<IswaBaseGroup> IswaManager::iswaGroup(std::string name){
    if(_groups.find(name) != _groups.end()){
        return _groups[name];
    }

    return nullptr;
}

std::map<int, std::shared_ptr<CygnetInfo>>& IswaManager::cygnetInformation(){
        return _cygnetInformation;
}

std::map<std::string, std::shared_ptr<IswaBaseGroup>>& IswaManager::groups(){
    return _groups;
}

std::map<std::string, std::vector<CdfInfo>>& IswaManager::cdfInformation(){
    return _cdfInformation;
}

void IswaManager::createScreenSpace(int id){
    std::string script = "openspace.iswa.addScreenSpaceCygnet("
        "{CygnetId =" + std::to_string(id) + "});";
    OsEng.scriptEngine().queueScript(
        script,
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void IswaManager::createIswaCygnet(std::shared_ptr<MetadataFuture> metadata){
    //convert metadata to json
    json jsondata;
    try{
        jsondata = json::parse(metadata->json);
    }catch(std::invalid_argument e){
        LERROR("Could not parse json metadata.");
        return;
    }
    // set geometry type
    std::string geometryType;
    if(jsondata["Coordinate Type"].is_null()){
        geometryType = _geometryType[GeometryType::Sphere];
    } else if (jsondata["Coordinate Type"] == "Cartesian"){
        geometryType = _geometryType[GeometryType::Plane];
    } else {
        LERROR("Unknown Coordinate type");
        return;
    }

    ResourceType resourceType = static_cast<ResourceType>(metadata->resourceType);
    std::string resource;
    if(resourceType == ResourceType::Text || resourceType == ResourceType::Json){
        resource = "Data";
    }else{
        resource = "Texture";
    }

    //check if cygnet type is valid
    CygnetType cygnetTypeId;
    std::string cygnetTypeString = resource + geometryType;
    if(!getCygnetType(cygnetTypeString, cygnetTypeId)){
        // LERROR("Unknown Cygnet type: " + metadata->cygnetType);
        return;
    }
    metadata->cygnetType = cygnetTypeId;

    // check if this plane already exist
    metadata->name = _cygnetType[metadata->cygnetType] + std::to_string(metadata->id);
    if( OsEng.renderEngine().scene()->sceneGraphNode(metadata->name) ){
        LERROR("A node with name \"" + metadata->name +"\" already exist");
        return;
    }

    //check if group exist
    if(!metadata->group.empty()){
        auto it = _groups.find(metadata->group);
        if(it == _groups.end()){
            registerGroup(metadata->group, cygnetTypeId, resourceType);
        } else if( !(*it).second->isType(cygnetTypeId) ){
            LWARNING("Group with name " + metadata->group + " Is not the same type as: " + _cygnetType[metadata->cygnetType]);
            metadata->group="";
        }
    }
    metadata->name = metadata->group + "_" + metadata->name;
    
    // create the luaTable for script
    std::string luaTable = _luaConverter.toLuaTable(metadata);

    //queue the add scene graph node script
    if(luaTable != ""){
        std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
        OsEng.scriptEngine().queueScript(
            script,
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
}

void IswaManager::createKameleonPlane(CdfInfo info, std::string cut){

    const std::string& extension = ghoul::filesystem::File(absPath(info.path)).fileExtension();
    if(FileSys.fileExists(absPath(info.path)) && extension == "cdf"){

        if(!info.group.empty()){
            ResourceType resourceType = ResourceType::Cdf;
            CygnetType cygnetType = CygnetType::KameleonPlane;

            if(_groups.find(info.group) == _groups.end())
                registerGroup(info.group, cygnetType, resourceType);

            auto it = _groups.find(info.group);
            if(it == _groups.end() || (*it).second->isType(cygnetType)){
                info.name = info.group +"_"+info.name;
            }else{
                info.group="";
            }
        }

        info.name = info.name+"-"+cut;

        if( OsEng.renderEngine().scene()->sceneGraphNode(info.name) ){
            LERROR("A node with name \"" + info.name +"\" already exist");
            return;
        }

        // create the luaTable for script
        std::string luaTable = _luaConverter.toLuaTable(info, cut);

        if(!luaTable.empty()){
            std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
            OsEng.scriptEngine().queueScript(
                script,
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    }else{
        LWARNING( absPath(info.path) + " is not a cdf file or can't be found.");
    }
}

void IswaManager::createFieldline(std::string name, std::string cdfPath, std::string seedPath) const {

    std::string luaTable = _luaConverter.toLuaTable(name, cdfPath, seedPath);
    if(!luaTable.empty()){
        std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
        OsEng.scriptEngine().queueScript(
			script,
			scripting::ScriptEngine::RemoteScripting::Yes
		);
    }
}

void IswaManager::fillCygnetInfo(std::string jsonString){
    if(jsonString != ""){
        json j = json::parse(jsonString);
        
        std::set<std::string> lists  =  {"listOfPriorityCygnets", "listOfOKCygnets"
                                        // other types of cygnets available
                                        // ,"listOfStaleCygnets", "listOfInactiveCygnets" 
                                        };

        for(auto list : lists){
            json jsonList = j[list];
            for(int i=0; i<jsonList.size(); i++){
                json jCygnet = jsonList.at(i);

                std::string name = jCygnet["cygnetDisplayTitle"];
                std::replace(name.begin(), name.end(),'.', ',');

                CygnetInfo info = {
                    name,
                    jCygnet["cygnetDescription"],
                    jCygnet["cygnetUpdateInterval"],
                    false
                };

                _cygnetInformation[jCygnet["cygnetID"]] = std::make_shared<CygnetInfo>(info);
            }
        }        
    }
}

// Called from script in scene config
void IswaManager::addCdfFiles(std::string path){
    path = absPath(path);
    if(FileSys.fileExists(path)){

        std::ifstream jsonFile(path);
        
        if (jsonFile.is_open()){
            json cdfGroups = json::parse(jsonFile);
            for(int i=0; i<cdfGroups.size(); i++){
                json cdfGroup = cdfGroups.at(i);

                std::string groupName = cdfGroup["group"];
                std::string fieldlineSeedsIndexFile = cdfGroup["fieldlinefile"];
                
                if(_cdfInformation.find(groupName) != _cdfInformation.end()){
                    LWARNING("CdfGroup with name" + groupName + " already exists.");
                    return;
                }

                _cdfInformation[groupName] = std::vector<CdfInfo>();

                json cdfs = cdfGroup["cdfs"];
                for(int j=0; j<cdfs.size(); j++){
                    json cdf = cdfs.at(j);

                    std::string name = cdf["name"];
                    std::string path = cdf["path"];
                    std::string date = cdf["date"];
            
                    _cdfInformation[groupName].push_back({name, path, groupName, date, fieldlineSeedsIndexFile});
                }

            }

            jsonFile.close();
        }
    }else{
        LWARNING( path + " is not a cdf file or can't be found.");
    }
}

void IswaManager::setBaseUrl(std::string bUrl){
    LDEBUG("Swapped baseurl to: " + bUrl);
    baseUrl = bUrl;
}

void IswaManager::clearGroupBuildData(std::string name){
    if(_groups.find(name) != _groups.end()){
        _groups[name]->dataProcessor()->clearBuildData();
    }
}

scripting::LuaLibrary IswaManager::luaLibrary() {
    return {
        "iswa",
        {
            {
                "addCygnet",
                &luascriptfunctions::iswa_addCygnet,
                "int, string, string",
                "Adds a IswaCygnet",
            },
            {
                "addScreenSpaceCygnet",
                &luascriptfunctions::iswa_addScreenSpaceCygnet,
                "int, string, string",
                "Adds a Screen Space Cygnets",
            },
            {
                "addKameleonPlanes",
                &luascriptfunctions::iswa_addKameleonPlanes,
                "string, int",
                "Adds KameleonPlanes from cdf file.",
            },
            {
                "addCdfFiles",
                &luascriptfunctions::iswa_addCdfFiles,
                "string",
                "Adds a cdf files to choose from.",
            },
            {
                "removeCygnet",
                &luascriptfunctions::iswa_removeCygnet,
                "string",
                "Remove a Cygnets",
            },
            {
                "removeScreenSpaceCygnet",
                &luascriptfunctions::iswa_removeScrenSpaceCygnet,
                "int",
                "Remove a Screen Space Cygnets",
            },
            {
                "removeGroup",
                &luascriptfunctions::iswa_removeGroup,
                "int",
                "Remove a group of Cygnets",
            },
            {
                "setBaseUrl",
                &luascriptfunctions::iswa_setBaseUrl,
                "string",
                "sets the base url",
            },
            {
                "clearGroupBuildData",
                &luascriptfunctions::iswa_clearGroupBuildData,
                "string",
                "Clear build data for a group",
            }
        }
    };
}

}// namsepace openspace