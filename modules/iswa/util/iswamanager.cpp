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

#include <ghoul/filesystem/filesystem>
#include <openspace/scene/scene.h>
#include <openspace/util/spicemanager.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>

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

    DlManager.fetchFile(
        "http://iswa3.ccmc.gsfc.nasa.gov/IswaSystemWebApp/CygnetHealthServlet",
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

void IswaManager::addIswaCygnet(int id, std::string type, std::string group){
    if(id > 0){

        createScreenSpace(id);

    }else if(id < 0){

        // create metadata object and assign group, id and resourceType
        std::shared_ptr<MetadataFuture> meta = std::make_shared<MetadataFuture>();
        meta->id = id;
        meta->group = group;
        meta->resourceType = type;

        // This callback determines what geometry should be used and creates the right cygbet
        auto metadataCallback = 
        [this, meta](const DownloadManager::MemoryFile& file){
            std::string metadata(file.buffer, file.size);
            meta->json = metadata;
            createIswaCygnet(meta);
            LDEBUG("Download to memory finished");
        };

        // Download metadata
        DlManager.fetchFile(
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
}

bool IswaManager::getResourceType(const std::string& type, int& enumType){
    for(int i = 0; i <= ResourceType::NoResourceType; i++){
        if(i == ResourceType::NoResourceType){
            return false;           
        } else if (type == _resourceType.at(i)) {
            enumType = i;
            return true;
        }  
    }
}

bool IswaManager::getCygnetType(const std::string& type, int& enumType){

    for(int i = 0; i <= CygnetType::NoCygnetType; i++) {
         if(i == CygnetType::NoCygnetType){
            return false;           
        } else if (type == _cygnetType.at(i)) {
            enumType = i;
            return true;
        }
    }
}

std::future<DownloadManager::MemoryFile> IswaManager::fetchImageCygnet(int id, double timestamp) const{
    return std::move( DlManager.fetchFile(
            iswaUrl(id, timestamp, "image"),
            [id](const DownloadManager::MemoryFile& file){
                LDEBUG("Download to memory finished for image cygnet with id: " + std::to_string(id));
            },
            [id](const std::string& err){
                LDEBUG("Download to memory was aborted for image cygnet with id "+ std::to_string(id)+": " + err);
            }
        ) );   
}

std::future<DownloadManager::MemoryFile> IswaManager::fetchDataCygnet(int id, double timestamp) const{
    return std::move( DlManager.fetchFile(
            iswaUrl(id, timestamp, "data"),
            [id](const DownloadManager::MemoryFile& file){
                LDEBUG("Download to memory finished for data cygnet with id: " + std::to_string(id));
            },
            [id](const std::string& err){
                LDEBUG("Download to memory was aborted for data cygnet with id "+ std::to_string(id)+": " + err);
            }
        ) );   
}

std::string IswaManager::iswaUrl(int id, double timestamp, std::string type) const{
    std::string url;
    if(id < 0){
        url = baseUrl+type+"/" + std::to_string(-id) + "/";
    } else{
        url = "http://iswa3.ccmc.gsfc.nasa.gov/IswaSystemWebApp/iSWACygnetStreamer?window=-1&cygnetId="+ std::to_string(id) +"&timestamp=";
    }        

    std::string t = SpiceManager::ref().dateFromEphemerisTime(timestamp);
    std::stringstream ss(t);
    std::string token;

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

void IswaManager::registerGroup(std::string groupName, int cygnetType, int resourceType){
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
            std::shared_ptr<IswaBaseGroup> group = std::make_shared<IswaBaseGroup>(groupName, _cygnetType[cygnetType]);
            _groups.insert( std::pair<std::string, std::shared_ptr<IswaBaseGroup> >(groupName, group));
        } else if(cygnetType == CygnetType::KameleonPlane){
           std::shared_ptr<IswaBaseGroup> group = std::make_shared<IswaKameleonGroup>(groupName, _cygnetType[cygnetType], dataProcessor);
            _groups.insert( std::pair<std::string, std::shared_ptr<IswaBaseGroup> >(groupName, group));
        } else if(cygnetType == CygnetType::DataSphere || cygnetType == CygnetType::DataPlane){
           std::shared_ptr<IswaBaseGroup> group = std::make_shared<IswaDataGroup>(groupName, _cygnetType[cygnetType], dataProcessor);
            _groups.insert( std::pair<std::string, std::shared_ptr<IswaBaseGroup> >(groupName, group));
        }
    } else {
        LWARNING("Trying to add Group with name: '" + groupName + "' but it already exist.");
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

std::map<int, std::shared_ptr<CygnetInfo>>& IswaManager::cygnetInformation() {
    return _cygnetInformation;
}

std::map<std::string, std::shared_ptr<IswaBaseGroup>>& IswaManager::groups() {
    return _groups;
}

std::map<std::string, std::vector<CdfInfo>>& IswaManager::cdfInformation() {
    return _cdfInformation;
}

void IswaManager::createScreenSpace(int id){
    std::string script = "openspace.iswa.addScreenSpaceCygnet("
        "{CygnetId =" + std::to_string(id) + "});";
    OsEng.scriptEngine().queueScript(script);
}

void IswaManager::createIswaCygnet(std::shared_ptr<MetadataFuture> metadata){
    //convert metadata to json
    json jsondata = json::parse(metadata->json);

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

    int resourceType;
    if(!getResourceType(metadata->resourceType, resourceType )){
        LERROR("\""+ metadata->resourceType + "\" is not a valid resource type");
        return;
    }

    //hacky, If resource type is either json or text => say it is "Data" so that cygnetType will be correct.
    // (this will be no problem once data format is standardized)
    std::string resource = metadata->resourceType;
    if(resourceType == ResourceType::Text || resourceType == ResourceType::Json)
        resource = "Data";

    //check if cygnet type is valid
    metadata->cygnetType = resource + geometryType;
    int cygnetType;
    if(!getCygnetType(metadata->cygnetType, cygnetType)){
        LERROR("Unknown Cygnet type: " + metadata->cygnetType);
        return;
    }

    // check if this plane already exist
    metadata->name = metadata->cygnetType + std::to_string(metadata->id);
    if( OsEng.renderEngine().scene()->sceneGraphNode(metadata->name) ){
        LERROR("A node with name \"" + metadata->name +"\" already exist");
        return;
    }

    //check if group exist
    if(!metadata->group.empty()){
        auto it = _groups.find(metadata->group);
        if(it == _groups.end()){
            registerGroup(metadata->group, cygnetType, resourceType);
        } else if( !(*it).second->isType(metadata->cygnetType) ){
            LWARNING("Group with name " + metadata->group + " Is not the same type as: " + metadata->cygnetType);
            metadata->group="";
        }
    }

    // create the luaTable for script
    std::string luaTable = _luaConverter.toLuaTable(metadata);

    //queue the add scene graph node script
    if(luaTable != ""){
        std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
        OsEng.scriptEngine().queueScript(script);
    }
}

void IswaManager::createKameleonPlane(CdfInfo info, std::string cut){

    const std::string& extension = ghoul::filesystem::File(absPath(info.path)).fileExtension();
    if(FileSys.fileExists(absPath(info.path)) && extension == "cdf"){


        if(!info.group.empty()){
            int resourceType = ResourceType::Cdf;
            int cygnetType = CygnetType::KameleonPlane;

            if(_groups.find(info.group) == _groups.end())
                registerGroup(info.group, cygnetType, resourceType);

            auto it = _groups.find(info.group);
            if(it == _groups.end() || (*it).second->isType(_cygnetType[cygnetType])){
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
            OsEng.scriptEngine().queueScript(script);
        }
    }else{
        LWARNING( absPath(info.path) + " is not a cdf file or can't be found.");
    }
}

void IswaManager::createFieldline(std::string name, std::string cdfPath, std::string seedPath) const {

    std::string luaTable = _luaConverter.toLuaTable(name, cdfPath, seedPath);
    if(!luaTable.empty()){
        std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
        OsEng.scriptEngine().queueScript(script);
    }
}

void IswaManager::fillCygnetInfo(std::string jsonString){
    if(jsonString != ""){
        json j = json::parse(jsonString);
        
        std::set<std::string> lists  =  {"listOfPriorityCygnets", "listOfOKCygnets"
                                        // ,"listOfStaleCygnets", "listOfInactiveCygnets",
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

        //std::string basePath = path.substr(0, path.find_last_of("/\\"));
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

scripting::ScriptEngine::LuaLibrary IswaManager::luaLibrary() {
    return {
        "iswa",
        {
            {
                "addCygnet",
                &luascriptfunctions::iswa_addCygnet,
                "int, string, string",
                "Adds a IswaCygnet",
                true
            },
            {
                "addScreenSpaceCygnet",
                &luascriptfunctions::iswa_addScreenSpaceCygnet,
                "int, string, string",
                "Adds a Screen Space Cygnets",
                true
            },
            {
                "addKameleonPlanes",
                &luascriptfunctions::iswa_addKameleonPlanes,
                "string, int",
                "Adds KameleonPlanes from cdf file.",
                true
            },
            // {
            //     "addKameleonPlane",
            //     &luascriptfunctions::iswa_addKameleonPlane,
            //     "string, string, string",
            //     "Adds a KameleonPlane from cdf file.",
            //     true
            // },
            {
                "addCdfFiles",
                &luascriptfunctions::iswa_addCdfFiles,
                "string",
                "Adds a cdf files to choose from.",
                true
            },
            {
                "removeCygnet",
                &luascriptfunctions::iswa_removeCygnet,
                "string",
                "Remove a Cygnets",
                true
            },
            {
                "removeScreenSpaceCygnet",
                &luascriptfunctions::iswa_removeScrenSpaceCygnet,
                "int",
                "Remove a Screen Space Cygnets",
                true
            },
            {
                "removeGroup",
                &luascriptfunctions::iswa_removeGroup,
                "int",
                "Remove a group of Cygnets",
                true
            },
            {
                "setBaseUrl",
                &luascriptfunctions::iswa_setBaseUrl,
                "string",
                "sets the base url",
                true
            }
        }
    };
}

}// namsepace openspace