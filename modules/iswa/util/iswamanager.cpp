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
#include <algorithm>

#include <ghoul/filesystem/filesystem>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/util/time.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scripting/script_helper.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>

#include "iswamanager_lua.inl";

namespace {
    using json = nlohmann::json;
    const std::string _loggerCat = "IswaManager";
}

namespace openspace{

IswaManager::IswaManager()
    : _iswaEvent()
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

    DlManager.fetchFile(
        "http://iswa2.ccmc.gsfc.nasa.gov/IswaSystemWebApp/CygnetHealthServlet",
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

        // create metadata object and assign group and id
        std::shared_ptr<MetadataFuture> metaFuture = std::make_shared<MetadataFuture>();
        metaFuture->id = id;
        metaFuture->group = group;

        // Assign type of cygnet Texture/Data
        if(type == _type[CygnetType::Texture]){
            metaFuture->type = CygnetType::Texture;
        } else if (type  == _type[CygnetType::Data]) {
            metaFuture->type = CygnetType::Data;
        } else {
            LERROR("\""+ type + "\" is not a valid type");
            return;
        }

        // This callback determines what geometry should be used and creates the right cygbet
        auto metadataCallback = 
        [this, metaFuture](const DownloadManager::MemoryFile& file){
            //Create a string from downloaded file
            std::string res;
            res.append(file.buffer, file.size);
            //add it to the metafuture object
            metaFuture->json = res;

            //convert to json
            json j = json::parse(res);

            // Check what kind of geometry here
            if(j["Coordinate Type"].is_null()){
                metaFuture->geom = CygnetGeometry::Sphere;
                createSphere(metaFuture);
            } else if (j["Coordinate Type"] == "Cartesian"){
                metaFuture->geom = CygnetGeometry::Plane;
                createPlane(metaFuture);
            }
            LDEBUG("Download to memory finished");
        };

        // Download metadata
        DlManager.fetchFile(
            "http://128.183.168.116:3000/" + std::to_string(-id),
            // "http://10.0.0.76:3000/" + std::to_string(-id),
            metadataCallback,
            [id](const std::string& err){
                LDEBUG("Download to memory was aborted for data cygnet with id "+ std::to_string(id)+": " + err);
            }
        );
    }
    // else{
    //     // Kameleonplane?
    //     // LERROR("No cygnet with id 0");
    //     std::string kwPath = "${OPENSPACE_DATA}/BATSRUS.cdf";
    //     if(type == "x" || type == "y" || type == "z")
    //         createKameleonPlane(kwPath, type, group);
    //     else
    //         createKameleonPlane(kwPath, "z", group);
    // }
}

void IswaManager::addKameleonCdf(std::string group, int pos){
    // auto info = _cdfInformation[group][pos];
    // std::cout << group << " " << pos << std::endl;
    createKameleonPlane(_cdfInformation[group][pos], "z");
    createKameleonPlane(_cdfInformation[group][pos], "y");
    createKameleonPlane(_cdfInformation[group][pos], "x");
}

std::future<DownloadManager::MemoryFile> IswaManager::fetchImageCygnet(int id){
    return std::move( DlManager.fetchFile(
            iswaUrl(id, "image"),
            [id](const DownloadManager::MemoryFile& file){
                LDEBUG("Download to memory finished for image cygnet with id: " + std::to_string(id));
            },
            [id](const std::string& err){
                LDEBUG("Download to memory was aborted for image cygnet with id "+ std::to_string(id)+": " + err);
            }
        ) );   
}

std::future<DownloadManager::MemoryFile> IswaManager::fetchDataCygnet(int id){
    return std::move( DlManager.fetchFile(
            iswaUrl(id, "data"),
            [id](const DownloadManager::MemoryFile& file){
                LDEBUG("Download to memory finished for data cygnet with id: " + std::to_string(id));
            },
            [id](const std::string& err){
                LDEBUG("Download to memory was aborted for data cygnet with id "+ std::to_string(id)+": " + err);
            }
        ) );   
}

std::string IswaManager::iswaUrl(int id, std::string type){
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

std::shared_ptr<IswaGroup> IswaManager::registerToGroup(std::string name, CygnetType type, IswaCygnet* cygnet){
    if(_groups.find(name) == _groups.end()){
        _groups.insert(std::pair<std::string, std::shared_ptr<IswaGroup>>(name, std::make_shared<IswaGroup>(name)));
    }

    _groups[name]->registerCygnet(cygnet, type);
    return _groups[name];
}

void IswaManager::unregisterFromGroup(std::string name, IswaCygnet* cygnet){
    if(_groups.find(name) != _groups.end()){
        _groups[name]->unregisterCygnet(cygnet);
    }
}

void IswaManager::registerOptionsToGroup(std::string name, const std::vector<properties::SelectionProperty::Option>& options){
    if(_groups.find(name) != _groups.end()){
        _groups[name]->registerOptions(options);
    }
}

std::shared_ptr<IswaGroup> IswaManager::iswaGroup(std::string name){
    if(_groups.find(name) != _groups.end()){
        return _groups[name];
    }

    return nullptr;
}

std::map<int, std::shared_ptr<CygnetInfo>>& IswaManager::cygnetInformation(){
        return _cygnetInformation;
}

std::map<std::string, std::shared_ptr<IswaGroup>>& IswaManager::groups(){
    return _groups;
}

std::map<std::string, std::vector<CdfInfo>>& IswaManager::cdfInformation(){
    return _cdfInformation;
}

std::shared_ptr<MetadataFuture> IswaManager::downloadMetadata(int id){
    std::shared_ptr<MetadataFuture> metaFuture = std::make_shared<MetadataFuture>();

    metaFuture->id = id;
    DlManager.downloadToMemory(
                "http://128.183.168.116:3000/" + std::to_string(-id),
                // "http://10.0.0.76:3000/" + std::to_string(-id),
                metaFuture->json,
                [metaFuture](const DownloadManager::FileFuture& f){
                    if(f.isFinished){
                        metaFuture->isFinished;
                        LDEBUG("Download to memory finished");
                    } else if (f.isAborted){
                        LWARNING("Download to memory was aborted: " + f.errorMessage);
                    }
                }
            );
    return metaFuture;
}

std::string IswaManager::jsonPlaneToLuaTable(std::shared_ptr<MetadataFuture> data){
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
            "Group = '"+ data->group + "',"
            "}"
        "}";
        
        return table;
    }
    return "";
}

std::string IswaManager::parseKWToLuaTable(CdfInfo info, std::string cut){
    if(info.path != ""){
        const std::string& extension = ghoul::filesystem::File(absPath(info.path)).fileExtension();
        if(extension == "cdf"){
            KameleonWrapper kw = KameleonWrapper(absPath(info.path));
     
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
                "Name = '"+info.name+"-"+cut+"_"+info.group+"',"
                "Parent = '" + parent + "', " 
                "Renderable = {"    
                    "Type = 'KameleonPlane', "
                    "Id = 0 ,"
                    "Frame = '" + frame + "' , "
                    "GridMin = " + std::to_string(min) + ", "
                    "GridMax = " + std::to_string(max) + ", "
                    "SpatialScale = " + std::to_string(spatialScale) + ", "
                    "UpdateTime = 0, "
                    "kwPath = '" + info.path + "' ," 
                    "axisCut = '"+cut+"',"
                    "CoordinateType = '" + coordinateType + "', "
                    "Group = '"+ info.group + "',"
                    "fieldlineSeedsIndexFile = '"+info.fieldlineSeedsIndexFile+"'"
                    "}"
                "}"
                ;
            // std::cout << table << std::endl;    
            return table;
        }
    }
    return "";
}

std::string IswaManager::jsonSphereToLuaTable(std::shared_ptr<MetadataFuture> data){
    if(data->json == ""){
        LWARNING("jsonSphereToLuaTable: no content in metadata json");
        return "";
    }

    json j = json::parse(data->json);
    j = j["metadata"];
    std::string parent = j["central_body"];
    parent[0] = toupper(parent[0]);
    std::string frame = j["standard_grid_target"];
    std::string coordinateType = j["grid_1_type"];
    std::string updateTime = j["output_time_interval"];
    float radius = j["radius"];

    glm::vec4 spatialScale(6.371f, 6.371f, 6.371f, 6);

    glm::vec3 max(
        j["x"]["actual_max"],
        j["y"]["actual_max"],
        j["z"]["actual_max"]
    );

    glm::vec3 min(
        j["x"]["actual_min"],
        j["y"]["actual_min"],
        j["z"]["actual_min"]
    );

    std::string table = "{"
    "Name = '" + data->name +"' , "
    "Parent = '" + parent + "', "
    "Renderable = {"    
        "Type = '" + _type[data->type] + _geom[data->geom] + "', "
        "Id = " + std::to_string(data->id) + ", "
        "Frame = '" + frame + "' , "
        "GridMin = " + std::to_string(min) + ", "
        "GridMax = " + std::to_string(max) + ", "
        "UpdateTime = " + updateTime + ", "
        "Radius = " + std::to_string(radius) + ", "
        "CoordinateType = '" + coordinateType + "', "
        "Group = '"+ data->group + "',"
        "}"
    "}";
    
    return table;
}

void IswaManager::createScreenSpace(int id){
    std::string script = "openspace.iswa.addScreenSpaceCygnet("
        "{CygnetId =" + std::to_string(id) + "});";
    OsEng.scriptEngine().queueScript(script);
}

void IswaManager::createPlane(std::shared_ptr<MetadataFuture> data){
    // check if this plane already exist
    std::string name = _type[data->type] + _geom[data->geom] + std::to_string(data->id);

    if(!data->group.empty()){
        auto it = _groups.find(data->group);
        if(it == _groups.end() || (*it).second->checkType((CygnetType) data->type))
            name += "_" + data->group;
    }

    data->name = name;

    if( OsEng.renderEngine().scene()->sceneGraphNode(name) ){
        LERROR("A node with name \"" + name +"\" already exist");
        return;
    }

    std::string luaTable = jsonPlaneToLuaTable(data);
    if(luaTable != ""){
        std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
        OsEng.scriptEngine().queueScript(script);
    }
}

void IswaManager::createSphere(std::shared_ptr<MetadataFuture> data){
    // check if this sphere already exist
    std::string name = _type[data->type] + _geom[data->geom] + std::to_string(data->id);

    if(!data->group.empty()){
        auto it = _groups.find(data->group);
        if(it == _groups.end() || (*it).second->checkType((CygnetType) data->type))
            name += "_" + data->group;
    }

    data->name = name;

    if( OsEng.renderEngine().scene()->sceneGraphNode(name) ){
        LERROR("A node with name \"" + name +"\" already exist");
        return;
    }
    std::string luaTable = jsonSphereToLuaTable(data);
    if(luaTable != ""){
        std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
        OsEng.scriptEngine().queueScript(script);
    }
}

void IswaManager::createKameleonPlane(CdfInfo info, std::string cut){
    std::cout << info.name << " " << cut << std::endl; 

    const std::string& extension = ghoul::filesystem::File(absPath(info.path)).fileExtension();

    if(FileSys.fileExists(absPath(info.path)) && extension == "cdf"){
        std::string luaTable = parseKWToLuaTable(info, cut);
        if(!luaTable.empty()){
    //         // std::cout << luaTable << std::endl;
            std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
            OsEng.scriptEngine().queueScript(script);
        }
    }else{
        LWARNING( absPath(info.path) + " is not a cdf file or can't be found.");
    }
}

void IswaManager::createFieldline(std::string name, std::string cdfPath, std::string seedPath){
    const std::string& extension = ghoul::filesystem::File(absPath(cdfPath)).fileExtension();

    if(FileSys.fileExists(absPath(cdfPath)) && extension == "cdf"){
        std::string luaTable = "{"
            "Name = '" + name + "',"
            "Parent = 'Earth',"
            "Renderable = {"
                "Type = 'RenderableFieldlines',"
                "VectorField = {"
                    "Type = 'VolumeKameleon',"
                    "File = '" + cdfPath + "',"
                    "Model = 'BATSRUS',"
                    "Variables = {'bx', 'by', 'bz'}"
                "},"
                "Fieldlines = {"
                    "Stepsize = 1,"
                    "Classification = true"
                "},"
                "SeedPoints = {"
                    "Type = 'File',"
                    "File = '" + seedPath + "'"
                "}"
            "}"
        "}";
        if(!luaTable.empty()){
            std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
            OsEng.scriptEngine().queueScript(script);
        }
    }else{
        LWARNING( cdfPath + " is not a cdf file or can't be found.");
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

void IswaManager::addCdfFiles(std::string path){
    path = absPath(path);
    if(FileSys.fileExists(path)){

        std::string basePath = path.substr(0, path.find_last_of("/\\"));
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
            
                    _cdfInformation[groupName].push_back({name, basePath+"/"+path, groupName, date, fieldlineSeedsIndexFile});
                }

            }
        }
    }else{
        LWARNING( path + " is not a cdf file or can't be found.");
    }
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
            }
        }
    };
}

}// namsepace openspace