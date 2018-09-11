/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/iswa/rendering/kameleonplane.h>
#include <modules/iswa/rendering/dataplane.h>
#include <modules/iswa/rendering/datasphere.h>
#include <modules/iswa/rendering/iswabasegroup.h>
#include <modules/iswa/rendering/iswacygnet.h>
#include <modules/iswa/rendering/iswadatagroup.h>
#include <modules/iswa/rendering/iswakameleongroup.h>
#include <modules/iswa/rendering/textureplane.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/json.h>
#include <openspace/engine/globals.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/constexpr.h>
#include <fstream>

#include "iswamanager_lua.inl"

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED

#ifdef WIN32
#pragma warning (push)
#pragma warning (disable : 4619) // #pragma warning: there is no warning number '4675'
#endif // WIN32

#include <ccmc/Kameleon.h>

#ifdef WIN32
#pragma warning (pop)
#endif // WIN32

#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

constexpr const char* monthNumber(const char* month) {
    if      (ghoul::equal(month, "JAN")) return "01";
    else if (ghoul::equal(month, "FEB")) return "02";
    else if (ghoul::equal(month, "MAR")) return "03";
    else if (ghoul::equal(month, "APR")) return "04";
    else if (ghoul::equal(month, "MAY")) return "05";
    else if (ghoul::equal(month, "JUN")) return "06";
    else if (ghoul::equal(month, "JUL")) return "07";
    else if (ghoul::equal(month, "AUG")) return "08";
    else if (ghoul::equal(month, "SEP")) return "09";
    else if (ghoul::equal(month, "OCT")) return "10";
    else if (ghoul::equal(month, "NOV")) return "11";
    else if (ghoul::equal(month, "DEC")) return "12";
    else                                 return "";
}


namespace {
    using json = nlohmann::json;
    constexpr const char* _loggerCat = "IswaManager";

    void createScreenSpace(int id) {
        std::string idStr = std::to_string(id);
        openspace::global::scriptEngine.queueScript(
            "openspace.iswa.addScreenSpaceCygnet({CygnetId =" + idStr + "});",
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

} // namespace

namespace openspace {

IswaManager::IswaManager()
    : properties::PropertyOwner({ "IswaManager" })
    , _baseUrl("https://iswa-demo-server.herokuapp.com/")
{
    _type[CygnetType::Texture] = "Texture";
    _type[CygnetType::Data] = "Data";
    _type[CygnetType::Kameleon] = "Kameleon";

    _geom[CygnetGeometry::Plane] = "Plane";
    _geom[CygnetGeometry::Sphere] = "Sphere";

    global::downloadManager.fetchFile(
        "http://iswa3.ccmc.gsfc.nasa.gov/IswaSystemWebApp/CygnetHealthServlet",
        [this](const DownloadManager::MemoryFile& file) {
            fillCygnetInfo(std::string(file.buffer));
        }
    );
}

IswaManager::~IswaManager() {
    _groups.clear();
    _cygnetInformation.clear();
}

void IswaManager::addIswaCygnet(int id, const std::string& type, std::string group) {
    if (id > 0) {
        createScreenSpace(id);
    } else if (id < 0) {
        // create metadata object and assign group and id
        MetadataFuture metaFuture;
        metaFuture.id = id;
        metaFuture.group = std::move(group);

        // Assign type of cygnet Texture/Data
        if (type == _type[CygnetType::Texture]) {
            metaFuture.type = CygnetType::Texture;
        } else if (type  == _type[CygnetType::Data]) {
            metaFuture.type = CygnetType::Data;
        } else {
            LERROR("\""+ type + "\" is not a valid type");
            return;
        }

        // This callback determines what geometry should be used and creates
        // the right cygnet
        auto metadataCallback =
            [this, &metaFuture](const DownloadManager::MemoryFile& file) {
                //Create a string from downloaded file
                std::string res(file.buffer, file.buffer + file.size);
                metaFuture.json = res;

                //convert to json
                json j = json::parse(res);

                // Check what kind of geometry here
                if (j["Coordinate Type"].is_null()) {
                    metaFuture.geom = CygnetGeometry::Sphere;
                    createSphere(metaFuture);
                } else if (j["Coordinate Type"] == "Cartesian") {
                    metaFuture.geom = CygnetGeometry::Plane;
                    createPlane(metaFuture);
                }
                LDEBUG("Download to memory finished");
            };

        // Download metadata
        global::downloadManager.fetchFile(
            _baseUrl + std::to_string(-id),
            metadataCallback,
            [id](const std::string& err) {
                LDEBUG(
                    "Download to memory was aborted for data cygnet with id " +
                    std::to_string(id) + ": " + err
                );
            }
        );
    }
}

void IswaManager::addKameleonCdf(std::string groupName, int pos) {
    // auto info = _cdfInformation[group][pos];
    IswaBaseGroup* group = iswaGroup(groupName);
    if (group) {
        dynamic_cast<IswaKameleonGroup*>(group)->changeCdf(
            _cdfInformation[groupName][pos].path
        );
        return;
    }

    createKameleonPlane(_cdfInformation[groupName][pos], "z");
    createKameleonPlane(_cdfInformation[groupName][pos], "y");
    createKameleonPlane(_cdfInformation[groupName][pos], "x");
}

std::future<DownloadManager::MemoryFile> IswaManager::fetchImageCygnet(int id,
                                                                       double timestamp)
{
    return global::downloadManager.fetchFile(
            iswaUrl(id, timestamp, "image"),
            [id](const DownloadManager::MemoryFile&) {
                LDEBUG(
                    "Download to memory finished for image cygnet with id: " +
                    std::to_string(id)
                );
            },
            [id](const std::string& err) {
                LDEBUG(
                    "Download to memory was aborted for image cygnet with id " +
                    std::to_string(id) + ": " + err
                );
            }
        );
}

std::future<DownloadManager::MemoryFile> IswaManager::fetchDataCygnet(int id,
                                                                      double timestamp)
{
    return global::downloadManager.fetchFile(
            iswaUrl(id, timestamp, "data"),
            [id](const DownloadManager::MemoryFile&) {
                LDEBUG(
                    "Download to memory finished for data cygnet with id: " +
                    std::to_string(id)
                );
            },
            [id](const std::string& err) {
                LDEBUG(
                    "Download to memory was aborted for data cygnet with id " +
                    std::to_string(id) + ": " + err
                );
            }
        );
}

std::string IswaManager::iswaUrl(int id, double timestamp, const std::string& type) {
    std::string url;
    if (id < 0) {
        url = _baseUrl + type + "/" + std::to_string(-id) + "/";
    } else {
        url = "http://iswa3.ccmc.gsfc.nasa.gov/IswaSystemWebApp/iSWACygnetStreamer?"
              "window=-1&cygnetId="+ std::to_string(id) +"&timestamp=";
    }

    //std::string t = Time::ref().currentTimeUTC();
    std::string t = SpiceManager::ref().dateFromEphemerisTime(timestamp);
    std::stringstream ss(t);
    std::string token;

    std::getline(ss, token, ' ');
    url += token + "-";
    std::getline(ss, token, ' ');
    url = url + monthNumber(token.c_str()) + "-";
    std::getline(ss, token, 'T');
    url += token + "%20";
    std::getline(ss, token, '.');
    url += token;

    return url;
}

void IswaManager::registerGroup(std::string groupName, std::string type) {
    if (_groups.find(groupName) == _groups.end()) {
        const bool dataGroup = (type == typeid(DataPlane).name()) ||
                               (type == typeid(DataSphere).name());

        const bool kameleonGroup = (type == typeid(KameleonPlane).name());

        if (dataGroup) {
            _groups[groupName] = std::make_shared<IswaDataGroup>(
                std::move(groupName),
                std::move(type)
            );
        } else if (kameleonGroup) {
            _groups[groupName] = std::make_shared<IswaKameleonGroup>(
                std::move(groupName),
                std::move(type)
            );
        } else {
            _groups[groupName] = std::make_shared<IswaBaseGroup>(
                std::move(groupName),
                std::move(type)
            );
        }
    } else if (!_groups[groupName]->isType(type)) {
        LWARNING("Can't add cygnet to groups with diffent type");
    }
}

IswaBaseGroup* IswaManager::iswaGroup(const std::string& name) {
    if (_groups.find(name) != _groups.end()) {
        return _groups[name].get();
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

std::shared_ptr<MetadataFuture> IswaManager::downloadMetadata(int id) {
    std::shared_ptr<MetadataFuture> metaFuture = std::make_shared<MetadataFuture>();

    metaFuture->id = id;
    global::downloadManager.fetchFile(
        _baseUrl + std::to_string(-id),
        [&metaFuture](const DownloadManager::MemoryFile& file) {
            metaFuture->json = std::string(file.buffer, file.buffer + file.size);
            metaFuture->isFinished = true;
        },
        [](const std::string& err) {
            LWARNING("Download Metadata to memory was aborted: " + err);
        }
    );
    return metaFuture;
}

std::string IswaManager::jsonPlaneToLuaTable(MetadataFuture& data) {
    if (data.json.empty()) {
        return "";
    }
    json j = json::parse(data.json);

    std::string parent = j["Central Body"];
    std::string frame = j["Coordinates"];
    std::string coordinateType = j["Coordinate Type"];
    int updateTime = j["ISWA_UPDATE_SECONDS"];

    glm::vec3 max(j["Plot XMAX"], j["Plot YMAX"], j["Plot ZMAX"]);
    glm::vec3 min(j["Plot XMIN"], j["Plot YMIN"], j["Plot ZMIN"]);

    glm::vec4 spatialScale(1.f, 1.f, 1.f, 10.f);
    std::string spatial = j["Spatial Scale (Custom)"];
    if (spatial == "R_E") {
        spatialScale.x = 6.371f;
        spatialScale.y = 6.371f;
        spatialScale.z = 6.371f;
        spatialScale.w = 6.f;
    }

    float xOffset = 0.f;
    if (data.id == -7) {
        xOffset = -10.f;
    }
    if (data.id == -8) {
        xOffset = -20.f;
    }
    if (data.id == -9) {
        xOffset = -30.f;
    }

    std::string table = "{"
    "Name = '" + data.name +"' , "
    "Parent = '" + parent + "', "
    "Renderable = {"
        "Type = '" + _type[data.type] + _geom[data.geom] + "', "
        "Id = " + std::to_string(data.id) + ", "
        "Frame = '" + frame + "' , "
        "GridMin = " + std::to_string(min) + ", "
        "GridMax = " + std::to_string(max) + ", "
        "SpatialScale = " + std::to_string(spatialScale) + ", "
        "UpdateTime = " + std::to_string(updateTime) + ", "
        "CoordinateType = '" + coordinateType + "', "
        "Group = '"+ data.group + "',"
        "XOffset = "+ std::to_string(xOffset) + ","
        "}"
    "}";

    return table;
}

std::string IswaManager::parseKWToLuaTable(const CdfInfo& info, const std::string& cut) {
    if (info.path.empty()) {
        return "";
    }

    const std::string& extension =
        ghoul::filesystem::File(absPath(info.path)).fileExtension();
    if (extension == "cdf") {
        KameleonWrapper kw = KameleonWrapper(absPath(info.path));

        std::string parent  = kw.parent();
        std::string frame   = kw.frame();
        glm::vec3   min     = kw.gridMin();
        glm::vec3   max     = kw.gridMax();


        std::array<std::string, 3> gridUnits = kw.gridUnits();

        glm::vec4 spatialScale;
        std::string coordinateType;
        if (gridUnits[0] == "R" && gridUnits[1] == "R" && gridUnits[2] == "R") {
            spatialScale.x = 6.371f;
            spatialScale.y = 6.371f;
            spatialScale.z = 6.371f;
            spatialScale.w = 6;

            coordinateType = "Cartesian";
        } else {
            spatialScale = glm::vec4(1.f);
            spatialScale.w = 1; //-log10(1.0f/max.x);
            coordinateType = "Polar";
        }

        std::string table = "{"
            "Name = '" + info.name + "',"
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
                "axisCut = '" + cut + "',"
                "CoordinateType = '" + coordinateType + "', "
                "Group = '" + info.group + "',"
                // "Group = '',"
                "fieldlineSeedsIndexFile = '" + info.fieldlineSeedsIndexFile + "'"
                "}"
            "}"
            ;
        return table;
    }
    else {
        return "";
    }
}

std::string IswaManager::jsonSphereToLuaTable(MetadataFuture& data) {
    if (data.json == "") {
        LWARNING("jsonSphereToLuaTable: no content in metadata json");
        return "";
    }

    json j = json::parse(data.json);
    j = j["metadata"];
    std::string parent = j["central_body"];
    parent[0] = static_cast<char>(toupper(static_cast<int>(parent[0])));
    std::string frame = j["standard_grid_target"];
    std::string coordinateType = j["grid_1_type"];
    float updateTime = j["output_time_interval"];
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
    "Name = '" + data.name +"' , "
    "Parent = '" + parent + "', "
    "Renderable = {"
        "Type = '" + _type[data.type] + _geom[data.geom] + "', "
        "Id = " + std::to_string(data.id) + ", "
        "Frame = '" + frame + "' , "
        "GridMin = " + std::to_string(min) + ", "
        "GridMax = " + std::to_string(max) + ", "
        "UpdateTime = " + std::to_string(updateTime) + ", "
        "Radius = " + std::to_string(radius) + ", "
        "CoordinateType = '" + coordinateType + "', "
        "Group = '"+ data.group + "',"
        "}"
    "}";

    return table;
}

void IswaManager::createPlane(MetadataFuture& data) {
    // check if this plane already exist
    std::string name = _type[data.type] + _geom[data.geom] + std::to_string(data.id);

    if (!data.group.empty()) {
        std::string type;
        if (data.type == CygnetType::Data) {
            type = typeid(DataPlane).name();
        } else {
            type = typeid(TexturePlane).name();
        }

        registerGroup(data.group, type);

        auto it = _groups.find(data.group);
        if (it == _groups.end() || (*it).second->isType(type)) {
            name = data.group + "_" + name;
        } else {
            data.group = "";
        }
    }

    data.name = name;

    if (global::renderEngine.scene()->sceneGraphNode(name)) {
        LERROR("A node with name \"" + name + "\" already exist");
        return;
    }

    std::string luaTable = jsonPlaneToLuaTable(data);
    if (!luaTable.empty()) {
        std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
        global::scriptEngine.queueScript(
            script,
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
}

void IswaManager::createSphere(MetadataFuture& data) {
    // check if this sphere already exist
    std::string name = _type[data.type] + _geom[data.geom] + std::to_string(data.id);

    if(!data.group.empty()){
        std::string type = typeid(DataSphere).name();
        registerGroup(data.group, type);

        auto it = _groups.find(data.group);
        if (it == _groups.end() || (*it).second->isType(type)) {
            name = data.group + "_" + name;
        } else {
            data.group = "";
        }
    }

    data.name = name;

    if (global::renderEngine.scene()->sceneGraphNode(name)) {
        LERROR("A node with name \"" + name +"\" already exist");
        return;
    }
    std::string luaTable = jsonSphereToLuaTable(data);
    if (luaTable != "") {
        std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
        global::scriptEngine.queueScript(
            script,
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
}

void IswaManager::createKameleonPlane(CdfInfo info, std::string cut) {
    const std::string& extension = ghoul::filesystem::File(
        absPath(info.path)
    ).fileExtension();
    if (FileSys.fileExists(absPath(info.path)) && extension == "cdf") {
        if (!info.group.empty()) {
            std::string type = typeid(KameleonPlane).name();
            registerGroup(info.group, type);

            auto it = _groups.find(info.group);
            if (it == _groups.end() || (*it).second->isType(type)) {
                info.name = info.group + "_" + info.name;
            } else {
                info.group = "";
            }
        }

        info.name = info.name + "-" + cut;

        if (global::renderEngine.scene()->sceneGraphNode(info.name)) {
            LERROR("A node with name \"" + info.name +"\" already exist");
            return;
        }

        std::string luaTable = parseKWToLuaTable(info, cut);
        if (!luaTable.empty()) {
            std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
            global::scriptEngine.queueScript(
                script,
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    } else {
        LWARNING( absPath(info.path) + " is not a cdf file or can't be found.");
    }
}

void IswaManager::createFieldline(std::string name, std::string cdfPath,
                                  std::string seedPath)
{
    const std::string& ext = ghoul::filesystem::File(absPath(cdfPath)).fileExtension();

    if (FileSys.fileExists(absPath(cdfPath)) && ext == "cdf") {
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
        if (!luaTable.empty()) {
            std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
            global::scriptEngine.queueScript(
                script,
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    } else {
        LWARNING( cdfPath + " is not a cdf file or can't be found.");
    }
}

void IswaManager::fillCygnetInfo(std::string jsonString) {
    if (jsonString != "") {
        json j = json::parse(jsonString);

        std::set<std::string> lists  =  {"listOfPriorityCygnets", "listOfOKCygnets"
                                        // ,"listOfStaleCygnets", "listOfInactiveCygnets",
                                        };

        for (auto list : lists) {
            json jsonList = j[list];
            for (size_t i = 0; i < jsonList.size(); ++i) {
                json jCygnet = jsonList.at(i);

                std::string name = jCygnet["cygnetDisplayTitle"];
                std::replace(name.begin(), name.end(),'.', ',');

                CygnetInfo info = {
                    name,
                    jCygnet["cygnetDescription"],
                    jCygnet["cygnetUpdateInterval"],
                    false
                };
                _cygnetInformation[jCygnet["cygnetID"]] = std::make_shared<CygnetInfo>(
                    info
                );
            }
        }
    }
}

ghoul::Event<>& IswaManager::iswaEvent() {
    return _iswaEvent;
}

void IswaManager::addCdfFiles(std::string cdfpath) {
    cdfpath = absPath(cdfpath);
    if (FileSys.fileExists(cdfpath)) {
        //std::string basePath = path.substr(0, path.find_last_of("/\\"));
        std::ifstream jsonFile(cdfpath);

        if (jsonFile.is_open()) {
            json cdfGroups = json::parse(jsonFile);
            for(size_t i = 0; i < cdfGroups.size(); ++i) {
                json cdfGroup = cdfGroups.at(i);

                std::string groupName = cdfGroup["group"];
                std::string fieldlineSeedsIndexFile = cdfGroup["fieldlinefile"];

                if (_cdfInformation.find(groupName) != _cdfInformation.end()) {
                    LWARNING("CdfGroup with name" + groupName + " already exists.");
                    return;
                }

                _cdfInformation[groupName] = std::vector<CdfInfo>();

                json cdfs = cdfGroup["cdfs"];
                for (size_t j = 0; j < cdfs.size(); j++) {
                    json cdf = cdfs.at(j);

                    std::string name = cdf["name"];
                    std::string path = cdf["path"];
                    std::string date = cdf["date"];

                    _cdfInformation[groupName].push_back({
                        name,
                        path,
                        groupName,
                        date,
                        fieldlineSeedsIndexFile
                    });
                }
            }

            jsonFile.close();
        }
    } else {
        LWARNING(cdfpath + " is not a cdf file or can't be found.");
    }
}

void IswaManager::setBaseUrl(std::string bUrl) {
    LDEBUG("Swapped baseurl to: " + bUrl);
    _baseUrl = bUrl;
}

scripting::LuaLibrary IswaManager::luaLibrary() {
    return {
        "iswa",
        {
            {
                "addCygnet",
                &luascriptfunctions::iswa_addCygnet,
                {},
                "int, string, string",
                "Adds a IswaCygnet",
            },
            {
                "addScreenSpaceCygnet",
                &luascriptfunctions::iswa_addScreenSpaceCygnet,
                {},
                "int, string, string",
                "Adds a Screen Space Cygnets",
            },
            {
                "addKameleonPlanes",
                &luascriptfunctions::iswa_addKameleonPlanes,
                {},
                "string, int",
                "Adds KameleonPlanes from cdf file.",
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
                {},
                "string",
                "Adds a cdf files to choose from.",
            },
            {
                "removeCygnet",
                &luascriptfunctions::iswa_removeCygnet,
                {},
                "string",
                "Remove a Cygnets",
            },
            {
                "removeScreenSpaceCygnet",
                &luascriptfunctions::iswa_removeScrenSpaceCygnet,
                {},
                "int",
                "Remove a Screen Space Cygnets",
            },
            {
                "removeGroup",
                &luascriptfunctions::iswa_removeGroup,
                {},
                "int",
                "Remove a group of Cygnets",
            },
            {
                "setBaseUrl",
                &luascriptfunctions::iswa_setBaseUrl,
                {},
                "string",
                "sets the base url",
            }
        }
    };
}

} // namsepace openspace
