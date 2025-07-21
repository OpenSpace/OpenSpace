/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
#include <modules/iswa/rendering/renderabledataplane.h>
#include <modules/iswa/rendering/renderabledatasphere.h>
#include <modules/iswa/rendering/renderableiswacygnet.h>
#include <modules/iswa/rendering/renderablekameleonplane.h>
#include <modules/iswa/rendering/renderabletextureplane.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/json.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/constexpr.h>
#include <ghoul/misc/stringhelper.h>
#include <filesystem>
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

namespace {
    constexpr std::string_view _loggerCat = "IswaManager";

    constexpr std::string_view monthNumber(std::string_view month) {
        if (month == "JAN") return "01";
        else if (month == "FEB") return "02";
        else if (month == "MAR") return "03";
        else if (month == "APR") return "04";
        else if (month == "MAY") return "05";
        else if (month == "JUN") return "06";
        else if (month == "JUL") return "07";
        else if (month == "AUG") return "08";
        else if (month == "SEP") return "09";
        else if (month == "OCT") return "10";
        else if (month == "NOV") return "11";
        else if (month == "DEC") return "12";
        else                     return "";
    }

    void createScreenSpace(int id) {
        using namespace openspace;
        std::string idStr = std::to_string(id);
        global::scriptEngine->queueScript(
            "openspace.iswa.addScreenSpaceCygnet({CygnetId =" + idStr + "});"
        );
    }
} // namespace

namespace openspace {

IswaManager* IswaManager::_instance = nullptr;

IswaManager::IswaManager()
    : properties::PropertyOwner({ "IswaManager", "Iswa Manager" })
    , _baseUrl("https://iswa-demo-server.herokuapp.com/")
{
    _cygnetType[CygnetType::Texture] = "Texture";
    _cygnetType[CygnetType::Data] = "Data";
    _cygnetType[CygnetType::Kameleon] = "Kameleon";

    _geom[CygnetGeometry::Plane] = "Plane";
    _geom[CygnetGeometry::Sphere] = "Sphere";

    // @TODO (abock, 2019-09-17): I commented this out as the webpage has been down for a
    // while and would probably not work anymore either way

    // global::downloadManager.fetchFile(
    //     "http://iswa3.ccmc.gsfc.nasa.gov/IswaSystemWebApp/CygnetHealthServlet",
    //     [this](const DownloadManager::MemoryFile& file) {
    //         fillCygnetInfo(std::string(file.buffer));
    //     }
    // );
}

IswaManager::~IswaManager() {
    _groups.clear();
    _cygnetInformation.clear();
}

void IswaManager::initialize() {
    ghoul_assert(!isInitialized(), "IswaManager is already initialized");
    _instance = new IswaManager;
}

void IswaManager::deinitialize() {
    ghoul_assert(isInitialized(), "IswaManager is not initialized");
    delete _instance;
    _instance = nullptr;
}

bool IswaManager::isInitialized() {
    return _instance != nullptr;
}

IswaManager& IswaManager::ref() {
    ghoul_assert(isInitialized(), "IswaManager is not initialized");
    return *_instance;
}

void IswaManager::addIswaCygnet(int id, const std::string& type, std::string group) {
    if (id > 0) {
        createScreenSpace(id);
    }
    else if (id < 0) {
        // create metadata object and assign group and id
        MetadataFuture metaFuture;
        metaFuture.id = id;
        metaFuture.group = std::move(group);

        // Assign type of cygnet Texture/Data
        if (type == _cygnetType[CygnetType::Texture]) {
            metaFuture.type = CygnetType::Texture;
        }
        else if (type  == _cygnetType[CygnetType::Data]) {
            metaFuture.type = CygnetType::Data;
        }
        else {
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
                nlohmann::json j = nlohmann::json::parse(res);

                // Check what kind of geometry here
                if (j["Coordinate Type"].is_null()) {
                    metaFuture.geom = CygnetGeometry::Sphere;
                    createSphere(metaFuture);
                }
                else if (j["Coordinate Type"] == "Cartesian") {
                    metaFuture.geom = CygnetGeometry::Plane;
                    createPlane(metaFuture);
                }
                LDEBUG("Download to memory finished");
            };

        // Download metadata
        global::downloadManager->fetchFile(
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
    return global::downloadManager->fetchFile(
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
    return global::downloadManager->fetchFile(
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
    }
    else {
        url = "http://iswa3.ccmc.gsfc.nasa.gov/IswaSystemWebApp/iSWACygnetStreamer?"
              "window=-1&cygnetId="+ std::to_string(id) +"&timestamp=";
    }

    std::stringstream ss;
    ss << SpiceManager::ref().dateFromEphemerisTime(timestamp);;
    std::string token;

    ghoul::getline(ss, token, ' ');
    url += token + "-";
    ghoul::getline(ss, token, ' ');
    url = std::format("{}{}-", url, monthNumber(token));
    ghoul::getline(ss, token, 'T');
    url += token + "%20";
    ghoul::getline(ss, token, '.');
    url += token;

    return url;
}

void IswaManager::registerGroup(std::string groupName, std::string type) {
    if (_groups.find(groupName) == _groups.end()) {
        const bool dataGroup = (type == typeid(RenderableDataPlane).name()) ||
                               (type == typeid(RenderableDataSphere).name());

        const bool kameleonGroup = (type == typeid(RenderableKameleonPlane).name());

        if (dataGroup) {
            _groups[groupName] = std::make_shared<IswaDataGroup>(
                std::move(groupName),
                std::move(type)
            );
        }
        else if (kameleonGroup) {
            _groups[groupName] = std::make_shared<IswaKameleonGroup>(
                std::move(groupName),
                std::move(type)
            );
        }
        else {
            _groups[groupName] = std::make_shared<IswaBaseGroup>(
                std::move(groupName),
                std::move(type)
            );
        }
    }
    else if (!_groups[groupName]->isType(type)) {
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
    auto metaFuture = std::make_shared<MetadataFuture>();
    metaFuture->id = id;
    global::downloadManager->fetchFile(
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
    nlohmann::json j = nlohmann::json::parse(data.json);

    std::string parent = j["Central Body"].get<std::string>();
    std::string frame = j["Coordinates"].get<std::string>();
    std::string coordinateType = j["Coordinate Type"].get<std::string>();
    int updateTime = j["ISWA_UPDATE_SECONDS"].get<int>();

    glm::vec3 max = glm::vec3(
        j["Plot XMAX"].get<float>(),
        j["Plot YMAX"].get<float>(),
        j["Plot ZMAX"].get<float>()
    );
    glm::vec3 min = glm::vec3(
        j["Plot XMIN"].get<float>(),
        j["Plot YMIN"].get<float>(),
        j["Plot ZMIN"].get<float>()
    );

    glm::vec4 spatialScale(1.f, 1.f, 1.f, 10.f);
    std::string spatial = j["Spatial Scale (Custom)"].get<std::string>();
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
        "Type = '" + _cygnetType[data.type] + _geom[data.geom] + "', "
        "Id = " + ghoul::to_string(data.id) + ", "
        "Frame = '" + frame + "' , "
        "GridMin = " + ghoul::to_string(min) + ", "
        "GridMax = " + ghoul::to_string(max) + ", "
        "SpatialScale = " + ghoul::to_string(spatialScale) + ", "
        "UpdateTime = " + ghoul::to_string(updateTime) + ", "
        "CoordinateType = '" + coordinateType + "', "
        "Group = '"+ data.group + "',"
        "XOffset = "+ ghoul::to_string(xOffset) + ","
        "}"
    "}";

    return table;
}

std::string IswaManager::parseKWToLuaTable(const CdfInfo& info, const std::string& cut) {
    if (info.path.empty()) {
        return "";
    }

    std::filesystem::path ext = std::filesystem::path(absPath(info.path)).extension();
    if (ext == ".cdf") {
        KameleonWrapper kw = KameleonWrapper(absPath(info.path));

        std::string parent = kw.parent();
        std::string frame = kw.frame();
        glm::vec3 min = kw.gridMin();
        glm::vec3 max = kw.gridMax();


        std::array<std::string, 3> gridUnits = kw.gridUnits();

        glm::vec4 spatialScale = glm::vec4(0.f);
        std::string coordinateType;
        if (gridUnits[0] == "R" && gridUnits[1] == "R" && gridUnits[2] == "R") {
            spatialScale.x = 6.371f;
            spatialScale.y = 6.371f;
            spatialScale.z = 6.371f;
            spatialScale.w = 6;

            coordinateType = "Cartesian";
        }
        else {
            spatialScale = glm::vec4(1.f);
            spatialScale.w = 1; //-log10(1.f/max.x);
            coordinateType = "Polar";
        }

        std::string table = "{"
            "Name = '" + info.name + "',"
            "Parent = '" + parent + "', "
            "Renderable = {"
                "Type = 'KameleonPlane', "
                "Id = 0 ,"
                "Frame = '" + frame + "' , "
                "GridMin = " + ghoul::to_string(min) + ", "
                "GridMax = " + ghoul::to_string(max) + ", "
                "SpatialScale = " + ghoul::to_string(spatialScale) + ", "
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

    nlohmann::json j = nlohmann::json::parse(data.json);
    j = j["metadata"];
    std::string parent = j["central_body"].get<std::string>();
    parent[0] = static_cast<char>(toupper(static_cast<int>(parent[0])));
    std::string frame = j["standard_grid_target"].get<std::string>();
    std::string coordinateType = j["grid_1_type"].get<std::string>();
    float updateTime = j["output_time_interval"].get<float>();
    float radius = j["radius"].get<float>();

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
        "Type = '" + _cygnetType[data.type] + _geom[data.geom] + "', "
        "Id = " + std::to_string(data.id) + ", "
        "Frame = '" + frame + "' , "
        "GridMin = " + ghoul::to_string(min) + ", "
        "GridMax = " + ghoul::to_string(max) + ", "
        "UpdateTime = " + ghoul::to_string(updateTime) + ", "
        "Radius = " + ghoul::to_string(radius) + ", "
        "CoordinateType = '" + coordinateType + "', "
        "Group = '"+ data.group + "',"
        "}"
    "}";

    return table;
}

void IswaManager::createPlane(MetadataFuture& data) {
    // check if this plane already exist
    std::string name = std::format(
        "{}{}{}", _cygnetType[data.type], _geom[data.geom], data.id
    );

    if (!data.group.empty()) {
        std::string type;
        if (data.type == CygnetType::Data) {
            type = typeid(RenderableDataPlane).name();
        }
        else {
            type = typeid(RenderableTexturePlane).name();
        }

        registerGroup(data.group, type);

        auto it = _groups.find(data.group);
        if (it == _groups.end() || (*it).second->isType(type)) {
            name = data.group + "_" + name;
        }
        else {
            data.group = "";
        }
    }

    data.name = name;

    if (global::renderEngine->scene()->sceneGraphNode(name)) {
        LERROR("A node with name \"" + name + "\" already exist");
        return;
    }

    std::string luaTable = jsonPlaneToLuaTable(data);
    if (!luaTable.empty()) {
        std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
        global::scriptEngine->queueScript(script);
    }
}

void IswaManager::createSphere(MetadataFuture& data) {
    // check if this sphere already exist
    std::string name = std::format(
        "{}{}{}", _cygnetType[data.type], _geom[data.geom], data.id
    );

    if (!data.group.empty()) {
        std::string type = typeid(RenderableDataSphere).name();
        registerGroup(data.group, type);

        auto it = _groups.find(data.group);
        if (it == _groups.end() || (*it).second->isType(type)) {
            name = data.group + "_" + name;
        }
        else {
            data.group = "";
        }
    }

    data.name = name;

    if (global::renderEngine->scene()->sceneGraphNode(name)) {
        LERROR("A node with name \"" + name +"\" already exist");
        return;
    }
    std::string luaTable = jsonSphereToLuaTable(data);
    if (luaTable != "") {
        std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
        global::scriptEngine->queueScript(script);
    }
}

void IswaManager::createKameleonPlane(CdfInfo info, std::string cut) {
    std::filesystem::path ext = std::filesystem::path(absPath(info.path)).extension();
    if (std::filesystem::is_regular_file(absPath(info.path)) && ext == ".cdf") {
        if (!info.group.empty()) {
            std::string type = typeid(RenderableKameleonPlane).name();
            registerGroup(info.group, type);

            auto it = _groups.find(info.group);
            if (it == _groups.end() || (*it).second->isType(type)) {
                info.name = info.group + "_" + info.name;
            }
            else {
                info.group = "";
            }
        }

        info.name = info.name + "-" + cut;

        if (global::renderEngine->scene()->sceneGraphNode(info.name)) {
            LERROR("A node with name \"" + info.name +"\" already exist");
            return;
        }

        std::string luaTable = parseKWToLuaTable(info, cut);
        if (!luaTable.empty()) {
            std::string script = "openspace.addSceneGraphNode(" + luaTable + ");";
            global::scriptEngine->queueScript(script);
        }
    }
    else {
        LWARNING(
            std::format("'{}' is not a CDF file or cannot be found", absPath(info.path))
        );
    }
}

void IswaManager::createFieldline(std::string name, std::filesystem::path cdfPath,
                                  std::string seedPath)
{
    std::filesystem::path ext = absPath(cdfPath).extension();
    if (std::filesystem::is_regular_file(absPath(cdfPath)) && ext == ".cdf") {
        std::string luaTable = "{"
            "Name = '" + name + "',"
            "Parent = 'Earth',"
            "Renderable = {"
                "Type = 'RenderableFieldlines',"
                "VectorField = {"
                    "Type = 'VolumeKameleon',"
                    "File = '" + cdfPath.string() + "',"
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
            global::scriptEngine->queueScript(script);
        }
    }
    else {
        LWARNING(std::format("{} is not a CDF file or cannot be found", cdfPath));
    }
}

void IswaManager::fillCygnetInfo(std::string jsonString) {
    if (jsonString != "") {
        nlohmann::json j = nlohmann::json::parse(jsonString);

        std::set<std::string> lists  =  {"listOfPriorityCygnets", "listOfOKCygnets"
                                        // ,"listOfStaleCygnets", "listOfInactiveCygnets",
                                        };

        for (const std::string& list : lists) {
            nlohmann::json jsonList = j[list];
            for (size_t i = 0; i < jsonList.size(); i++) {
                nlohmann::json jCygnet = jsonList.at(i);

                std::string name = jCygnet["cygnetDisplayTitle"].get<std::string>();
                std::replace(name.begin(), name.end(),'.', ',');

                CygnetInfo info = {
                    name,
                    jCygnet["cygnetDescription"].get<std::string>(),
                    jCygnet["cygnetUpdateInterval"].get<int>(),
                    false
                };
                _cygnetInformation[jCygnet["cygnetID"].get<int>()] =
                    std::make_shared<CygnetInfo>(info);
            }
        }
    }
}

ghoul::Event<>& IswaManager::iswaEvent() {
    return _iswaEvent;
}

void IswaManager::addCdfFiles(std::string cdfpath) {
    std::filesystem::path cdfFile = absPath(cdfpath);
    if (std::filesystem::is_regular_file(cdfFile)) {
        //std::string basePath = path.substr(0, path.find_last_of("/\\"));
        std::ifstream jsonFile(cdfFile);

        if (jsonFile.is_open()) {
            nlohmann::json cdfGroups = nlohmann::json::parse(jsonFile);
            for (size_t i = 0; i < cdfGroups.size(); i++) {
                nlohmann::json cdfGroup = cdfGroups.at(i);

                std::string groupName = cdfGroup["group"].get<std::string>();
                std::string fieldlineSeedsIndexFile =
                    cdfGroup["fieldlinefile"].get<std::string>();

                if (_cdfInformation.find(groupName) != _cdfInformation.end()) {
                    LWARNING("CdfGroup with name" + groupName + " already exists");
                    return;
                }

                _cdfInformation[groupName] = std::vector<CdfInfo>();

                nlohmann::json cdfs = cdfGroup["cdfs"];
                for (size_t j = 0; j < cdfs.size(); j++) {
                    nlohmann::json cdf = cdfs.at(j);

                    std::string name = cdf["name"].get<std::string>();
                    std::string path = cdf["path"].get<std::string>();
                    std::string date = cdf["date"].get<std::string>();

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
    }
    else {
        LWARNING(std::format("'{}' is not a CDF file or cannot be found", cdfFile));
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
            codegen::lua::AddCygnet,
            codegen::lua::AddScreenSpaceCygnet,
            codegen::lua::RemoveCygnet,
            codegen::lua::RemoveScreenSpaceCygnet,
            codegen::lua::RemoveGroup,
            codegen::lua::AddCdfFiles,
            codegen::lua::AddKameleonPlanes,
            codegen::lua::SetBaseUrl
        }
    };
}

} // namsepace openspace
