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
#include <modules/iswa/util/luacygnetconverter.h>
#include <modules/iswa/util/iswamanager.h>
#include <modules/iswa/ext/json/json.hpp>
#include <ghoul/filesystem/filesystem> //abspath
#include <modules/kameleon/include/kameleonwrapper.h>

 namespace {
    using json = nlohmann::json;
    const std::string _loggerCat = "IswaCygnet";
}

namespace openspace{

LuaCygnetConverter::LuaCygnetConverter(){};
LuaCygnetConverter::~LuaCygnetConverter(){};

std::string LuaCygnetConverter::toLuaTable(std::shared_ptr<MetadataFuture> metadata) const {

    //get resource type as enum value
    IswaManager::ResourceType resourceType = static_cast<IswaManager::ResourceType>(metadata->resourceType);

    std::string luaTable;
    if (IswaManager::ResourceType::Json == resourceType)
        luaTable = sphereToLuaTable(metadata); // Datasphere
    else if (IswaManager::ResourceType::Text == resourceType || IswaManager::ResourceType::Texture == resourceType)
        luaTable = planeToLuaTable(metadata); // DataPlane and TexturePlane

    return luaTable;
}

std::string LuaCygnetConverter::toLuaTable(CdfInfo info, std::string cut) const {
    return kameleonToLuaTable(info, cut); // KameleonPlane
}

std::string LuaCygnetConverter::toLuaTable(std::string name, std::string cdfPath, std::string seedPath) const {
    return fieldlineToLuaTable(name, cdfPath, seedPath); // Fieldlines
}


std::string LuaCygnetConverter::planeToLuaTable(std::shared_ptr<MetadataFuture> data) const {
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

        float xOffset = 0.0f;
        if(data->id == -7)
            xOffset = -10.0f;
        if(data->id == -8)
            xOffset = -20.0f;
        if(data->id == -9)
            xOffset = -30.0f;

        std::string table = "{"
        "Name = '" + data->group +"_"+ data->name +"' , "
        "Parent = '" + parent + "', "
        "Renderable = {"    
            "Type = '" + IswaManager::ref().cygnetType(data->cygnetType) + "', "
            "Id = " + std::to_string(data->id) + ", "
            "Frame = '" + frame + "' , "
            "GridMin = " + std::to_string(min) + ", "
            "GridMax = " + std::to_string(max) + ", "
            "SpatialScale = " + std::to_string(spatialScale) + ", "
            "UpdateTime = " + std::to_string(updateTime) + ", "
            "CoordinateType = '" + coordinateType + "', "
            "Group = '"+ data->group + "',"
            "XOffset = "+ std::to_string(xOffset) + ","
            "}"
        "}";
        
        return table;
    }
    return "";
}

std::string LuaCygnetConverter::kameleonToLuaTable(CdfInfo info, std::string cut) const {
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
                spatialScale.w = -log10(1.0f/max.x);
                coordinateType = "Polar";
            }
            float xOffset = 0.0f;
            std::string table = "{"
                "Name = '" + info.group +"_" + info.name + "',"
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
                    "fieldlineSeedsIndexFile = '"+info.fieldlineSeedsIndexFile+"',"
                    "XOffset = "+ std::to_string(xOffset) + ","
                    "}"
                "}"
                ; 
            return table;
        }
    }
    return "";
}

std::string LuaCygnetConverter::sphereToLuaTable(std::shared_ptr<MetadataFuture> data) const {
    if(data->json == ""){
        LWARNING("SphereToLuaTable: no content in metadata json");
        return "";
    }

    json j = json::parse(data->json);
    j = j["metadata"];
    std::string parent = j["central_body"];
    parent[0] = toupper(parent[0]);
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
    "Name = '" + data->group +"_"+ data->name +"' , "
    "Parent = '" + parent + "', "
    "Renderable = {"    
        "Type = '" + IswaManager::ref().cygnetType(data->cygnetType) + "', "
        "Id = " + std::to_string(data->id) + ", "
        "Frame = '" + frame + "' , "
        "GridMin = " + std::to_string(min) + ", "
        "GridMax = " + std::to_string(max) + ", "
        "SpatialScale = " + std::to_string(spatialScale) + ", "
        "UpdateTime = " + std::to_string(updateTime) + ", "
        "Radius = " + std::to_string(radius) + ", "
        "CoordinateType = '" + coordinateType + "', "
        "Group = '"+ data->group + "',"
        "}"
    "}";
    
    return table;
}

std::string LuaCygnetConverter::fieldlineToLuaTable(std::string name, std::string cdfPath, std::string seedPath) const {
    const std::string& extension = ghoul::filesystem::File(absPath(cdfPath)).fileExtension();
    std::string table = "";
    if(FileSys.fileExists(absPath(cdfPath)) && extension == "cdf"){
        table = "{"
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
    } else {
        LWARNING( cdfPath + " is not a cdf file or can't be found.");
    }
    return table;
}

}// openspace