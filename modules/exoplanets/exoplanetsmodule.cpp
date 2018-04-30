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

#include <modules/exoplanets/exoplanetsmodule.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/openspaceengine.h>

#include <openspace/util/factorymanager.h>
#include <openspace/util/time.h>

#include <ghoul/misc/assert.h>

#include <modules/exoplanets/tasks/exoplanetscsvtobintask.h>

#include <fstream>
#include <sstream>
#include <algorithm>
#include <string>

namespace openspace {

using namespace exoplanets;

struct Exoplanet {
    float A;
    double AUPPER;
    double ALOWER;
    double UA;
    float BIGOM;
    float BIGOMUPPER;
    float BIGOMLOWER;
    float UBIGOM;
    bool BINARY;
    float BMV;
    float ECC;
    float ECCUPPER;
    float ECCLOWER;
    float UECC;
    float I;
    float IUPPER;
    float ILOWER;
    float UI;
    int NCOMP;
    float OM;
    float OMUPPER;
    float OMLOWER;
    float UOM;
    double PER;
    float PERUPPER;
    float PERLOWER;
    float UPER; 
    double R;
    double RUPPER;
    double RLOWER;
    double UR;
    float RSTAR;
    float RSTARUPPER;
    float RSTARLOWER;
    float URSTAR;
    //float TEFF;
    //float TEFFUPPER;
    //float TEFFLOWER;
    //float UTEFF;
    double TT;
    float TTUPPER;
    float TTLOWER;
    float UTT;
    float POSITIONX;
    float POSITIONY;
    float POSITIONZ;
};


ExoplanetsModule::ExoplanetsModule() : OpenSpaceModule(Name) {}

int addNode(lua_State* L) {

    const int StringLocation = -1;
    const std::string starname = luaL_checkstring(L, StringLocation);


    std::ifstream data("C:/Users/Karin/Documents/OpenSpace/modules/exoplanets/expl_data.bin", std::ios::in | std::ios::binary);
    if (!data.good()) {
        std::cout << "Failed to open exoplanets data file";
    }

    std::ifstream lut("C:/Users/Karin/Documents/OpenSpace/modules/exoplanets/lookup.txt");
    if (!lut.good()) {
        std::cout << "Failed to open exoplanets look-up table file";
    }

    //1. search lut for the starname and return the corresponding location
    //2. go to that location in the data file
    //3. read sizeof(exoplanet) bytes into an exoplanet object.
    std::string planetname;
    size_t len = 0;
    Exoplanet p;
    std::string line;
    bool found = false;

    std::vector<Exoplanet> plsy;
    std::vector<std::string> plna;
    while (getline(lut, line)) {

        std::istringstream ss(line);
        getline(ss, planetname, ',');

        if (planetname.compare(0, planetname.length()-2, starname) == 0) {
            std::string location_s;
            getline(ss, location_s);
            long location = std::stol(location_s.c_str());

            data.seekg(location);
            data.read((char*)&p, sizeof(struct Exoplanet));
            plna.push_back(planetname);
            plsy.push_back(p);
            found = true;
        }
    }
    data.close();
    lut.close();


    if (found && !isnan(p.POSITIONX) && !p.BINARY )
    {
        Time epoch;
        double parsec = 0.308567756E17;

        const std::string luaTableParent = "{"
            "Name = '" + starname + "',"
            "Parent = 'SolarSystemBarycenter',"
            "Transform = {"
                "Translation = {"
                    "Type = 'StaticTranslation',"
                    "Position = {" + std::to_string(p.POSITIONX * parsec) + ", " + std::to_string(p.POSITIONY * parsec) + ", " + std::to_string(p.POSITIONZ * parsec) + "}"
                "}"
            "}"
        "}";

        if (isnan(p.RSTAR))
        {
            p.RSTAR = 1.46046;
        }
        
        const std::string luaTableStarGlare = "{"
            "Name = '" + starname + "Plane',"
            "Parent = '" + starname + "',"
            "Renderable = {"
                "Type = 'RenderableGlobe',"
                "Radii = " + std::to_string(p.RSTAR) + " * 6.957E8,"
                "SegmentsPerPatch = 64,"
                "PerformShading = false,"
                "Layers = {"
                    "ColorLayers = {"
                        "{"
                        "Name = 'Star Texture',"
                        "FilePath = 'C:/Users/Karin/Documents/OpenSpace/modules/exoplanets/test3.jpg'," // adapt texture according to strar-temperature (TEFF)
                        "Enabled = true"
                        "}"
                    "}"
                "}"
                //"Type = 'RenderablePlaneImageLocal',"
                //"Size = " + std::to_string(p.RSTAR) + " * 6.95700E8," //RSTAR. in meters. 1 solar radii = 6.95700×10e8 m
                //"Billboard = true,"
                //"Texture = 'C:/Users/Karin/Documents/OpenSpace/modules/exoplanets/halo.png',"
                //"BlendMode = 'Additive'"
            "}"
        "}";

        std::string scriptParent = "openspace.addSceneGraphNode(" + luaTableParent + "); openspace.addSceneGraphNode(" + luaTableStarGlare + ");";

        for (size_t i = 0; i < plsy.size(); i++)
        {
            
            if (isnan(plsy[i].R))
            {
                plsy[i].R = 0.320116;
            }
            if (isnan(plsy[i].ECC))
            {
                plsy[i].ECC = 0.0585235;
            }
            if (isnan(plsy[i].A))
            {
                plsy[i].A = 0.435568;
            }
            if (isnan(plsy[i].I))
            {
                plsy[i].I = 90.00;
            }
            if (isnan(plsy[i].BIGOM))
            {
                plsy[i].BIGOM = 45;
            }
            if (isnan(plsy[i].OM))
            {
                plsy[i].OM = 90;
            }
            if (isnan(plsy[i].PER))
            {
                plsy[i].PER = 358.802;
            }
            std::string sepoch;
            if (!isnan(plsy[i].TT)) {
                epoch.setTime("JD " + std::to_string(plsy[i].TT));
                sepoch = epoch.ISO8601();
            }
            else
                sepoch = "2009-05-19T07:11:34.080";


            const std::string luaTablePlanet = "{"
                "Name = '" + plna[i] + "',"
                "Parent = '" + starname + "',"
                "Renderable = {"
                    "Type = 'RenderableGlobe',"
                    "Radii = " + std::to_string(plsy[i].R) + " *7.1492E7," //R. in meters. 1 jupiter radii = 7.1492×10e7 m
                    "SegmentsPerPatch = 64,"
                    "PerformShading = false,"
                    "Layers = {"
                        "ColorLayers = {"
                            "{"
                                "Name = 'Exoplanet Texture',"
                                "FilePath = 'C:/Users/Karin/Documents/OpenSpace/modules/exoplanets/test3.jpg',"
                                "Enabled = true"
                            "}"
                        "}"
                    "}"
                "},"
                "Transform = {"
                    "Translation = {"
                        "Type = 'KeplerTranslation',"
                        "Eccentricity = " + std::to_string(plsy[i].ECC) + "," //ECC
                        "SemiMajorAxis = " + std::to_string(plsy[i].A) + " * 149597871," // 149 597 871km = 1 AU. A
                        "Inclination = " + std::to_string(plsy[i].I) + "," //I
                        "AscendingNode  = " + std::to_string(plsy[i].BIGOM) + "," //BIGOM
                        "ArgumentOfPeriapsis  = " + std::to_string(plsy[i].OM) + "," //OM
                        "MeanAnomaly = 0.0,"
                        "Epoch = '" + sepoch + "'," //TT. JD to YYYY MM DD hh:mm:ss
                        "Period = " + std::to_string(plsy[i].PER) + "* 86400" //PER. 86 400sec = 1 day.
                    "}"
                "},"
            "}";

            const std::string PlanetTrail = "{"
                "Name = '" + plna[i] + "Trail',"
                "Parent = '" + starname + "',"
                "Renderable = {"
                    "Type = 'RenderableTrailOrbit',"
                    "Period = " + std::to_string(plsy[i].PER) + ","
                    "Resolution = 100,"
                    "Translation = {"
                        "Type = 'KeplerTranslation',"
                        "Eccentricity = " + std::to_string(plsy[i].ECC) + "," //ECC
                        "SemiMajorAxis = " + std::to_string(plsy[i].A) + " * 149597871," // 149 597 871km = 1 AU. A
                        "Inclination = " + std::to_string(plsy[i].I) + "," //I
                        "AscendingNode  = " + std::to_string(plsy[i].BIGOM) + "," //BIGOM
                        "ArgumentOfPeriapsis  = " + std::to_string(plsy[i].OM) + "," //OM
                        "MeanAnomaly = 0.0,"
                        "Epoch = '" + sepoch + "'," //TT. JD to YYYY MM DD hh:mm:ss
                        "Period = " + std::to_string(plsy[i].PER) + "* 86400" //PER. 86 400sec = 1 day.
                    "},"
                    "Color = { 1, 0, 0 }"
                "},"
            "}";

            scriptParent += "openspace.addSceneGraphNode(" + luaTablePlanet + "); openspace.addSceneGraphNode(" + PlanetTrail + ");";

        }


        OsEng.scriptEngine().queueScript(
            scriptParent,
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );

    }
    else
    {
        printf("No star with that name or not enough data about it.");
    }

    return 0;
}

int removeNode(lua_State* L) {
    const int StringLocation = -1;
    const std::string starname = luaL_checkstring(L, StringLocation);

    std::ifstream lut("C:/Users/Karin/Documents/OpenSpace/modules/exoplanets/lookup.txt");
    if (!lut.good()) {
        std::cout << "Failed to open exoplanets look-up table file";
    }
    std::string line;
    std::string planetname;
    std::vector<std::string> plna;
    while (getline(lut, line)) {

        std::istringstream ss(line);
        getline(ss, planetname, ',');

        if (planetname.compare(0, planetname.length() - 2, starname) == 0) {
            plna.push_back(planetname);
        }
    }

    std::string scriptParent;
    for (size_t i = 0; i < plna.size(); i++)
    {
        scriptParent += "openspace.removeSceneGraphNode('" + plna[i] + "Trail'); openspace.removeSceneGraphNode('" + plna[i] + "');";
    }
    scriptParent += " openspace.removeSceneGraphNode('" + starname + "Plane'); openspace.removeSceneGraphNode('" + starname + "');";
    OsEng.scriptEngine().queueScript(
        scriptParent,
        openspace::scripting::ScriptEngine::RemoteScripting::Yes
    );

    return 0;
}

scripting::LuaLibrary ExoplanetsModule::luaLibrary() const {

    scripting::LuaLibrary res;
    res.name = "exoplanets";
    res.functions = {
        {
            "addNode",
            &addNode,
            {},
            "string",
            "Adds two nodes to the scenegraph, one position node and one node to represenet the star."
        },
        {
            "removeNode",
            &removeNode,
            {},
            "string",
            "Removes the node with the name given in the arguments."
        }

    };

    return res;
}

void ExoplanetsModule::internalInitialize(const ghoul::Dictionary&) {
    
    auto fTask = FactoryManager::ref().factory<Task>();
    ghoul_assert(fTask, "No task factory existed");
    fTask->registerClass<ExoplanetsCsvToBinTask>("ExoplanetsCsvToBinTask");
}

std::vector<documentation::Documentation> ExoplanetsModule::documentations() const {
    return {
        ExoplanetsCsvToBinTask::documentation()
    };
}


} // namespace openspace
