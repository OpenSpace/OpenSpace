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

//#include <openspace/rendering/renderable.h>
//#include <openspace/util/factorymanager.h>

#include <ghoul/misc/assert.h>

#include <fstream>
#include <sstream>
#include <algorithm>

struct exoplanet {
    float A;
    double AUPPER;
    double ALOWER;
    double UA;
    float BIGOM;
    float BIGOMUPPER;
    float BIGOMLOWER;
    float UBIGOM;
    float ECC;
    float ECCUPPER;
    float ECCLOWER;
    float UECC;
    float I;
    float IUPPER;
    float ILOWER;
    float UI;
    float KOI;
    int MULT; // 0 and 1 are values(boolean), -1 indicates missing value
    float OM;
    float OMUPPER;
    float OMLOWER;
    float UOM;
    double PER;
    float PERUPPER; //skrivs med e ibland
    float PERLOWER; //skrivs med e ibland
    float UPER; //skrivs med e ibland
    double R;
    double RUPPER;
    double RLOWER;
    double UR;
    float RSTAR;
    float RSTARUPPER;
    float RSTARLOWER;
    float URSTAR;
    float TEFF;
    float TEFFUPPER;
    float TEFFLOWER;
    float UTEFF;
    double TT;
    float TTUPPER;
    float TTLOWER;
    float UTT;
    float POSITIONX;
    float POSITIONY;
    float POSITIONZ;
};

namespace openspace {

ExoplanetsModule::ExoplanetsModule() : OpenSpaceModule(Name) {}

int addNode(lua_State* L) {

    const int StringLocation = -1;
    const std::string starname = luaL_checkstring(L, StringLocation); // has white spaces


    std::ifstream data("C:/Users/Karin/Documents/OpenSpace/modules/exoplanets/data.bin", std::ios::in | std::ios::binary);
    if (!data.good()) {
        std::cout << "Failed to open exoplanets data file";
    }

    std::ifstream lut("C:/Users/Karin/Documents/OpenSpace/modules/exoplanets/lut.txt");
    if (!lut.good()) {
        std::cout << "Failed to open exoplanets look-up table file";
    }

    //1. search lut for the starname and return the corresponding location
    //2. go to that location in the data file
    //3. read sizeof(exoplanet) bytes into an exoplanet object.
    std::string planetname;
    size_t len = 0;
    exoplanet p;
    std::string line;
    bool found = false;
    while (getline(lut, line)) {

        std::istringstream ss(line);
        getline(ss, planetname, ',');

        if (planetname.compare(0, starname.length(), starname) == 0) {
            std::string location_s;
            getline(ss, location_s);
            long location = std::stol(location_s.c_str());

            data.seekg(location);
            data.read((char*)&p, sizeof(struct exoplanet));

            found = true;
        }
    }
    data.close();
    lut.close();


    if (found && !isnan(p.POSITIONX))
    {
        
        if (isnan(p.RSTAR))
        {
            p.RSTAR = 1.46046;
        }
        if (isnan(p.R))
        {
            p.R = 0.320116;
        }
        if (isnan(p.ECC))
        {
            p.ECC = 0.0585235;
        }
        if (isnan(p.A))
        {
            p.A = 0.435568;
        }
        if (isnan(p.I))
        {
            p.I = 86.6873;
        }
        if (isnan(p.BIGOM))
        {
            p.BIGOM = 44.705;
        }
        if (isnan(p.OM))
        {
            p.OM = 90;
        }
        if (isnan(p.PER))
        {
            p.PER = 358.802;
        }


        double parsecinmeter = 3.08567758*10e16;

        const std::string luaTableParent = "{"
            "Name = '" + starname + "',"
            "Parent = 'SolarSystemBarycenter',"
            "Transform = {"
                "Translation = {"
                    "Type = 'StaticTranslation',"
                    "Position = {" + std::to_string(p.POSITIONX * parsecinmeter) + ", " + std::to_string(p.POSITIONY * parsecinmeter) + ", " + std::to_string(p.POSITIONZ * parsecinmeter) + "}"
                "}"
            "}"
        "}";
        const std::string luaTableStarGlare = "{"
            "Name = '" + starname + "Plane',"
            "Parent = '" + starname + "',"
            "Renderable = {"
                "Type = 'RenderablePlaneImageLocal',"
                "Size = "+ std::to_string(p.RSTAR) +" * 6.95700*10e8," //RSTAR. in meters. 1 solar radii = 6.95700×10e8 m
                "Billboard = true,"
                "Texture = 'C:/Users/Karin/Documents/OpenSpace/modules/exoplanets/glare.png',"
                "BlendMode = 'Additive'"
            "}"
        "}";
        const std::string luaTablePlanet = "{"
            "Name = '" + starname + "Planet',"
            "Parent = '" + starname + "',"
            "Renderable = {"
                "Type = 'RenderableGlobe',"
                "Radii = "+ std::to_string(p.R) +" *7.1492*10e7," //R. in meters. 1 jupiter radii = 7.1492×10e7 m
                "SegmentsPerPatch = 64,"
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
                     "Eccentricity = "+ std::to_string(p.ECC) +"," //ECC
                     "SemiMajorAxis = "+ std::to_string(p.A) +" * 149597871," // 149 597 871km = 1 AU. A
                     "Inclination = "+ std::to_string(p.I) +"," //I
                     "AscendingNode  = "+ std::to_string(p.BIGOM) +"," //BIGOM
                     "ArgumentOfPeriapsis  = "+ std::to_string(p.OM) +"," //OM
                     "MeanAnomaly = 0.0,"
                     "Epoch = '2010 07 14 19:34:21.8'," //TT. JD to YYYY MM DD hh:mm:ss
                     "Period = "+ std::to_string(p.PER) +" * 86400" //PER. 86 400sec = 1 day.
                 "}"
            "},"
        "}";

        const std::string scriptParent = "openspace.addSceneGraphNode(" + luaTableParent + "); openspace.addSceneGraphNode(" + luaTableStarGlare + "); openspace.addSceneGraphNode(" + luaTablePlanet + ");";
        OsEng.scriptEngine().queueScript(
            scriptParent,
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );

    }
    else
    {
        printf("No star with that name.");
    }
    
    return 0;
}

int removeNode(lua_State* L) {
    const int StringLocation = -1;
    const std::string starname = luaL_checkstring(L, StringLocation);

    const std::string scriptParent = "openspace.removeSceneGraphNode('" + starname + "Planet'); openspace.removeSceneGraphNode('" + starname + "Plane'); openspace.removeSceneGraphNode('" + starname + "');";
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
            "Removes the node with the name given in teh arguments."
        }

    };

    return res;
}

//void ExoplanetsModule::internalInitialize(const ghoul::Dictionary&) {
    //auto fRenderable = FactoryManager::ref().factory<Renderable>();
    //ghoul_assert(fRenderable, "No renderable factory existed");

    //fRenderable->registerClass<RenderableDebugPlane>("RenderableDebugPlane");
//}

//std::vector<documentation::Documentation> ExoplanetsModule::documentations() const {
    //return {
        //RenderableDebugPlane::Documentation()
    //};
//}


} // namespace openspace
