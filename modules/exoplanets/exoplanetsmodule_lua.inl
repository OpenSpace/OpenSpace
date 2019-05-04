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

#include <openspace/engine/moduleengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/engine/globals.h>
#include <openspace/scripting/scriptengine.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <ghoul/misc/assert.h>
#include <glm/gtx/transform.hpp>
#include <glm/gtc/quaternion.hpp>
#include <glm/gtx/quaternion.hpp>

#include <fstream>
#include <iostream>
#include <sstream>

namespace openspace{

std::string getStarColor(float bv) {
	std::string colorString;

	std::ifstream colormap(absPath("${MODULE_EXOPLANETS}/colorbv.cmap"), std::ios::in);
	if (!colormap.good()) {
		std::cout << "Failed to open colormap data file";
	}

	int t = round(((bv + 0.4) / (2.0 + 0.4)) * 255);

	std::string color;
	for (size_t i = 0; i < t + 12; i++)
	{
		getline(colormap, color);
	}

	std::istringstream colorstream(color);
	std::string r, g, b;
	getline(colorstream, r, ' ');
	getline(colorstream, g, ' ');
	getline(colorstream, b, ' ');
	colorString = "{" + r + ", " + g + ", " + b + "}";

	colormap.close();

	return colorString;
}

glm::dmat4 computeOrbitPlaneRotationMatrix(float i, float bigom, float om , glm::dmat3 rot) {
    // Exoplanet defined inclination changed to be used as Kepler defined inclination

    const glm::dvec3 ascendingNodeAxisRot = rot * glm::dvec3(0.f, 0.f, 1.f);
    //const glm::dvec3 ascendingNodeAxisRot = glm::dvec3(0.f, 0.f, 1.f);
    const glm::dvec3 inclinationAxisRot = rot * glm::dvec3(1.f, 0.f, 0.f );
    //const glm::dvec3 inclinationAxisRot = glm::dvec3(1.f, 0.f, 0.f );
	const glm::vec3 argPeriapsisAxisRot = rot * glm::dvec3( 0.f, 0.f, 1.f );
	//const glm::vec3 argPeriapsisAxisRot = glm::dvec3( 0.f, 0.f, 1.f );

    /*const glm::vec3 ascendingNodeAxisRot = { 0.f, 0.f, 1.f };
    const glm::vec3 inclinationAxisRot = { 1.f, 0.f, 0.f };
    const glm::vec3 argPeriapsisAxisRot = { 0.f, 0.f, 1.f };*/

	const double asc = glm::radians(bigom);
	const double inc = glm::radians(i);
	const double per = glm::radians(om);

	glm::dmat4 orbitPlaneRotation =
        glm::rotate(asc, glm::dvec3(ascendingNodeAxisRot)) *
		glm::rotate(inc, glm::dvec3(inclinationAxisRot)) *
		glm::rotate(per, glm::dvec3(argPeriapsisAxisRot));

	return orbitPlaneRotation;
	//return std::to_string(orbitPlaneRotation);
}

std::string getSpeckStarname(std::string csvName) {
    std::string explName = csvName;
    if (csvName == "HD 1237")
        explName = "GJ 3021";
    else if (csvName == "MOA-2009-BLG-387L")
        explName = "MOA 2009-BLG-387L";
    else if (csvName == "HD 126614 A")
        explName = "HD 126614";
    else if (csvName == "epsilon Ret")
        explName = "HD 27442";
    else if (csvName == "PH-1")
        explName = "PH1";
    else if (csvName == "gamma Leo A")
        explName = "gam 1 Leo";
    else if (csvName == "OGLE-2007-BLG-368L")
        explName = "OGLE 2007-BLG-368L";
    else if (csvName == "alpha Ari")
        explName = "alf Ari";
    else if (csvName == "mu Ara")
        explName = "HD 160691";
    else if (csvName == "OGLE-05-169L")
        explName = "OGLE 2005-BLG-169L";
    else if (csvName == "tau Gru")
        explName = "HD 216435";
    else if (csvName == "iota Hor")
        explName = "HR 810";
    else if (csvName == "OGLE-05-071L")
        explName = "OGLE 2005-BLG-71L";
    else if (csvName == "OGLE235-MOA53")
        explName = "OGLE 2003-BLG-235L";
    else if (csvName == "MOA-2008-BLG-310L")
        explName = "MOA 2008-BLG-310L";
    else if (csvName == "KIC 11442793")
        explName = "KOI-351";
    else if (csvName == "OGLE-2006-BLG-109L")
        explName = "OGLE 2006-BLG-109L";
    else if (csvName == "HD 137388")
        explName = "HD 137388 A";
    else if (csvName == "kappa CrB")
        explName = "kap CrB";
    else if (csvName == "XO-2")
        explName = "XO-2 N";
    else if (csvName == "epsilon Tau")
        explName = "eps Tau";
    else if (csvName == "epsilon Eri")
        explName = "eps Eri";
    else if (csvName == "Kepler-448")
        explName = "KOI-12";
    else if (csvName == "omega Ser")
        explName = "ome Ser";
    else if (csvName == "MOA-2010-BLG-477L")
        explName = "MOA 2010-BLG-477L";
    else if (csvName == "GJ 176")
        explName = "HD 285968";
    else if (csvName == "HIP 2247")
        explName = "BD-17 63";
    else if (csvName == "MOA-2009-BLG-266L")
        explName = "MOA 2009-BLG-266L";
    else if (csvName == "Kepler-89")
        explName = "KOI-94";
    else if (csvName == "iota Dra")
        explName = "HIP 75458";
    else if (csvName == "MOA-2007-BLG-400L")
        explName = "MOA 2007-BLG-400L";
    else if (csvName == "upsilon And")
        explName = "ups And";
    else if (csvName == "OGLE-2011-BLG-0251")
        explName = "OGLE 2011-BLG-251L";
    else if (csvName == "OGLE-05-390L")
        explName = "OGLE 2005-BLG-390L";
    else if (csvName == "Kepler-420")
        explName = "KOI-1257";
    else if (csvName == "beta Pic")
        explName = "bet Pic";
    else if (csvName == "gamma Cep")
        explName = "gam Cep";
    else if (csvName == "MOA-2007-BLG-192L")
        explName = "MOA 2007-BLG-192L";
    else if (csvName == "MOA-2009-BLG-319L")
        explName = "MOA 2009-BLG-319L";
    else if (csvName == "omicron CrB")
        explName = "omi CrB";
    else if (csvName == "beta Gem")
        explName = "HD 62509";
    else if (csvName == "epsilon CrB")
        explName = "eps CrB";
    else if (csvName == "omicron UMa")
        explName = "omi UMa";
    else if (csvName == "HD 142022")
        explName = "HD 142022 A";

    return explName;
}

std::string getCsvStarname(std::string explName) {
	std::string csvName = explName;
	if (explName == "GJ 3021")
		csvName = "HD 1237";
	else if (explName == "MOA 2009-BLG-387L")
		csvName = "MOA-2009-BLG-387L";
	else if (explName == "HD 126614")
		csvName = "HD 126614 A";
	else if (explName == "HD 27442")
		csvName = "epsilon Ret";
	else if (explName == "PH1")
		csvName = "PH-1";
	else if (explName == "gam 1 Leo")
		csvName = "gamma Leo A";
	else if (explName == "OGLE 2007-BLG-368L")
		csvName = "OGLE-2007-BLG-368L";
	else if (explName == "alf Ari")
		csvName = "alpha Ari";
	else if (explName == "HD 160691")
		csvName = "mu Ara";
	else if (explName == "OGLE 2005-BLG-169L")
		csvName = "OGLE-05-169L";
	else if (explName == "HD 216435")
		csvName = "tau Gru";
	else if (explName == "HR 810")
		csvName = "iota Hor";
	else if (explName == "OGLE 2005-BLG-71L")
		csvName = "OGLE-05-071L";
	else if (explName == "OGLE 2003-BLG-235L")
		csvName = "OGLE235-MOA53";
	else if (explName == "MOA 2008-BLG-310L")
		csvName = "MOA-2008-BLG-310L";
	else if (explName == "KOI-351")
		csvName = "KIC 11442793";
	else if (explName == "OGLE 2006-BLG-109L")
		csvName = "OGLE-2006-BLG-109L";
	else if (explName == "HD 137388 A")
		csvName = "HD 137388";
	else if (explName == "kap CrB")
		csvName = "kappa CrB";
	else if (explName == "XO-2 N")
		csvName = "XO-2";
	else if (explName == "eps Tau")
		csvName = "epsilon Tau";
	else if (explName == "eps Eri")
		csvName = "epsilon Eri";
	else if (explName == "KOI-12")
		csvName = "Kepler-448";
	else if (explName == "ome Ser")
		csvName = "omega Ser";
	else if (explName == "MOA 2010-BLG-477L")
		csvName = "MOA-2010-BLG-477L";
	else if (explName == "HD 285968")
		csvName = "GJ 176";
	else if (explName == "BD-17 63")
		csvName = "HIP 2247";
	else if (explName == "MOA 2009-BLG-266L")
		csvName = "MOA-2009-BLG-266L";
	else if (explName == "KOI-94")
		csvName = "Kepler-89";
	else if (explName == "HIP 75458")
		csvName = "iota Dra";
	else if (explName == "MOA 2007-BLG-400L")
		csvName = "MOA-2007-BLG-400L";
	else if (explName == "ups And")
		csvName = "upsilon And";
	else if (explName == "OGLE 2011-BLG-251L")
		csvName = "OGLE-2011-BLG-0251";
	else if (explName == "OGLE 2005-BLG-390L")
		csvName = "OGLE-05-390L";
	else if (explName == "KOI-1257")
		csvName = "Kepler-420";
	else if (explName == "bet Pic")
		csvName = "beta Pic";
	else if (explName == "gam Cep")
		csvName = "gamma Cep";
	else if (explName == "MOA 2007-BLG-192L")
		csvName = "MOA-2007-BLG-192L";
	else if (explName == "MOA 2009-BLG-319L")
		csvName = "MOA-2009-BLG-319L";
	else if (explName == "omi CrB")
		csvName = "omicron CrB";
	else if (explName == "HD 62509")
		csvName = "beta Gem";
	else if (explName == "eps CrB")
		csvName = "epsilon CrB";
	else if (explName == "omi UMa")
		csvName = "omicron UMa";
	else if (explName == "HD 142022 A")
		csvName = "HD 142022";

	return csvName;
}

// Rotate the original coordinate system (where x is pointing to First Point of Aries)
// so that x is pointing from star to the sun.
// Modified from http://www.opengl-tutorial.org/intermediate-tutorials/tutorial-17-quaternions/#how-do-i-find-the-rotation-between-2-vectors- 
glm::dmat3 getExoplanetSystemRotation(glm::dvec3 start, glm::dvec3 end) {

    glm::quat rotInQuat;
    //glm::dvec3 oldXVec = glm::dvec3(1.0, 0.0, 0.0); //parent nod x-vec
    //glm::dvec3 starToSunVec = normalize(glm::dvec3(0.0, 0.0, 0.0) - end);

    float cosTheta = dot(start, end);
    glm::dvec3 rotationAxis;

    if (cosTheta < -1 + 0.001f) {
        // special case when vectors in opposite directions:
        // there is no "ideal" rotation axis
        // So guess one; any will do as long as it's perpendicular to startvector(oldXVec)
        rotationAxis = cross(glm::dvec3(0.0f, 0.0f, 1.0f), start);
        if (length2(rotationAxis) < 0.01) // bad luck, they were parallel, try again!
            rotationAxis = cross(glm::dvec3(1.0f, 0.0f, 0.0f), start);

        rotationAxis = normalize(rotationAxis);
        rotInQuat = glm::quat(glm::radians(180.0f), rotationAxis);
        return glm::dmat3(toMat4(rotInQuat));
    }

    rotationAxis = cross(start, end);

    float s = sqrt((1 + cosTheta) * 2);
    float invs = 1 / s;

    rotInQuat = glm::quat(
        s * 0.5f,
        rotationAxis.x * invs,
        rotationAxis.y * invs,
        rotationAxis.z * invs
    );

    return glm::dmat3(toMat4(rotInQuat));
}

int addExoplanetSystem(lua_State* L) {

	const int StringLocation = -1;
	const std::string starname = luaL_checkstring(L, StringLocation);

    //change expl-starname to exoplanet.csv-starname
    std::string starname_csv = getCsvStarname(starname);
    // If user have given name as in EOD, change it to speck-name
    std::string starname_speck = getSpeckStarname(starname);
    std::replace(starname_speck.begin(), starname_speck.end(), ' ', '_');

    global::moduleEngine.module<ExoplanetsModule>()->setStarName(starname_speck);

	std::ifstream data(absPath("${MODULE_EXOPLANETS}/expl_data.bin"), std::ios::in | std::ios::binary);
	if (!data.good()) {
		std::cout << "Failed to open exoplanets data file";
	}

	std::ifstream lut(absPath("${MODULE_EXOPLANETS}/lookup.txt"));
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

		if (planetname.compare(0, planetname.length() - 2, starname_speck) == 0) {
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
    global::moduleEngine.module<ExoplanetsModule>()->setPlna(plna);
    global::moduleEngine.module<ExoplanetsModule>()->setPlsy(plsy);
    global::moduleEngine.module<ExoplanetsModule>()->setClosestExoplanet(p);

	if (found && !isnan(p.POSITIONX) && !isnan(p.A) && !isnan(p.PER)) //&& !p.BINARY
	{
		Time epoch;
		double parsec = 0.308567756E17;
		std::string script = "";

        glm::dvec3 position = glm::dvec3(p.POSITIONX * parsec , p.POSITIONY * parsec, p.POSITIONZ * parsec);
        
        glm::dvec3 starToSunVec = normalize(glm::dvec3(0.0, 0.0, 0.0) - position);
        glm::dvec3 galacticNorth = glm::dvec3(0.0, 0.0, 1.0);

        glm::dmat3 galaxticToCelectialMatrix = SpiceManager::ref().positionTransformMatrix( // galaxtic north in celectial coordinates, or just celectial north?
            "GALACTIC",
            "J2000",
            0.0
        );
        glm::dvec3 celestialNorth = normalize(galaxticToCelectialMatrix * galacticNorth);


        // Earths north vector projected onto the skyplane, the plane perpendicular to the viewing vector (starTosunVec)
        glm::dvec3 northProjected = normalize(celestialNorth - (((dot(celestialNorth, starToSunVec)) / (glm::length(starToSunVec)))*starToSunVec));
        global::moduleEngine.module<ExoplanetsModule>()->setNorthVector(northProjected);

        glm::dvec3 beta = normalize(cross(starToSunVec, northProjected));

        const glm::dmat3 exoplanetSystemRot = glm::dmat3(northProjected.x, northProjected.y, northProjected.z,
                                                            beta.x, beta.y, beta.z,
                                                    starToSunVec.x, starToSunVec.y, starToSunVec.z);

        std::replace(starname_speck.begin(), starname_speck.end(), ' ', '_');


		const std::string starParent = "{"
			"Identifier = '" + starname_speck + "',"
			"Parent = 'SolarSystemBarycenter'," 
			"Transform = {"
                "Rotation = {"
                    "Type = 'StaticRotation',"
                    "Rotation = " + ghoul::to_string(exoplanetSystemRot) + ","
                "},"
				"Translation = {"
					"Type = 'StaticTranslation',"
					"Position = " + ghoul::to_string(position) + ","
				"},"
			"}"
		"}";

		script = "openspace.addSceneGraphNode(" + starParent + ");";
        openspace::global::scriptEngine.queueScript(
            script,
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
		if (!isnan(p.RSTAR))
		{
			std::string color = getStarColor(p.BMV);

            if (isnan(plsy[0].ECC))
            {
                plsy[0].ECC = 0;
            }
            if (isnan(plsy[0].I))
            {
                plsy[0].I = 90;
            }
            if (isnan(plsy[0].BIGOM))
            {
                plsy[0].BIGOM = 180;
            }
            if (isnan(plsy[0].OM))
            {
                plsy[0].OM = 90;
            }
            std::string sepoch_star;
            if (!isnan(plsy[0].TT)) {
                epoch.setTime("JD " + std::to_string(plsy[0].TT));
                sepoch_star = epoch.ISO8601();
            }
            else
                sepoch_star = "2009-05-19T07:11:34.080";

			const std::string starGlobe = "{"
				"Identifier = '" + starname_speck + "Globe',"
				"Parent = '" + starname_speck + "',"
				"Renderable = {"
					"Type = 'RenderableGlobe',"
					"Radii = " + std::to_string(p.RSTAR) + " * 6.957E8,"
					"SegmentsPerPatch = 64,"
					"PerformShading = false,"
					"Layers = {"
						"ColorLayers = {"
							"{"
								"Identifier = 'StarColor',"
								"Type = 'SolidColor',"
								"Color = " + color + ","
								"BlendMode = 'Normal',"
								"Enabled = true"
							"},"
							"{"
								"Identifier = 'StarTexture',"
								"FilePath = openspace.absPath('${MODULE_EXOPLANETS}/sun.jpg'),"
								"BlendMode = 'Color',"
								"Enabled = true"
							"}"
						"}"
					"}"
				"},"
				"Transform = {"
					"Scale = {"
						"Type = 'StaticScale',"
						"Scale = 1.0,"
					"},"
                    "Translation = {"
                        "Type = 'KeplerTranslation',"
                        "Eccentricity = " + std::to_string(plsy[0].ECC) + "," //ECC
                        "SemiMajorAxis = 0," // 149 597 871km = 1 AU. A
                        "Inclination = " + std::to_string(plsy[0].I) + "," //I
                        "AscendingNode  = " + std::to_string(plsy[0].BIGOM) + "," //BIGOM
                        "ArgumentOfPeriapsis  = " + std::to_string(plsy[0].OM) + "," //OM
                        "MeanAnomaly = 180.0,"
                        "Epoch = '" + sepoch_star + "'," //TT. JD to YYYY MM DD hh:mm:ss
                        "Period = " + std::to_string(plsy[0].PER) + "* 86400" //PER. 86 400sec = 1 day.
                    "}"
				"}"
			"}";
            script = "";
            script = " openspace.addSceneGraphNode(" + starGlobe + ");";
            openspace::global::scriptEngine.queueScript(
                script,
                openspace::scripting::ScriptEngine::RemoteScripting::Yes
            );
            //script = "";

			//const std::string starGlare = "{"
			//	"Identifier = '" + starname_speck + "Glare',"
			//	"Parent = '" + starname_speck + "',"
			//	"Renderable = {"
			//	"Type = 'RenderablePlaneImageLocal',"
			//	"Size = " + std::to_string(p.RSTAR) + " * 5E9," //RSTAR. in meters. 1 solar radii = 6.95700×10e8 m
			//	"Billboard = true,"
			//	"Texture = 'C:/Users/Karin/Documents/OpenSpace/modules/exoplanets/target-blue.png',"
			//	"BlendMode = 'Additive'"
			//	"}"
			//	"}";
            //
			//script = "openspace.addSceneGraphNode(" + starGlare + ");";
            //global::scriptEngine.queueScript(
            //    script,
            //    scripting::ScriptEngine::RemoteScripting::Yes
            //);
		}

		


        for (size_t i = 0; i < plsy.size(); i++)
        {
            script = "";

            if (isnan(plsy[i].ECC))
            {
                plsy[i].ECC = 0;
            }
            if (isnan(plsy[i].I))
            {
                plsy[i].I = 90;
            }
            if (isnan(plsy[i].BIGOM))
            {
                plsy[i].BIGOM = 180;
            }
            if (isnan(plsy[i].OM))
            {
                plsy[i].OM = 90;
            }
            std::string sepoch;
            if (!isnan(plsy[i].TT)) {
                epoch.setTime("JD " + std::to_string(plsy[i].TT));
                sepoch = epoch.ISO8601();
            }
            else
                sepoch = "2009-05-19T07:11:34.080";

            float planetradius;
            std::string enabled = "";
            if (isnan(plsy[i].R))
            {
                if (isnan(plsy[i].RSTAR)) {
                    planetradius = plsy[i].A * 149597870700 * 0.001;
                }
                else {
                    planetradius = plsy[i].RSTAR *6.95700E8 *0.1;
                }

                enabled = "false";

            }
            else {
                planetradius = plsy[i].R *7.1492E7;
                enabled = "true";
            }

            const std::string planet = "{"
                "Identifier = '" + plna[i] + "',"
                "Parent = '" + starname_speck + "',"
                "Enabled = true,"
                "Renderable = {"
                    "Type = 'RenderableGlobe',"
                    "Enabled = " + enabled + ","
                    "Radii = " + std::to_string(planetradius) + "," //R. in meters. 1 jupiter radii = 7.1492×10e7 m
                    "SegmentsPerPatch = 64,"
                    "PerformShading = false,"
                    "Layers = {"
                        "ColorLayers = {"
                            "{"
                                "Identifier = 'ExoplanetTexture',"
                                "FilePath = openspace.absPath('${DATA}/test3.jpg'),"
                                "Enabled = true"
                            "}"
                        "}"
                    "}"
                "},"
                "Transform = {"
                    "Scale = {"
                        "Type = 'StaticScale',"
                        "Scale = 1.0,"
                    "},"
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
                "},"
            "}";

            script = "openspace.addSceneGraphNode(" + planet + ");";
            openspace::global::scriptEngine.queueScript(
                script,
                openspace::scripting::ScriptEngine::RemoteScripting::Yes
            );
            script = "";




            const std::string planetTrail = "{"
                "Identifier = '" + plna[i] + "Trail',"
                "Parent = '" + starname_speck + "',"
                "Enabled = true,"
                "Renderable = {"
                    "Type = 'RenderableTrailOrbit',"
                    "Period = " + std::to_string(plsy[i].PER) + ","
                    "Resolution = 1000,"
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
                    "Color = { 1, 1, 1 }"
                "},"
			"}";

			openspace::global::scriptEngine.queueScript(
				"openspace.addSceneGraphNode(" + planetTrail + ");",
				openspace::scripting::ScriptEngine::RemoteScripting::Yes
			);

			if (!isnan(plsy[i].AUPPER) && !isnan(plsy[i].ALOWER))
			{
                // Get the orbit plane that the trail orbit and planet have from the KeplerTranslation
                glm::dmat4 orbitPlaneRotationMatrix = computeOrbitPlaneRotationMatrix(plsy[i].I, plsy[i].BIGOM, plsy[i].OM, exoplanetSystemRot);
                //glm::dmat4 orbitPlaneRotationMatrix = computeOrbitPlaneRotationMatrix(plsy[i].I, plsy[i].BIGOM, plsy[i].OM); // , exoplanetSystemRot);
                glm::dmat3 rot = orbitPlaneRotationMatrix;
                global::moduleEngine.module<ExoplanetsModule>()->setRotation(rot);
				const std::string disc = "{"
					"Identifier = '" + plna[i] + "Disc',"
					"Parent = '" + starname_speck + "',"
                    "Enabled = true,"
					"Renderable = {"
					    "Type = 'RenderableOrbitdisc',"
					    "Texture = openspace.absPath('${MODULE_EXOPLANETS}/disc3.png'),"
					    "Size = " + std::to_string(plsy[i].A) + " * 149597870700," // 149 597 870 700 m = 1 AU. A
					    "Eccentricity = " + std::to_string(plsy[i].ECC) + ","
					    "Offset = { " + std::to_string(plsy[i].ALOWER) + ", " + std::to_string(plsy[i].AUPPER) + " }," //min / max extend
					    "Transparency = 0.5"
					"},"
					"Transform = {"
					    "Rotation = {"
					        "Type = 'StaticRotation',"
					        "Rotation =  " + ghoul::to_string(rot) + ","
					    "}"
					"},"
				"}";
				openspace::global::scriptEngine.queueScript(
					"openspace.addSceneGraphNode(" + disc + ");",
					openspace::scripting::ScriptEngine::RemoteScripting::Yes
				);


				if (!isnan(plsy[i].ECCUPPER) && !isnan(plsy[i].ECCLOWER) && plsy[i].ECCUPPER > 0.0 && plsy[i].ECCLOWER > 0.0)
				{
					double lower_ecc = plsy[i].ECC - plsy[i].ECCLOWER;
					if (lower_ecc < 0.0)
					{
						lower_ecc = 0.0;
					}
					const std::string discECCLOWER = "{"
						"Identifier = '" + plna[i] + "discECCLOWER',"
						"Parent = '" + starname_speck + "',"
						"Renderable = {"
						    "Type = 'RenderableOrbitdisc',"
						    "Texture = openspace.absPath('${MODULE_EXOPLANETS}/discL.png'),"
						    "Size = " + std::to_string(plsy[i].A) + " * 149597870700," // 149 597 870 700 m = 1 AU. A
						    "Eccentricity = " + std::to_string(lower_ecc) + ","
						    "Offset = { " + std::to_string(plsy[i].ALOWER) + ", " + std::to_string(plsy[i].AUPPER) + " }," //min / max extend
						    "Transparency = 0.98,"
						    "Enabled = false"
						"},"
						"Transform = {"
						    "Rotation = {"
						        "Type = 'StaticRotation',"
                                "Rotation =  " + ghoul::to_string(rot) + ","
						    "}"
						"},"
					"}";


					openspace::global::scriptEngine.queueScript(
						"openspace.addSceneGraphNode(" + discECCLOWER + ");",
						openspace::scripting::ScriptEngine::RemoteScripting::Yes
					);

					double upper_ecc = plsy[i].ECC + plsy[i].ECCUPPER;
					if (upper_ecc > 1.0)
					{
						upper_ecc = 1.0;
					}
					const std::string discECCUPPER = "{"
						"Identifier = '" + plna[i] + "discECCUPPER',"
						"Parent = '" + starname_speck + "',"
						"Renderable = {"
						    "Type = 'RenderableOrbitdisc',"
						    "Texture = openspace.absPath('${MODULE_EXOPLANETS}/discU.png'),"
						    "Size = " + std::to_string(plsy[i].A) + " * 149597870700," // 149 597 870 700 m = 1 AU. A
						    "Eccentricity = " + std::to_string(upper_ecc) + ","
						    "Offset = { " + std::to_string(plsy[i].ALOWER) + ", " + std::to_string(plsy[i].AUPPER) + " }," //min / max extend
						    "Transparency = 0.98,"
						    "Enabled = false"
						"},"
						"Transform = {"
						    "Rotation = {"
						        "Type = 'StaticRotation',"
                                "Rotation =  " + ghoul::to_string(rot) + ","
						    "}"
						"},"
					"}";


					openspace::global::scriptEngine.queueScript(
						"openspace.addSceneGraphNode(" + discECCUPPER + ");",
						openspace::scripting::ScriptEngine::RemoteScripting::Yes
					);
				}
			}

		}


	}
	else
	{
		printf("No star with that name or not enough data about it.");
	}


	return 0;
}

int removeExoplanetSystem(lua_State* L) {
	const int StringLocation = -1;
	const std::string starname = luaL_checkstring(L, StringLocation);
    std::string starname_speck = getSpeckStarname(starname);
    std::replace(starname_speck.begin(), starname_speck.end(), ' ', '_');
	std::string script = "openspace.removeSceneGraphNode('" + starname_speck + "');";
	openspace::global::scriptEngine.queueScript(
		script,
		scripting::ScriptEngine::RemoteScripting::Yes
	);

	return 0;
}
} //namespace
