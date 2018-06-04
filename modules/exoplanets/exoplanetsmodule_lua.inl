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

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/moduleengine.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <glm/gtx/transform.hpp>

#include <fstream>

namespace openspace{

std::string getStarColor(float bv) {
	std::string colorString;

	std::ifstream colormap(absPath("${BASE}/modules/exoplanets/colorbv.cmap"), std::ios::in);
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

std::string computeRotationMatrix(float i, float bigom, float om) {

	const glm::vec3 ascendingNodeAxisRot = { 0.f, 1.f, 0.f };
	const glm::vec3 inclinationAxisRot = { 1.f, 0.f, 0.f };
	const glm::vec3 argPeriapsisAxisRot = { 0.f, 0.f, 1.f };

	const double asc = glm::radians(bigom);
	const double inc = glm::radians(i);
	const double per = glm::radians(om);

	glm::dmat3 orbitPlaneRotation =
		glm::rotate(inc, glm::dvec3(inclinationAxisRot)) *
		glm::rotate(asc, glm::dvec3(ascendingNodeAxisRot)) *
		glm::rotate(per, glm::dvec3(argPeriapsisAxisRot));

	return std::to_string(orbitPlaneRotation);

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


int addExoplanetSystem(lua_State* L) {

	const int StringLocation = -1;
	const std::string starname = luaL_checkstring(L, StringLocation);

    OsEng.moduleEngine().module<ExoplanetsModule>()->setStarName(starname);

	//change expl-starname to exoplanet.csv-starname
	std::string starname_csv = getCsvStarname(starname);

	std::ifstream data(absPath("${BASE}/modules/exoplanets/expl_data.bin"), std::ios::in | std::ios::binary);
	if (!data.good()) {
		std::cout << "Failed to open exoplanets data file";
	}

	std::ifstream lut(absPath("${BASE}/modules/exoplanets/lookup.txt"));
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

		if (planetname.compare(0, planetname.length() - 2, starname_csv) == 0) {
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

    OsEng.moduleEngine().module<ExoplanetsModule>()->setClosestExoplanet(p);

	if (found && !isnan(p.POSITIONX) && !isnan(p.A) && !isnan(p.PER)) //&& !p.BINARY
	{
		Time epoch;
		double parsec = 0.308567756E17;
		std::string script = "";

		const std::string starParent = "{"
			"Identifier = '" + starname + "',"
			"Parent = 'SolarSystemBarycenter',"
			"Transform = {"
				"Translation = {"
					"Type = 'StaticTranslation',"
					"Position = {" + std::to_string(p.POSITIONX * parsec) + ", " + std::to_string(p.POSITIONY * parsec) + ", " + std::to_string(p.POSITIONZ * parsec) + "}"
				"}"
			"}"
		"}";
		script = "openspace.addSceneGraphNode(" + starParent + ");";

		if (!isnan(p.RSTAR))
		{
			std::string color = getStarColor(p.BMV);

            if (isnan(p.ECC))
            {
                p.ECC = 0;
            }
            if (isnan(p.I))
            {
                p.I = 90;
            }
            if (isnan(p.BIGOM))
            {
                p.BIGOM = 0;
            }
            if (isnan(p.OM))
            {
                p.OM = 90;
            }
            std::string sepoch_star;
            if (!isnan(p.TT)) {
                epoch.setTime("JD " + std::to_string(p.TT));
                sepoch_star = epoch.ISO8601();
            }
            else
                sepoch_star = "2009-05-19T07:11:34.080";
			const std::string starGlobe = "{"
				"Identifier = '" + starname + "Globe',"
				"Parent = '" + starname + "',"
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
								"FilePath = 'C:/Users/Karin/Documents/OpenSpace/modules/exoplanets/sun.jpg',"
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
                        "Eccentricity = " + std::to_string(p.ECC) + "," //ECC
                        "SemiMajorAxis = 0," // 149 597 871km = 1 AU. A
                        "Inclination = " + std::to_string(p.I) + "," //I
                        "AscendingNode  = " + std::to_string(p.BIGOM) + "," //BIGOM
                        "ArgumentOfPeriapsis  = " + std::to_string(p.OM) + "," //OM
                        "MeanAnomaly = 0.0,"
                        "Epoch = '" + sepoch_star + "'," //TT. JD to YYYY MM DD hh:mm:ss
                        "Period = " + std::to_string(p.PER) + "* 86400" //PER. 86 400sec = 1 day.
                    "}"
				"}"
			"}";

            script += " openspace.addSceneGraphNode(" + starGlobe + ");";
            OsEng.scriptEngine().queueScript(
                script,
                openspace::scripting::ScriptEngine::RemoteScripting::Yes
            );
            script = "";

			const std::string starGlare = "{"
				"Identifier = '" + starname + "Glare',"
				"Parent = '" + starname + "',"
				"Renderable = {"
				"Type = 'RenderablePlaneImageLocal',"
				"Size = " + std::to_string(p.RSTAR) + " * 5E9," //RSTAR. in meters. 1 solar radii = 6.95700×10e8 m
				"Billboard = true,"
				"Texture = 'C:/Users/Karin/Documents/OpenSpace/modules/exoplanets/halo.png',"
				"BlendMode = 'Additive'"
				"}"
				"}";

			script = "openspace.addSceneGraphNode(" + starGlare + ");";
            //OsEng.scriptEngine().queueScript(
            //    script,
           //     openspace::scripting::ScriptEngine::RemoteScripting::Yes
           // );
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
				plsy[i].BIGOM = 0;
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

			if (!isnan(plsy[i].R))
			{
				const std::string planet = "{"
					"Identifier = '" + plna[i] + "',"
					"Parent = '" + starname + "',"
					"Renderable = {"
						"Type = 'RenderableGlobe',"
						"Radii = " + std::to_string(plsy[i].R) + " *7.1492E7," //R. in meters. 1 jupiter radii = 7.1492×10e7 m
						"SegmentsPerPatch = 64,"
						"PerformShading = false,"
						"Layers = {"
							"ColorLayers = {"
								"{"
									"Identifier = 'ExoplanetTexture',"
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

				script = "openspace.addSceneGraphNode(" + planet + ");";
				OsEng.scriptEngine().queueScript(
					script,
					openspace::scripting::ScriptEngine::RemoteScripting::Yes
				);
				script = "";

			}


			const std::string planetTrail = "{"
				"Identifier = '" + plna[i] + "Trail',"
				"Parent = '" + starname + "',"
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
				"Color = { 1, 0, 0 }"
				"},"
				"}";

			OsEng.scriptEngine().queueScript(
				"openspace.addSceneGraphNode(" + planetTrail + ");",
				openspace::scripting::ScriptEngine::RemoteScripting::Yes
			);

			if (!isnan(plsy[i].AUPPER) && !isnan(plsy[i].ALOWER))
			{
				std::string rotation_matrix = computeRotationMatrix(plsy[i].I, plsy[i].BIGOM, plsy[i].OM);

				const std::string disc = "{"
					"Identifier = '" + plna[i] + "Disc',"
					"Parent = '" + starname + "',"
					"Renderable = {"
					"Type = 'RenderableOrbitdisc',"
					"Texture = 'C:/Users/Karin/Documents/OpenSpace/modules/exoplanets/disc.png',"
					"Size = " + std::to_string(plsy[i].A) + " * 149597870700," // 149 597 870 700 m = 1 AU. A
					"Eccentricity = " + std::to_string(plsy[i].ECC) + ","
					"Offset = { " + std::to_string(plsy[i].ALOWER) + ", " + std::to_string(plsy[i].AUPPER) + " }," //min / max extend
					"Transparency = 0.99"
					"},"
					"Transform = {"
					"Rotation = {"
					"Type = 'StaticRotation',"
					"Rotation =  " + rotation_matrix + ","
					"}"
					"},"
					"}";
				OsEng.scriptEngine().queueScript(
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
						"Parent = '" + starname + "',"
						"Renderable = {"
						"Type = 'RenderableOrbitdisc',"
						"Texture = 'C:/Users/Karin/Documents/OpenSpace/modules/exoplanets/discL.png',"
						"Size = " + std::to_string(plsy[i].A) + " * 149597870700," // 149 597 870 700 m = 1 AU. A
						"Eccentricity = " + std::to_string(lower_ecc) + ","
						"Offset = { " + std::to_string(plsy[i].ALOWER) + ", " + std::to_string(plsy[i].AUPPER) + " }," //min / max extend
						"Transparency = 0.98,"
						"Enabled = false"
						"},"
						"Transform = {"
						"Rotation = {"
						"Type = 'StaticRotation',"
						"Rotation =  " + rotation_matrix + ","
						"}"
						"},"
						"}";


					OsEng.scriptEngine().queueScript(
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
						"Parent = '" + starname + "',"
						"Renderable = {"
						"Type = 'RenderableOrbitdisc',"
						"Texture = 'C:/Users/Karin/Documents/OpenSpace/modules/exoplanets/discU.png',"
						"Size = " + std::to_string(plsy[i].A) + " * 149597870700," // 149 597 870 700 m = 1 AU. A
						"Eccentricity = " + std::to_string(upper_ecc) + ","
						"Offset = { " + std::to_string(plsy[i].ALOWER) + ", " + std::to_string(plsy[i].AUPPER) + " }," //min / max extend
						"Transparency = 0.98,"
						"Enabled = false"
						"},"
						"Transform = {"
						"Rotation = {"
						"Type = 'StaticRotation',"
						"Rotation =  " + rotation_matrix + ","
						"}"
						"},"
						"}";


					OsEng.scriptEngine().queueScript(
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

	std::string script = "openspace.removeSceneGraphNode('" + starname + "');";
	OsEng.scriptEngine().queueScript(
		script,
		openspace::scripting::ScriptEngine::RemoteScripting::Yes
	);

	return 0;
}
} //namespace
