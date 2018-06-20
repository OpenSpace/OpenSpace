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

#include <modules/exoplanets/discoverymethods/discoverymethods.h>
#include <modules/exoplanets/exoplanetsmodule.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <openspace/scene/scene.h>

namespace {
	constexpr const char* _loggerCat = "DiscoveryMethods";

	static const openspace::properties::Property::PropertyInfo TransitMethodInfo = {
		"TransitMethod",
		"Show transit method",
		"Change the view so that the transit method can be presented."
	};
	
	static const openspace::properties::Property::PropertyInfo DopplerMethodInfo = {
		"DopplerMethod",
		"Show doppler method",
		"Change the view so that the doppler method can be presented."
	};
} // namespace

namespace openspace::exoplanets{

    void DiscoveryMethods::addDirectionsMarkers(glm::dvec3 viewDirecionPos, glm::dvec3 northDirectionPos, float starRadius) {

        const std::string markerView = "{"
            "Identifier = 'markerView',"
            "Parent = 'SolarSystemBarycenter',"
            "Enabled = true,"
            "Renderable = {"
                "Type = 'RenderableGlobe',"
                "Radii = " + std::to_string(starRadius) + "* 0.5,"
                "SegmentsPerPatch = 64,"
                "PerformShading = false,"
                "Layers = {"
                    "ColorLayers = {"
                        "{"
                            "Identifier = 'dir',"
                            "Type = 'SolidColor',"
                            "BlendMode = 'Normal',"
                            "Color = {1.0,0.0,0.0},"
                            "Enabled = true"
                        "},"
                    "}"
                "}"
            "},"
            "Transform = {"
                "Translation = {"
                    "Type = 'StaticTranslation',"
                    "Position = " + std::to_string(viewDirecionPos) + ","
                "},"
            "},"
        "}";
        std::string script = "openspace.addSceneGraphNode(" + markerView + ");";
        OsEng.scriptEngine().queueScript(
            script,
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
        const std::string markerNorth = "{"
            "Identifier = 'markerNorth',"
            "Parent = 'SolarSystemBarycenter',"
            "Enabled = true,"
            "Renderable = {"
                "Type = 'RenderableGlobe',"
                "Radii = " + std::to_string(starRadius) + "* 0.5,"
                "SegmentsPerPatch = 64,"
                "PerformShading = false,"
                "Layers = {"
                    "ColorLayers = {"
                        "{"
                            "Identifier = 'dir',"
                            "Type = 'SolidColor',"
                            "BlendMode = 'Normal',"
                            "Color = {0.0,0.0,1.0},"
                            "Enabled = true"
                        "},"
                    "}"
                "}"
            "},"
            "Transform = {"
                "Translation = {"
                    "Type = 'StaticTranslation',"
                    "Position = " + std::to_string(northDirectionPos) + ","
                "},"
            "},"
        "}";
        script = "";
        script = "openspace.addSceneGraphNode(" + markerNorth + ");";
        OsEng.scriptEngine().queueScript(
            script,
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
    void DiscoveryMethods::removeDirectionsMarkers() {
        std::string script = "openspace.removeSceneGraphNode('markerView'); openspace.removeSceneGraphNode('markerNorth');";
        OsEng.scriptEngine().queueScript(
            script,
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    float DiscoveryMethods::getTransitScaleFactor() {
        return _transitScaleFactor;
    }

    void addDopplerGraphs() {
        std::string script = "openspace.addScreenSpaceRenderable("
                "{"
                "Identifier = 'DopplerShift2',"
                "Type = 'ScreenSpaceImageLocal',"
                "TexturePath = openspace.absPath('${BASE}/modules/exoplanets/stripes2.png'),"
                "EuclideanPosition = {0.0, -0.7}"
                "}"
            ");"
            "openspace.addScreenSpaceRenderable("
                "{"
                "Identifier = 'DopplerShift1',"
                "Type = 'ScreenSpaceImageLocal',"
                "TexturePath = openspace.absPath('${BASE}/modules/exoplanets/spectrum.jpg'),"
                "EuclideanPosition = {0.0, -0.7}"
                "}"
            ");";

        OsEng.scriptEngine().queueScript(
            script,
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    

    void DiscoveryMethods::scaleNode(std::string nodeName, float scalefactor) {
        std::string script = "openspace.setPropertyValueSingle( 'Scene."+ nodeName +".Scale.Scale', " + std::to_string(scalefactor) + ", 1);"; //get name of current star from em
        OsEng.scriptEngine().queueScript(
            script,
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    void DiscoveryMethods::moveStar(std::string starName, float semiMajorAxis) {
        std::string script = "openspace.setPropertyValueSingle( 'Scene."+starName+"Globe.Translation.SemiMajorAxis', " + std::to_string(semiMajorAxis) + ", 1); ";
        OsEng.scriptEngine().queueScript(
            script,
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    void DiscoveryMethods::toggleVisabilityOuterPlanets(std::vector<std::string> planetNames, std::string visability) {
        std::vector<Exoplanet> planets = OsEng.moduleEngine().module<ExoplanetsModule>()->getPlsy();
        //std::vector<std::string> planetNames = OsEng.moduleEngine().module<ExoplanetsModule>()->getPlna();

        if (planetNames.size()>1)
        {
            //keeping first planet in the list, wich dosn't neccesarily mean the closest one...
            for (size_t i = 1; i < planetNames.size(); i++) {
                
                std::string script = "";
                //remove planetglobe
                if (!isnan(planets[i].R)) {
                    script += "openspace.setPropertyValueSingle( 'Scene." + planetNames[i] + ".renderable.Enabled', " + visability + "); ";
                }
                //remove trail
                script += "openspace.setPropertyValueSingle( 'Scene." + planetNames[i] + "Trail.renderable.Enabled', " + visability + "); ";
                //remove disc
                if (!isnan(planets[i].AUPPER) && !isnan(planets[i].ALOWER)) {
                    script += "openspace.setPropertyValueSingle( 'Scene." + planetNames[i] + "Disc.renderable.Enabled', " + visability + "); ";
                }

                OsEng.scriptEngine().queueScript(
                    script,
                    openspace::scripting::ScriptEngine::RemoteScripting::Yes
                );
            }   
        }
    }

    void DiscoveryMethods::toggleVisabilityPlanet(std::string nodeName, std::string visability) {
        std::string script = "openspace.setPropertyValueSingle( 'Scene." +nodeName  + ".RenderableGlobe.Enabled', " + visability + "); ";
        OsEng.scriptEngine().queueScript(
            script,
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
    
    void DiscoveryMethods::moveCameraDopplerView(glm::dvec3 pos) {

        Camera* cam = OsEng.navigationHandler().camera();
        cam->setPositionVec3(pos);
        OsEng.navigationHandler().resetCameraDirection();

    }

    void DiscoveryMethods::moveCameraTransitView(glm::dvec3 pos) {

        Camera* cam = OsEng.navigationHandler().camera();
        cam->setPositionVec3(pos);
        OsEng.navigationHandler().resetCameraDirection();

    }

    bool DiscoveryMethods::isDoppler() {
        return _showDoppler;
    }
    bool DiscoveryMethods::isTransit() {
        return _showTransit;
    }
    void DiscoveryMethods::setDopplerImagePos(float value) {
        std::string script = "openspace.setPropertyValueSingle( 'ScreenSpace.DopplerShift2.EuclideanPosition', {"+std::to_string(value)+", -0.7}); "; //get name of current star from em
        OsEng.scriptEngine().queueScript(
            script,
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    void DiscoveryMethods::addDopplerMethodVisualization() {
        SceneGraphNode* focusNode = OsEng.navigationHandler().focusNode();
        std::string starName = OsEng.moduleEngine().module<ExoplanetsModule>()->getStarName(); // getStarName 
        glm::dvec3 starPosition = focusNode->worldPosition(); // can get from Exoplanet.POSITIONX/.POSITIONY/.POSITIONZ (in parsecs)
        glm::dvec3 starToSunVec = normalize(glm::dvec3(0.0, 0.0, 0.0) - starPosition);
        std::vector<Exoplanet> planets = OsEng.moduleEngine().module<ExoplanetsModule>()->getPlsy();
        std::vector<std::string> planetNames = OsEng.moduleEngine().module<ExoplanetsModule>()->getPlna();

        float semiMajorAxis = planets[0].A; // in AU
        float starSemiMajorAxis = 0.1 * semiMajorAxis; // 10% of exoplanets semiMajorAxis
        float eccentricity = planets[0].ECC;
        float starRadius = planets[0].RSTAR; // in Solar Radii

        glm::dvec3 north = glm::dvec3(0.0, 0.0, 1.0);
        //glm::dvec3 northProjected = glm::normalize(glm::length(north)*glm::sin(glm::dot(north, starToSunVec)) * glm::cross(starToSunVec, glm::cross(north, starToSunVec)));
        glm::dvec3 northProjected = normalize(north - (((dot(north, starToSunVec)) / (glm::length(starToSunVec)))*starToSunVec));

        // MOVE CAMERA
        glm::dvec3 faceOnVector = glm::normalize(glm::cross(starToSunVec, northProjected));
        glm::dvec3 cameraPosition = starPosition + ((4.0 * semiMajorAxis * 149597870700.0) * faceOnVector);
        moveCameraDopplerView(cameraPosition);
        // END CAMERA

        // SCALE STAR AND PLANET
        float periapsisDistance = semiMajorAxis * (1.0 - eccentricity); // in AU
        periapsisDistance *= 149597870700.0; // in m
        starRadius *= 6.957E8; // in m
        float scale = (0.1 * periapsisDistance) / starRadius;
        scaleNode(starName + "Globe", scale);
        scaleNode(planetNames[0], scale); // using planetNames[0] because i know that will be the one visible after removing outer planets
        // END SCALE

        // MOVE STAR
        starSemiMajorAxis *= 149597871.0; // in km
        moveStar(starName, starSemiMajorAxis);

        // SHOW ONE PLANET
        // for planets found with doppler method, the radius is not always known.
        //so this "fake" planet is shown for the sake of the vizualisation
        toggleVisabilityPlanet(planetNames[0], "true");

        // HIDE THE REST OF THE PLANETS
        // in some cases there are multiple planets in the system, but for the viz only one can be shown
        toggleVisabilityOuterPlanets(planetNames, "false");

        // SHOW GRAPHS
        addDopplerGraphs();


        // HELPER MARKERS
        glm::dvec3 northDirectionPos = starPosition + (double(starRadius * scale) * northProjected);
        glm::dvec3 viewDirectionPos = starPosition + (double(starRadius * scale) * starToSunVec);
        addDirectionsMarkers(viewDirectionPos, northDirectionPos, starRadius);
        // END MARKERS

    }

    void DiscoveryMethods::removeDopplerMethodVisualization() {
        std::string starName = OsEng.moduleEngine().module<ExoplanetsModule>()->getStarName();
        std::vector<std::string> planetNames = OsEng.moduleEngine().module<ExoplanetsModule>()->getPlna();
        std::vector<Exoplanet> planets = OsEng.moduleEngine().module<ExoplanetsModule>()->getPlsy();

        //SCALE STAR AND PLANET
        scaleNode(starName + "Globe", 1.0);
        scaleNode(planetNames[0], 1.0);

        // MOVE STAR
        moveStar(starName, 0.0);

        //HIDE THE PLANET (if it was hidden from the start)
        if (isnan(planets[0].R)) {
            toggleVisabilityPlanet(planetNames[0], "false");
        }
        
        // SHOW THE REST OF THE PLANETS
        toggleVisabilityOuterPlanets(planetNames, "true");

        // HIDE GRAPHS
        std::string script = "openspace.removeScreenSpaceRenderable('DopplerShift1'); openspace.removeScreenSpaceRenderable('DopplerShift2');";
        OsEng.scriptEngine().queueScript(
            script,
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );

        //REMOVE HELP MARKERS
        removeDirectionsMarkers();
    }

    void DiscoveryMethods::addTransitMethodVisualization() {

        SceneGraphNode* focusNode = OsEng.navigationHandler().focusNode();
        std::string starName = OsEng.moduleEngine().module<ExoplanetsModule>()->getStarName(); // getStarName 
        glm::dvec3 starPosition = focusNode->worldPosition(); // can get from Exoplanet.POSITIONX/.POSITIONY/.POSITIONZ (in parsecs)
        glm::dvec3 starToSunVec = normalize(glm::dvec3(0.0, 0.0, 0.0) - starPosition);
        std::vector<Exoplanet> planets = OsEng.moduleEngine().module<ExoplanetsModule>()->getPlsy();
        std::vector<std::string> planetNames = OsEng.moduleEngine().module<ExoplanetsModule>()->getPlna();
        
        float semiMajorAxis = planets[0].A; // in AU (1AU = 149 597 870 700m)
        float eccentricity = planets[0].ECC;
        float starRadius = planets[0].RSTAR; // in Solar Radii

        // MOVE CAMERA
        //borde kanske va periapsis distance, men det går bra ändå
        glm::dvec3 cameraPosition = starPosition + ((3.0 * semiMajorAxis * 149597870700.0) * starToSunVec);;
        moveCameraTransitView(cameraPosition);
        // END CAMERA
        toggleVisabilityPlanet(planetNames[0], "true");

        // SCALE BOTH STAR AND PLANET
        // want star to take up 2/3 of the radius, the radius is as smallest at the periapsis
        float periapsisDistance = semiMajorAxis * (1.0 - eccentricity); // in AU
        periapsisDistance *= 149597870700.0; // in m
        starRadius *= 6.957E8; // in m

        float scale = (0.666 * periapsisDistance) / starRadius; // actual radius * scale = wanted radius
        scaleNode(starName + "Globe", scale);
        scaleNode(planetNames[0], scale); //eller använda getPlna()?
        _transitScaleFactor = scale;
        // END SCALE

        // HELPER MARKERS
        glm::dvec3 north = glm::dvec3(0.0, 0.0, 1.0);
        //glm::dvec3 northProjected = glm::normalize(glm::length(north)*glm::sin(glm::dot(north, starToSunVec)) * glm::cross(starToSunVec, glm::cross(north, starToSunVec)));
        glm::dvec3 northProjected = normalize(north - (((dot(north, starToSunVec)) / (glm::length(starToSunVec)))*starToSunVec));
        glm::dvec3 northDirectionPos = starPosition + (double(starRadius * scale) * northProjected);
        glm::dvec3 viewDirectionPos = starPosition + (double(starRadius * scale) * starToSunVec);
        addDirectionsMarkers(viewDirectionPos, northDirectionPos, starRadius);
        // END MARKERS
    }
    void DiscoveryMethods::removeTransitMethodVisualization() {

        //SCALE STAR AND PLANET
        std::string starName = OsEng.moduleEngine().module<ExoplanetsModule>()->getStarName();
        scaleNode(starName + "Globe", 1);
        scaleNode(starName + " b", 1);



        //REMOVE HELP MARKERS
        removeDirectionsMarkers();
    }


	DiscoveryMethods::DiscoveryMethods()
		: PropertyOwner({ "DiscoveryMethods" })
		, _showTransit(TransitMethodInfo, false)
		, _showDoppler(DopplerMethodInfo, false)
	{
        _showTransit.onChange([&]() {
            if (_showTransit) //just changed to true
            {
                if (_showDoppler) //only one viz at the time
                {
                    _showDoppler = false;
                    removeDopplerMethodVisualization();
                }

                addTransitMethodVisualization();
            }
            else //just changed to false
            {
                removeTransitMethodVisualization();
            }
             
        });
        addProperty(_showTransit);
        _showDoppler.onChange([&]() { 
            if (_showDoppler) //just changed to true
            {
                if (_showTransit)
                {
                    
                    _showTransit = false;
                    removeTransitMethodVisualization();
                }
                addDopplerMethodVisualization();
            }
            else //just changed to false
            {
                removeDopplerMethodVisualization();
            }
             
        });
        addProperty(_showDoppler);
        printf("slut pa constructor");
	}

} // namespce

