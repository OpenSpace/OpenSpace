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

    void DiscoveryMethods::addTransitMethodVisualization() {
       
        std::string starName = OsEng.moduleEngine().module<ExoplanetsModule>()->getStarName();
        float planetSemiMajorAxis = (OsEng.moduleEngine().module<ExoplanetsModule>()->getClosestExoplanet()).A; // AU
        float eccentricity = (OsEng.moduleEngine().module<ExoplanetsModule>()->getClosestExoplanet()).ECC; 
        float starRadius = (OsEng.moduleEngine().module<ExoplanetsModule>()->getClosestExoplanet()).RSTAR; // Solar Radii

        // periapsis radius
        float periapsisRadius = planetSemiMajorAxis * (1.0 - eccentricity);
        // to km
        periapsisRadius *= 149597871;
        starRadius *= 695700;

        // increase star radius to two thirds of periapsis radius
        float scaleFactor = (0.666 * periapsisRadius) / starRadius;
        std::string planetName = starName + " b";
        scaleStar(starName +"Globe", scaleFactor);
        scaleStar(planetName, scaleFactor);
        moveCameraTransitView();
    }
    void DiscoveryMethods::removeTransitMethodVisualization() {

        std::string starName = OsEng.moduleEngine().module<ExoplanetsModule>()->getStarName();
        std::string planetName = starName + " b";
        scaleStar(starName + "Globe", 1);
        scaleStar(planetName, 1);
    }

    void DiscoveryMethods::addDopplerMethodVisualization() {

        std::string starName = OsEng.moduleEngine().module<ExoplanetsModule>()->getStarName();
        float planetSemiMajorAxis = (OsEng.moduleEngine().module<ExoplanetsModule>()->getClosestExoplanet()).A;
        float starSemiMajorAxis = 0.1 * planetSemiMajorAxis * 149597871; // 10% of exoplanets semiMajorAxis (get value from em)
        float eccentricity = (OsEng.moduleEngine().module<ExoplanetsModule>()->getClosestExoplanet()).ECC;
        float starRadius = (OsEng.moduleEngine().module<ExoplanetsModule>()->getClosestExoplanet()).RSTAR; // Solar Radii

        float periapsisRadius = planetSemiMajorAxis * (1.0 - eccentricity);
        // to km
        periapsisRadius *= 149597871;
        starRadius *= 695700;
        float scaleFactor = (0.1 * periapsisRadius) / starRadius;

        moveCameraDopplerView();
        scaleStar(starName + "Globe", scaleFactor);
        moveStar(starName, starSemiMajorAxis);
    }

    void DiscoveryMethods::removeDopplerMethodVisualization() {
        std::string starName = OsEng.moduleEngine().module<ExoplanetsModule>()->getStarName();
        scaleStar(starName + "Globe", 1.0);
        moveStar(starName, 0.0);
    }

    void DiscoveryMethods::scaleStar(std::string nodeName, float scalefactor) {
        std::string script = "openspace.setPropertyValueSingle( 'Scene."+ nodeName +".Scale.Scale', " + std::to_string(scalefactor) + ", 1);"; //get name of current star from em
        OsEng.scriptEngine().queueScript(
            script,
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    void DiscoveryMethods::moveStar(std::string starName, float semiMajorAxis) {
        std::string script = "openspace.setPropertyValueSingle( 'Scene."+starName+"Globe.Translation.SemiMajorAxis', " + std::to_string(semiMajorAxis) + ", 1); "; //get name of current star from em
        OsEng.scriptEngine().queueScript(
            script,
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    void DiscoveryMethods::moveCameraTransitView() {

        Camera* cam = OsEng.navigationHandler().camera();

        SceneGraphNode* focusNode = OsEng.navigationHandler().focusNode();
        glm::dvec3 starPosition = focusNode->worldPosition();
        // get the vector between star and the sun
        glm::dvec3 starToSunVec = normalize(glm::dvec3(0.0, 0.0, 0.0) - starPosition);

        // a position along that vector (twice the semimajor axis away from the sun)
        float semiMajorAxis = (OsEng.moduleEngine().module<ExoplanetsModule>()->getClosestExoplanet()).A;
        glm::dvec3 newCameraPosition = starPosition + ((3.0 * semiMajorAxis * 149597870700) * starToSunVec);

        //move camera to that pos
        cam->setPositionVec3(newCameraPosition);
        OsEng.navigationHandler().resetCameraDirection();

    }
    
    void DiscoveryMethods::moveCameraDopplerView() {

        Camera* cam = OsEng.navigationHandler().camera();

        SceneGraphNode* focusNode = OsEng.navigationHandler().focusNode();
        glm::dvec3 starPosition = focusNode->worldPosition();
        glm::dvec3 starToSunVec = normalize(glm::dvec3(0.0, 0.0, 0.0) - starPosition);
        glm::dvec3 north = glm::dvec3(0.0, 0.0, 1.0);
        glm::dvec3 northProjected = glm::normalize(glm::length(north)*glm::sin(glm::dot(north, starToSunVec)) * glm::cross(starToSunVec, glm::cross(north, starToSunVec)));
        glm::dvec3 faceOnVector = glm::normalize(glm::cross(starToSunVec, northProjected));
        // a position along that vector (twice the semimajor axis away from the sun)
        float semiMajorAxis = (OsEng.moduleEngine().module<ExoplanetsModule>()->getClosestExoplanet()).A;
        glm::dvec3 newCameraPosition = starPosition + ((3.0 * semiMajorAxis * 149597870700) * faceOnVector);

        //move camera to that pos
        cam->setPositionVec3(newCameraPosition);
        OsEng.navigationHandler().resetCameraDirection();

    }

	DiscoveryMethods::DiscoveryMethods()
		: PropertyOwner({ "DiscoveryMethods" })
		, _showTransit(TransitMethodInfo, false)
		, _showDoppler(DopplerMethodInfo, false)
	{
		addProperty(_showTransit);
		addProperty(_showDoppler);
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
	}

} // namespce

