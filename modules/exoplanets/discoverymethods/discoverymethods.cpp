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

#include <openspace/engine/globals.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/camera.h>
#include <openspace/scene/scene.h>

#include <ghoul/glm.h>


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
    
    static const openspace::properties::Property::PropertyInfo SolarSystemReferenceInfo = {
        "SolarSystemReference",
        "Show solar system reference",
        "Show the size of the solar system as a reference for size."
    };
} // namespace

namespace openspace::exoplanets {

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
                "Position = " + ghoul::to_string(viewDirecionPos) + ","
            "},"
        "},"
    "}";
    std::string script = "openspace.addSceneGraphNode(" + markerView + ");";
    openspace::global::scriptEngine.queueScript(
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
                "Position = " + ghoul::to_string(northDirectionPos) + ","
            "},"
        "},"
    "}";
    script = "";
    script = "openspace.addSceneGraphNode(" + markerNorth + ");";
    openspace::global::scriptEngine.queueScript(
        script,
        openspace::scripting::ScriptEngine::RemoteScripting::Yes
    );
}
void DiscoveryMethods::removeDirectionsMarkers() {
    std::string script = "openspace.removeSceneGraphNode('markerView'); openspace.removeSceneGraphNode('markerNorth');";
    openspace::global::scriptEngine.queueScript(
        script,
        openspace::scripting::ScriptEngine::RemoteScripting::Yes
    );
}

float DiscoveryMethods::getTransitScaleFactor() {
    return _transitScaleFactor;
}

void addDopplerGraphs() {
    std::string script = 
        "openspace.addScreenSpaceRenderable("
        "{"
            "Identifier = 'DopplerShift2',"
            "Type = 'ScreenSpaceImageLocal',"
            "TexturePath = openspace.absPath('${BASE}/modules/exoplanets/stripes2.png'),"
            "EuclideanPosition = {0.0, -0.7}"
        "});"
        "openspace.addScreenSpaceRenderable("
        "{"
            "Identifier = 'DopplerShift1',"
            "Type = 'ScreenSpaceImageLocal',"
            "TexturePath = openspace.absPath('${BASE}/modules/exoplanets/spectrum.jpg'),"
            "EuclideanPosition = {0.0, -0.7}"
        "});";

    openspace::global::scriptEngine.queueScript(
        script,
        openspace::scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void addTransitGraphs() {
    std::string script = 
        "openspace.addScreenSpaceRenderable("
        "{"
            "Identifier = 'Transit2',"
            "Type = 'ScreenSpaceImageLocal',"
            "TexturePath = openspace.absPath('${BASE}/modules/exoplanets/prick.png'),"
            "EuclideanPosition = {0.0, 0.0},"
        "});"
        "openspace.addScreenSpaceRenderable("
        "{"
            "Identifier = 'Transit1',"
            "Type = 'ScreenSpaceImageLocal',"
            "TexturePath = openspace.absPath('${BASE}/modules/exoplanets/graph.png'),"
            "Scale = 0.485,"
            "EuclideanPosition = {0.0, -0.65}"
        "});"
        "openspace.addScreenSpaceRenderable("
        "{"
            "Identifier = 'Transit3',"
            "Type = 'ScreenSpaceImageLocal',"
            "TexturePath = openspace.absPath('${BASE}/modules/exoplanets/axes.png'),"
            "Scale = 0.7,"
            "EuclideanPosition = {-0.05, -0.65}"
        "});";

    openspace::global::scriptEngine.queueScript(
        script,
        openspace::scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void DiscoveryMethods::scaleNode(std::string nodeName, float scalefactor) {
    std::string script = "openspace.setPropertyValueSingle( 'Scene."+ nodeName +".Scale.Scale', " + std::to_string(scalefactor) + ", 1);"; //get name of current star from em
    openspace::global::scriptEngine.queueScript(
        script,
        openspace::scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void DiscoveryMethods::moveStar(std::string starName, float semiMajorAxis) {
    std::string script = "openspace.setPropertyValueSingle( 'Scene."+starName+"Globe.Translation.SemiMajorAxis', " + std::to_string(semiMajorAxis) + ", 1); ";
    openspace::global::scriptEngine.queueScript(
        script,
        openspace::scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void DiscoveryMethods::toggleVisabilityOuterPlanets(std::vector<std::string> planetNames, std::string visability) {
    std::vector<Exoplanet> planets = global::moduleEngine.module<ExoplanetsModule>()->planetSystem();

    if (planetNames.size()>1)
    {
        //keeping first planet in the list, wich dosn't neccesarily mean the closest one...
        for (size_t i = 1; i < planetNames.size(); i++) {
            std::string script = "";
            //remove planetglobe
            if (!isnan(planets[i].R)) {
                script += "openspace.setPropertyValueSingle( 'Scene." + planetNames[i] + ".RenderableGlobe.Enabled', " + visability + "); ";
            }
            //remove trail
            script += "openspace.setPropertyValueSingle( 'Scene." + planetNames[i] + "Trail.renderable.Enabled', " + visability + "); ";
            //remove disc
            if (!isnan(planets[i].AUPPER) && !isnan(planets[i].ALOWER)) {
                script += "openspace.setPropertyValueSingle( 'Scene." + planetNames[i] + "Disc.renderable.Enabled', " + visability + "); ";
            }

            openspace::global::scriptEngine.queueScript(
                script,
                openspace::scripting::ScriptEngine::RemoteScripting::Yes
            );
        }   
    }
}

void DiscoveryMethods::toggleVisabilityPlanet(std::string nodeName, std::string visability) {
    std::string script = "openspace.setPropertyValueSingle( 'Scene." +nodeName  + ".RenderableGlobe.Enabled', " + visability + "); ";
    openspace::global::scriptEngine.queueScript(
        script,
        openspace::scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void DiscoveryMethods::moveCamera(glm::dvec3 pos) {
    Camera* cam = global::navigationHandler.camera();
    cam->setPositionVec3(pos);
    global::navigationHandler.resetCameraDirection();
}

bool DiscoveryMethods::isDoppler() {
    return _showDoppler;
}
bool DiscoveryMethods::isTransit() {
    return _showTransit;
}
bool DiscoveryMethods::isReference() {
    return _showSolarSystemReference;
}

void DiscoveryMethods::setDopplerImagePos(float value) {
    std::string script = "openspace.setPropertyValueSingle( 'ScreenSpace.DopplerShift2.EuclideanPosition', {"+std::to_string(value)+", -0.7}); ";
    openspace::global::scriptEngine.queueScript(
        script,
        openspace::scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void DiscoveryMethods::setTransitImagePos(float valueX,float valueY) {
    std::string script = "openspace.setPropertyValueSingle( 'ScreenSpace.Transit2.EuclideanPosition', {" + std::to_string(valueX) + "," + std::to_string(valueY) + "}); ";
    openspace::global::scriptEngine.queueScript(
        script,
        openspace::scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void DiscoveryMethods::addDopplerMethodVisualization() {
    const SceneGraphNode* focusNode = global::navigationHandler.anchorNode();
    std::string starName = global::moduleEngine.module<ExoplanetsModule>()->getStarName(); // getStarName 
    glm::dvec3 starPosition = focusNode->worldPosition(); // can get from Exoplanet.POSITIONX/.POSITIONY/.POSITIONZ (in parsecs)
    glm::dvec3 starToSunVec = normalize(glm::dvec3(0.0, 0.0, 0.0) - starPosition);
    std::vector<Exoplanet> planets = global::moduleEngine.module<ExoplanetsModule>()->planetSystem();
    std::vector<std::string> planetNames = global::moduleEngine.module<ExoplanetsModule>()->planetNames();

    float semiMajorAxis = planets[0].A; // in AU
    float starSemiMajorAxis = 0.1 * semiMajorAxis; // 10% of exoplanets semiMajorAxis
    float eccentricity = planets[0].ECC;
    if (isnan(planets[0].ECC)) {
        eccentricity = 0.0;
    }
    float starRadius = planets[0].RSTAR; // in Solar Radii

    glm::dvec3 north = global::moduleEngine.module<ExoplanetsModule>()->getNorthVector();

    // MOVE CAMERA
    glm::dvec3 faceOnVector = glm::normalize(glm::cross(starToSunVec, north));
    glm::dvec3 cameraPosition = starPosition + ((4.0 * semiMajorAxis * 149597870700.0) * faceOnVector);
    moveCamera(cameraPosition);
    // END CAMERA

    // SCALE STAR AND PLANET
    float periapsisDistance = semiMajorAxis * (1.0 - eccentricity); // in AU
    periapsisDistance *= 149597870700.0; // in m
    starRadius *= 6.957E8; // in m
    float scale = (0.2 * periapsisDistance) / starRadius;
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
    glm::dvec3 northDirectionPos = starPosition + (double(starRadius * scale) * north);
    glm::dvec3 viewDirectionPos = starPosition + (double(starRadius * scale) * starToSunVec);
    //addDirectionsMarkers(viewDirectionPos, northDirectionPos, starRadius);
    // END MARKERS
}

void DiscoveryMethods::removeDopplerMethodVisualization() {
    std::string starName = global::moduleEngine.module<ExoplanetsModule>()->getStarName();
    std::vector<std::string> planetNames = global::moduleEngine.module<ExoplanetsModule>()->planetNames();
    std::vector<Exoplanet> planets = global::moduleEngine.module<ExoplanetsModule>()->planetSystem();

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
    global::scriptEngine.queueScript(
        script,
        openspace::scripting::ScriptEngine::RemoteScripting::Yes
    );

    //REMOVE HELP MARKERS
    //removeDirectionsMarkers();
}

void DiscoveryMethods::addTransitMethodVisualization() {
    const SceneGraphNode* focusNode = global::navigationHandler.anchorNode();
    std::string starName = global::moduleEngine.module<ExoplanetsModule>()->getStarName(); // getStarName 
    glm::dvec3 starPosition = focusNode->worldPosition(); // can get from Exoplanet.POSITIONX/.POSITIONY/.POSITIONZ (in parsecs)
    glm::dvec3 starToSunVec = normalize(glm::dvec3(0.0, 0.0, 0.0) - starPosition);
    std::vector<Exoplanet> planets = global::moduleEngine.module<ExoplanetsModule>()->planetSystem();
    std::vector<std::string> planetNames = global::moduleEngine.module<ExoplanetsModule>()->planetNames();
        
    float semiMajorAxis = planets[0].A; // in AU (1AU = 149 597 870 700m)
    float eccentricity = planets[0].ECC;
    if (isnan(planets[0].ECC)) {
        eccentricity = 0.0;
    }
    float starRadius = planets[0].RSTAR; // in Solar Radii

    // MOVE CAMERA
    //borde kanske va periapsis distance, men det går bra ändå
    glm::dvec3 north = global::moduleEngine.module<ExoplanetsModule>()->getNorthVector();
    //glm::dvec3 faceOnVector = glm::normalize(glm::cross(starToSunVec, north));
    glm::dvec3 cameraPosition = starPosition + ((4.0 * semiMajorAxis * 149597870700.0) * starToSunVec);
    //glm::dvec3 cameraPosition = starPosition + ((3.0 * semiMajorAxis * 149597870700.0) * faceOnVector);
        
    moveCamera(cameraPosition);
    // END CAMERA
    toggleVisabilityPlanet(planetNames[0], "true");

    // SCALE BOTH STAR AND PLANET
    // want star to take up 2/3 of the radius, the radius is as smallest at the periapsis
    float periapsisDistance = semiMajorAxis * (1.0 - eccentricity); // in AU
    periapsisDistance *= 149597870700.0; // in m
    starRadius *= 6.957E8; // in m

    float scale = (0.5 * periapsisDistance) / starRadius; // actual radius * scale = wanted radius
    scaleNode(starName + "Globe", scale);
    scaleNode(planetNames[0], scale); //eller använda getPlna()?
    _transitScaleFactor = scale;
    // END SCALE

    // ADD THE GRAPH
    addTransitGraphs();
    // END GRAPH

    // HELPER MARKERS
        
    glm::dvec3 northDirectionPos = starPosition + (double(starRadius * scale) * north);
    glm::dvec3 viewDirectionPos = starPosition + (double(starRadius * scale) * starToSunVec);
    //addDirectionsMarkers(viewDirectionPos, northDirectionPos, starRadius);
    // END MARKERS
}

void DiscoveryMethods::removeTransitMethodVisualization() {
    std::vector<std::string> planetNames = global::moduleEngine.module<ExoplanetsModule>()->planetNames();
    //SCALE STAR AND PLANET
    std::string starName = global::moduleEngine.module<ExoplanetsModule>()->getStarName();
    scaleNode(starName + "Globe", 1);
    scaleNode(planetNames[0], 1);

    // REMOVE GRAPH
    std::string script = "openspace.removeScreenSpaceRenderable('Transit3');"
        "openspace.removeScreenSpaceRenderable('Transit2');"
        "openspace.removeScreenSpaceRenderable('Transit1');";

    openspace::global::scriptEngine.queueScript(
        script,
        openspace::scripting::ScriptEngine::RemoteScripting::Yes
    );

    //REMOVE HELP MARKERS
    //removeDirectionsMarkers();
}

void DiscoveryMethods::addSolarSystemReferenceVisualization() {
    std::string starName = global::moduleEngine.module<ExoplanetsModule>()->getStarName();
    std::vector<Exoplanet> planets = global::moduleEngine.module<ExoplanetsModule>()->planetSystem();
    std::vector<std::string> planetNames = global::moduleEngine.module<ExoplanetsModule>()->planetNames();
        
    // SUN
    const std::string sunRef = "{"
        "Identifier = 'SunReference',"
        "Parent = '" + starName + "',"
        "Renderable = {"
            "Type = 'RenderablePlaneImageLocal',"
            "Size = 6.957E8," //RSTAR. in meters. 1 solar radii = 6.95700×10e8 m
            "Billboard = true,"
            "Texture = openspace.absPath('${MODULE_EXOPLANETS}/target-blue-ring.png'),"
            "BlendMode = 'Additive'"
        "},"
        "Transform = {"
            "Translation = {"
                "Type = 'StaticTranslation',"
                "Position = {0, 0, 0}," //"+ std::to_string(planets[0].RSTAR) + "* 6.957E8
            "},"
        "},"
    "}";
        
    std::string script = "openspace.addSceneGraphNode(" + sunRef + ");";
    openspace::global::scriptEngine.queueScript(
        script,
        openspace::scripting::ScriptEngine::RemoteScripting::Yes
    );

    // EARTH
    for (int i = 0; i < planetNames.size(); i++)
    {
        const std::string earthRef = "{"
            "Identifier = 'EarthReference" + std::to_string(i) + "',"
            "Parent = '" + planetNames[i] + "',"
            "Renderable = {"
                "Type = 'RenderablePlaneImageLocal',"
                "Size = 6378137," // in meters
                "Billboard = true,"
                "Texture = openspace.absPath('${MODULE_EXOPLANETS}/target-blue-ring.png'),"
                "BlendMode = 'Additive'"
            "},"
            "Transform = {"
                "Translation = {"
                    "Type = 'StaticTranslation',"
                    "Position = {0, 0, 0}," //Jupiter radii to m " + std::to_string(planets[0].R) + "* 7.1492E7
                "},"
            "},"
        "}";
        script = "";
        script = "openspace.addSceneGraphNode(" + earthRef + ");";
        openspace::global::scriptEngine.queueScript(
            script,
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    glm::dmat3 rotation = global::moduleEngine.module<ExoplanetsModule>()->getRotation();

    // ORBIT
    const std::string orbitRef = "{"
        "Identifier = 'OrbitReference',"
        "Parent = '" + starName + "',"
        "Renderable = {"
            "Type = 'RenderablePlaneImageLocal',"
            "Size = 1.496E11," // earths semi-major axis in m
            "Billboard = false,"
            "Texture = openspace.absPath('${MODULE_EXOPLANETS}/target-blue-ring.png'),"
            "BlendMode = 'Additive'"
        "},"
        "Transform = {"
            "Rotation = {"
                "Type = 'StaticRotation',"
                "Rotation =  " + ghoul::to_string(rotation) + ","
            "}"
        "},"
    "}";
    script = "";
    script = "openspace.addSceneGraphNode(" + orbitRef + ");";
    openspace::global::scriptEngine.queueScript(
        script,
        openspace::scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void DiscoveryMethods::removeSolarSystemReferenceVisualization() {
    std::string script = "openspace.removeSceneGraphNode('SunReference');"
        "openspace.removeSceneGraphNode('EarthReference');"
        "openspace.removeSceneGraphNode('OrbitReference');";

    openspace::global::scriptEngine.queueScript(
        script,
        openspace::scripting::ScriptEngine::RemoteScripting::Yes
    );
}

DiscoveryMethods::DiscoveryMethods()
    : PropertyOwner({ "DiscoveryMethods" })
    , _showTransit(TransitMethodInfo, false)
    , _showDoppler(DopplerMethodInfo, false)
    , _showSolarSystemReference(SolarSystemReferenceInfo, false)
{
    _showTransit.onChange([&]() {
        if (_showTransit) { //just changed to true
            if (_showDoppler) { //only one viz at the time
                _showDoppler = false;
                removeDopplerMethodVisualization();
            }
            addTransitMethodVisualization();
        }
        else { //just changed to false
            removeTransitMethodVisualization();
        }
    });
    addProperty(_showTransit);

    _showDoppler.onChange([&]() { 
        if (_showDoppler) { //just changed to true
            if (_showTransit) {
                _showTransit = false;
                removeTransitMethodVisualization();
            }
            addDopplerMethodVisualization();
        }
        else { //just changed to false
            removeDopplerMethodVisualization();
        }
    });
    addProperty(_showDoppler);

    _showSolarSystemReference.onChange([&]() {
        if (_showSolarSystemReference) {
            addSolarSystemReferenceVisualization();
        }
        else {
            removeSolarSystemReferenceVisualization();
        }
    });
    addProperty(_showSolarSystemReference);
}

} // namespce
