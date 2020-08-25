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
#include <modules/exoplanets/tasks/exoplanetscsvtobintask.h>
#include <modules/exoplanets/rendering/renderableorbitdisc.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>

#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/factorymanager.h>
#include <openspace/interaction/navigationhandler.h>

#include <thread> 
#include <chrono>  

#include "exoplanetsmodule_lua.inl"

namespace openspace {

const char* _loggerCat = "exoplanets";

using namespace exoplanets;

ExoplanetsModule::ExoplanetsModule() : OpenSpaceModule(Name) {}

void ExoplanetsModule::setClosestExoplanet(Exoplanet closestExo) {
	_exo = closestExo;
}

Exoplanet ExoplanetsModule::getClosestExoplanet() {
	return _exo;
}

void ExoplanetsModule::setStarName(std::string starName) {
	_starName = starName;
}

std::string ExoplanetsModule::getStarName() {
	return _starName;
}
void ExoplanetsModule::setPlsy(std::vector<Exoplanet> plsy) {
    _plsy = plsy;
}
std::vector<Exoplanet> ExoplanetsModule::getPlsy() {
    return _plsy;
}
void ExoplanetsModule::setPlna(std::vector<std::string> plna) {
    _plna = plna;
}
std::vector<std::string> ExoplanetsModule::getPlna() {
    return _plna;
}

void ExoplanetsModule::setRotation(glm::dmat3 rot) {
    _rotation = rot;
}
glm::dmat3 ExoplanetsModule::getRotation() {
    return _rotation;
}
void ExoplanetsModule::setNorthVector(glm::dvec3 vector) {
    _north = vector;
}
glm::dvec3 ExoplanetsModule::getNorthVector() {
    return _north;
}

scripting::LuaLibrary ExoplanetsModule::luaLibrary() const {
    scripting::LuaLibrary res;
    res.name = "exoplanets";
    res.functions = {
        {
            "addExoplanetSystem",
            &addExoplanetSystem,
            {},
            "string",
            "Adds the nodes to the scene graph of the exoplanet system."
        },
        {
            "removeExoplanetSystem",
            &removeExoplanetSystem,
            {},
            "string",
            "Removes the nodes from the scene graph of the exoplanet system."
        }
    };

    return res;
}

void ExoplanetsModule::internalInitialize(const ghoul::Dictionary&) {
    auto fTask = FactoryManager::ref().factory<Task>();
    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fTask, "No task factory existed");
    fTask->registerClass<ExoplanetsCsvToBinTask>("ExoplanetsCsvToBinTask");
    fRenderable->registerClass<RenderableOrbitdisc>("RenderableOrbitdisc");

    global::callback::initializeGL.push_back([&]() {
        _discoveryMethods = std::make_unique<openspace::exoplanets::DiscoveryMethods>();
        addPropertySubOwner(*_discoveryMethods);
    });

    // Render
    global::callback::render.push_back([&]() {
        if (_discoveryMethods->isDoppler()) {
            std::string starName = global::moduleEngine.module<ExoplanetsModule>()->getStarName();
            std::vector<std::string> planetNames = global::moduleEngine.module<ExoplanetsModule>()->getPlna();
            SceneGraphNode* planetNode = global::renderEngine.scene()->sceneGraphNode(planetNames[0]);
            SceneGraphNode* starNode = global::renderEngine.scene()->sceneGraphNode(starName);
            glm::dvec3 planetPos = planetNode->worldPosition();
            glm::dvec3 starPos = starNode->worldPosition();
            glm::dvec3 starToPosVec = normalize(planetPos - starPos);
            glm::dvec3 starToSunVec = normalize(glm::dvec3(0.0, 0.0, 0.0) - starPos);
            glm::dvec3 north = glm::dvec3(0.0, 0.0, 1.0);
            glm::dvec3 northProjected = glm::normalize(
                glm::length(north) * glm::sin(glm::dot(north, starToSunVec)) * glm::cross(starToSunVec, glm::cross(north, starToSunVec))
            );
            float northAngle = glm::acos(glm::dot(starToPosVec, northProjected)) * 57.2957795;
            float viewAngle = glm::acos(glm::dot(starToPosVec, starToSunVec)) * 57.2957795;

            float imagePos = 0;
            if ( viewAngle <= 90.0 && northAngle <= 90.0) {
                imagePos = viewAngle / -90.0;
            }
            else if (viewAngle > 90.0 && northAngle <= 90.0) {
                imagePos = (180.0 - viewAngle) / -90.0;
            }
            else if (viewAngle > 90.0 && northAngle > 90.0) {
                imagePos = (180.0 - viewAngle) / 90.0;
            }
            else if (viewAngle <= 90.0 && northAngle > 90.0) {
                imagePos = viewAngle / 90.0;
            }
            
            imagePos *= 0.01;
            _discoveryMethods->setDopplerImagePos(imagePos);
        }

        if (_discoveryMethods->isTransit()) {
            std::string starName = global::moduleEngine.module<ExoplanetsModule>()->getStarName();
            std::vector<std::string> planetNames = global::moduleEngine.module<ExoplanetsModule>()->getPlna();
            SceneGraphNode* planetNode = global::renderEngine.scene()->sceneGraphNode(planetNames[0]);
            SceneGraphNode* starNode = global::renderEngine.scene()->sceneGraphNode(starName);
            glm::dvec3 planetPos = planetNode->worldPosition();
            glm::dvec3 starPos = starNode->worldPosition();

            glm::dvec3 starToPosVec = planetPos - starPos;
            glm::dvec3 starToSunVec = normalize(glm::dvec3(0.0, 0.0, 0.0) - starPos);
            
            std::vector<Exoplanet> planets = global::moduleEngine.module<ExoplanetsModule>()->getPlsy();
            float starRadius = planets[0].RSTAR * 6.957E8 * _discoveryMethods->getTransitScaleFactor(); // in m

            glm::dvec3 north = _north;
            float northAngle = glm::acos(glm::dot(normalize(starToPosVec), north)) * 57.2957795;
            float viewAngle = glm::acos(glm::dot(normalize(starToPosVec), starToSunVec)) * 57.2957795;

            glm::dvec3 posVecProjected = starToPosVec - (((dot(starToPosVec, starToSunVec)) / (glm::length(starToSunVec)))*starToSunVec);
            float l = glm::length(posVecProjected); //in m
            float imageYPos = -0.60;

            if (l<(starRadius*0.82) && viewAngle <= 90.0) {
                imageYPos = -0.80;
            }

            float imageXPos = 0;
            if (viewAngle <= 90.0 && northAngle <= 90.0) {
                imageXPos = (viewAngle / 90.0) * 0.5;
            }
            else if (viewAngle > 90.0 && northAngle <= 90.0) {
                imageXPos = (viewAngle / 90.0) * 0.5;
            }
            else if (viewAngle > 90.0 && northAngle > 90.0) {
                imageXPos = (viewAngle / 90.0) * -0.5;
            }
            else if (viewAngle <= 90.0 && northAngle > 90.0) {
                imageXPos = (viewAngle / 90.0) * -0.5;
            }
            imageXPos *= 0.5;
            _discoveryMethods->setTransitImagePos(imageXPos, imageYPos);
        }
    });
}

std::vector<documentation::Documentation> ExoplanetsModule::documentations() const {
    return {
        ExoplanetsCsvToBinTask::documentation()
    };
}

} // namespace openspace
