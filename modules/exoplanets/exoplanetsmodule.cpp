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

#include <modules/exoplanets/rendering/renderableorbitdisc.h>
#include <modules/exoplanets/tasks/exoplanetscsvtobintask.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/factorymanager.h>
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

Exoplanet ExoplanetsModule::closestExoplanet() {
    return _exo;
}

void ExoplanetsModule::setStarName(std::string starName) {
    _starName = starName;
}

std::string ExoplanetsModule::getStarName() {
    return _starName;
}

void ExoplanetsModule::setPlanetSystem(std::vector<Exoplanet> planets) {
    _planetSystem = planets;
}

std::vector<Exoplanet> ExoplanetsModule::planetSystem() {
    return _planetSystem;
}

void ExoplanetsModule::setPlanetNames(std::vector<std::string> names) {
    _planetNames = names;
}

std::vector<std::string> ExoplanetsModule::planetNames() {
    return _planetNames;
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
            &exoplanets::luascriptfunctions::addExoplanetSystem,
            {},
            "string",
            "Adds the nodes to the scene graph of the exoplanet system."
        },
        {
            "removeExoplanetSystem",
            &exoplanets::luascriptfunctions::removeExoplanetSystem,
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
            std::vector<std::string> planetNames = global::moduleEngine.module<ExoplanetsModule>()->planetNames();
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
            float northAngle = glm::acos(glm::dot(starToPosVec, northProjected)) * 57.2957795f;
            float viewAngle = glm::acos(glm::dot(starToPosVec, starToSunVec)) * 57.2957795f;

            float imagePos = 0.0f;
            if ( viewAngle <= 90.f && northAngle <= 90.f) {
                imagePos = viewAngle / -90.f;
            }
            else if (viewAngle > 90.f && northAngle <= 90.f) {
                imagePos = (180.f - viewAngle) / -90.f;
            }
            else if (viewAngle > 90.f && northAngle > 90.f) {
                imagePos = (180.f - viewAngle) / 90.f;
            }
            else if (viewAngle <= 90.f && northAngle > 90.f) {
                imagePos = viewAngle / 90.f;
            }
            
            imagePos *= 0.01f;
            _discoveryMethods->setDopplerImagePos(imagePos);
        }

        if (_discoveryMethods->isTransit()) {
            std::string starName = global::moduleEngine.module<ExoplanetsModule>()->getStarName();
            std::vector<std::string> planetNames = global::moduleEngine.module<ExoplanetsModule>()->planetNames();
            const SceneGraphNode* planetNode = global::renderEngine.scene()->sceneGraphNode(planetNames[0]);
            const SceneGraphNode* starNode = global::renderEngine.scene()->sceneGraphNode(starName);
            glm::dvec3 planetPosition = planetNode->worldPosition();
            glm::dvec3 starPosition = starNode->worldPosition();

            glm::dvec3 starToPosVec = planetPosition - starPosition;
            glm::dvec3 starToSunVec = normalize(glm::dvec3(0.0, 0.0, 0.0) - starPosition);
            
            std::vector<Exoplanet> planets = global::moduleEngine.module<ExoplanetsModule>()->planetSystem();
            float starRadius = planets[0].RSTAR * 6.957E8f * _discoveryMethods->getTransitScaleFactor(); // in m

            float northAngle = glm::acos(glm::dot(normalize(starToPosVec), _north)) * 57.2957795f;
            float viewAngle = glm::acos(glm::dot(normalize(starToPosVec), starToSunVec)) * 57.2957795f;

            glm::dvec3 posVecProjected = starToPosVec - (((dot(starToPosVec, starToSunVec)) / (glm::length(starToSunVec)))*starToSunVec);
            float l = static_cast<float>(glm::length(posVecProjected)); //in m
            float imageYPos = -0.6f;

            if (l < (starRadius * 0.82f) && viewAngle <= 90.f) {
                imageYPos = -0.8f;
            }

            float imageXPos = 0.f;
            if (viewAngle <= 90.f && northAngle <= 90.f) {
                imageXPos = (viewAngle / 90.f) * 0.5f;
            }
            else if (viewAngle > 90.f && northAngle <= 90.f) {
                imageXPos = (viewAngle / 90.f) * 0.5f;
            }
            else if (viewAngle > 90.f && northAngle > 90.f) {
                imageXPos = (viewAngle / 90.f) * -0.5f;
            }
            else if (viewAngle <= 90.f && northAngle > 90.f) {
                imageXPos = (viewAngle / 90.f) * -0.5f;
            }
            imageXPos *= 0.5f;
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
