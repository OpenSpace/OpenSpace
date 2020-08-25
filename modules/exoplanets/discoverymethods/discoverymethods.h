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

#ifndef __OPENSPACE_MODULE_EXOPLANETS___DISCOVERY_METHODS___H__
#define __OPENSPACE_MODULE_EXOPLANETS___DISCOVERY_METHODS___H__

#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalar/boolproperty.h>

namespace openspace::exoplanets {

class DiscoveryMethods : public properties::PropertyOwner {
public:
    DiscoveryMethods();
    bool isDoppler();
    bool isTransit();
    bool isReference();
    void setDopplerImagePos(float);
    void setTransitImagePos(float,float);
    float getTransitScaleFactor();

private:
    properties::BoolProperty _showTransit;
    properties::BoolProperty _showDoppler;
    properties::BoolProperty _showSolarSystemReference;

    void addSolarSystemReferenceVisualization();
    void removeSolarSystemReferenceVisualization();
    void addTransitMethodVisualization();
    void removeTransitMethodVisualization();
    void addDopplerMethodVisualization();
    void removeDopplerMethodVisualization();

    void addDirectionsMarkers(glm::dvec3, glm::dvec3, float);
    void removeDirectionsMarkers();
    void scaleNode(std::string, float);
    void moveStar(std::string, float);
    void moveCamera(glm::dvec3);
    void toggleVisabilityOuterPlanets(std::vector<std::string>, std::string);
    void toggleVisabilityPlanet(std::string, std::string);

    float _transitScaleFactor;
};

} // namespace

#endif // __OPENSPACE_MODULE_EXOPLANETS___TRANSIT_METHOD___H__
