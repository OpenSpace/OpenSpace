/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/sonification/include/nodessonification.h>

#include <modules/sonification/sonificationmodule.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/distanceconversion.h>

namespace {
    constexpr std::string_view _loggerCat = "NodesSonification";

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        NodesSonificationInfo =
    {
        "NodesSonification",
        "Nodes Sonification",
        "Sonification of the nodes in our solarsystem"
    };

    constexpr openspace::properties::Property::PropertyInfo DistanceUnitInfo = {
        "DistanceUnit",
        "Distance Unit",
        "The unit used for the distance part of the sonifciation.",
        openspace::properties::Property::Visibility::User
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo PrecisionInfo = {
        "Precision",
        "Precision",
        "Settings for the precision of the sonification"
    };

    constexpr openspace::properties::Property::PropertyInfo LowDistancePrecisionInfo = {
        "LowDistancePrecision",
        "Distance Precision (Low)",
        "The precision in meters used to determin when to send updated distance data "
        "over the OSC connection. This is the lower precision used, for nodes that are "
        "not the current focus",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo HighDistancePrecisionInfo = {
        "HighDistancePrecision",
        "Distance Precision (High)",
        "The precision in meters used to determin when to send updated distance data "
        "over the OSC connection. This is the higher precision used, for nodes the "
        "current focus",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LowAnglePrecisionInfo = {
        "LowAnglePrecision",
        "Angle Precision (Low)",
        "The precision in radians used to determin when to send updated angle data "
        "over the OSC connection. This is the lower precision used, for nodes that are "
        "not the current focus",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo HighAnglePrecisionInfo = {
        "HighAnglePrecision",
        "Angle Precision (High)",
        "The precision in radians used to determin when to send updated angle data "
        "over the OSC connection. This is the higher precision used, for nodes the "
        "current focus",
        openspace::properties::Property::Visibility::User
    };

} // namespace
#include "nodessonification_lua.inl"

namespace openspace {

NodesSonification::NodesSonification(const std::string& ip, int port)
    : SonificationBase(NodesSonificationInfo, ip, port)
    , _distanceUnitOption(
        DistanceUnitInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _precisionProperty(NodesSonification::PrecisionProperty(PrecisionInfo))
{
    // Add all distance units as options in the drop down menu
    for (int i = 0; i < DistanceUnitNamesSingular.size(); ++i) {
        _distanceUnitOption.addOption(i, DistanceUnitNamesSingular[i].data());
    }

    // Select Meters as the default distance unit
    _distanceUnitOption.setValue(static_cast<int>(DistanceUnit::Meter));
    addProperty(_distanceUnitOption);

    // Precision
    _anglePrecision = _precisionProperty.lowAnglePrecision;
    _distancePrecision = _precisionProperty.lowDistancePrecision;
    addPropertySubOwner(_precisionProperty);
}

NodesSonification::~NodesSonification() {
    stop();
}

NodesSonification::PrecisionProperty::PrecisionProperty(
                               properties::PropertyOwner::PropertyOwnerInfo precisionInfo)
    : properties::PropertyOwner(precisionInfo)
    , lowDistancePrecision(
        LowDistancePrecisionInfo,
        10000.0,
        0,
        1e+25
    )
    , highDistancePrecision(
        HighDistancePrecisionInfo,
        1000.0,
        0,
        1e+25
    )
    , lowAnglePrecision(
        LowAnglePrecisionInfo,
        0.1,
        0,
        10
    )
    , highAnglePrecision(
        HighAnglePrecisionInfo,
        0.05,
        0,
        10
    )
{
    addProperty(lowDistancePrecision);
    lowDistancePrecision.setExponent(20.f);

    addProperty(highDistancePrecision);
    highDistancePrecision.setExponent(20.f);

    addProperty(lowAnglePrecision);

    addProperty(highAnglePrecision);
}

void NodesSonification::sendData(int nodeIndex) {
    if (_nodes.size() <= nodeIndex) {
        LWARNING(std::format("Node list does not include index {}", nodeIndex));
        return;
    }

    std::string label = "/" + _nodes[nodeIndex].identifier;
    std::vector<OscDataType> data(NumDataItems);

    // Distance
    data[DistanceIndex] = _nodes[nodeIndex].data[DistanceIndex];

    // Horizontal Angle
    data[HAngleIndex] = _nodes[nodeIndex].data[HAngleIndex];

    // Vertical Angle
    data[VAngleIndex] = _nodes[nodeIndex].data[VAngleIndex];

    // Distance Unit
    data[DistanceUnitIndex] = _distanceUnitOption.getDescriptionByValue(
        _distanceUnitOption.value()
    );

    _connection->send(label, data);
}

// Extract data from the given identifier
bool NodesSonification::getData(const Camera* camera, int nodeIndex) {
    double distance = SonificationBase::calculateDistanceTo(
        camera,
        _nodes[nodeIndex].identifier,
        DistanceUnits[_distanceUnitOption]
    );
    double HAngle = SonificationBase::calculateAngleTo(
        camera,
        _nodes[nodeIndex].identifier
    );

    double VAngle = SonificationBase::calculateElevationAngleTo(
        camera,
        _nodes[nodeIndex].identifier
    );

    if (std::abs(distance) < std::numeric_limits<double>::epsilon()) {
        // Scene is likely not yet initialized
        return false;
    }

    // Check if this data is new, otherwise don't update it
    double prevDistance = _nodes[nodeIndex].data[DistanceIndex];
    double prevHAngle = _nodes[nodeIndex].data[HAngleIndex];
    double prevVAngle = _nodes[nodeIndex].data[VAngleIndex];
    bool shouldSendData = false;

    if (std::abs(prevDistance - distance) > _distancePrecision) {
        _nodes[nodeIndex].data[DistanceIndex] = distance;
        shouldSendData = true;
    }

    if (std::abs(prevHAngle - HAngle) > _anglePrecision) {
        _nodes[nodeIndex].data[HAngleIndex] = HAngle;
        shouldSendData = true;
    }

    if (std::abs(prevVAngle - VAngle) > _anglePrecision) {
        _nodes[nodeIndex].data[VAngleIndex] = VAngle;
        shouldSendData = true;
    }

    return shouldSendData;
}

void NodesSonification::update(const Camera* camera) {
    if (!_enabled) {
        return;
    }

    SonificationModule* module = global::moduleEngine->module<SonificationModule>();
    if (!module) {
        LERROR("Could not find the SonificationModule");
        return;
    }

    const SceneGraphNode* focusNode =
        global::navigationHandler->orbitalNavigator().anchorNode();

    if (!focusNode) {
        return;
    }

    // Update data for all nodes
    for (int i = 0; i < _nodes.size(); ++i) {
        // Increase precision if the node is in focus
        if (focusNode->identifier() == _nodes[i].identifier) {
            _anglePrecision = _precisionProperty.highAnglePrecision;
            _distancePrecision = _precisionProperty.highDistancePrecision;
        }
        else {
            _anglePrecision = _precisionProperty.lowAnglePrecision;
            _distancePrecision = _precisionProperty.lowDistancePrecision;
        }

        bool hasNewData = getData(camera, i);

        // Only send data if something new has happened
        if (hasNewData) {
            sendData(i);
        }
    }
}

void NodesSonification::stop() {}

void NodesSonification::addNode(const std::string& node) {
    _nodes.push_back(node);
}

scripting::LuaLibrary NodesSonification::luaLibrary() {
    return {
        "sonification",
        {
            codegen::lua::AddNodes
        }
    };
}

} // namespace openspace
