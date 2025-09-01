/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/telemetry/include/general/nodestelemetry.h>

#include <modules/telemetry/include/util.h>
#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/distanceconversion.h>

#include "nodestelemetry_lua.inl"

namespace {
    constexpr std::string_view _loggerCat = "NodesTelemetry";

    // Indices for data items
    constexpr int DistanceIndex = 0;
    constexpr int HorizontalAngleIndex = 1;
    constexpr int VerticalAngleIndex = 2;
    constexpr int DistanceUnitIndex = 3;

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        NodesTelemetryInfo =
    {
        "NodesTelemetry",
        "Nodes Telemetry",
        "Telemetry of a list of nodes that has been added by the user."
    };

    constexpr openspace::properties::Property::PropertyInfo DistanceUnitInfo = {
        "DistanceUnit",
        "Distance Unit",
        "The unit used for the distance part of the telemetry information.",
        openspace::properties::Property::Visibility::User
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo PrecisionInfo = {
        "Precision",
        "Precision",
        "Settings for the precision of the nodes telemetry information."
    };

    constexpr openspace::properties::Property::PropertyInfo LowDistancePrecisionInfo = {
        "LowDistancePrecision",
        "Distance Precision (Low)",
        "The precision in meters used to determine when to send updated distance data "
        "to the Open Sound Control receiver. This is the low precision value (low level "
        "of detail) that is used for objects that are not the current focus, saving "
        "performance.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo HighDistancePrecisionInfo = {
        "HighDistancePrecision",
        "Distance Precision (High)",
        "The precision in meters used to determine when to send updated distance data "
        "to the Open Sound Control receiver. This is the high precision value (high "
        "level of detail) that is used when the monitored object is the current focus, "
        "providing more accurate data.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LowAnglePrecisionInfo = {
        "LowAnglePrecision",
        "Angle Precision (Low)",
        "The precision in radians used to determine when to send updated angle data "
        "to the Open Sound Control receiver. This is the low precision value (low level "
        "of detail) that is used for objects that are not the current focus, saving "
        "performance.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo HighAnglePrecisionInfo = {
        "HighAnglePrecision",
        "Angle Precision (High)",
        "The precision in radians used to determine when to send updated angle data "
        "to the Open Sound Control receiver. This is the high precision value (high "
        "level of detail) that is used when the monitored object is the current focus, "
        "providing more accurate data.",
        openspace::properties::Property::Visibility::User
    };

} // namespace

namespace openspace {

NodesTelemetry::NodesTelemetry(const std::string& ip, int port)
    : TelemetryBase(NodesTelemetryInfo, ip, port)
    , _distanceUnitOption(DistanceUnitInfo)
    , _precisionProperties(NodesTelemetry::PrecisionProperties(PrecisionInfo))
{
    for (int i = 0; i < DistanceUnitNames.size(); ++i) {
        _distanceUnitOption.addOption(i, DistanceUnitNames[i].singular.data());
    }

    _distanceUnitOption.setValue(static_cast<int>(DistanceUnit::Meter));
    addProperty(_distanceUnitOption);

    _anglePrecision = _precisionProperties.lowAnglePrecision;
    _distancePrecision = _precisionProperties.lowDistancePrecision;
    addPropertySubOwner(_precisionProperties);
}

NodesTelemetry::TelemetryNode::TelemetryNode(std::string id)
    : identifier(std::move(id))
{}

NodesTelemetry::PrecisionProperties::PrecisionProperties(
                               properties::PropertyOwner::PropertyOwnerInfo precisionInfo)
    : properties::PropertyOwner(precisionInfo)
    , lowDistancePrecision(LowDistancePrecisionInfo, 10000.0, 0.0, 1.0e+25)
    , highDistancePrecision(HighDistancePrecisionInfo, 1000.0, 0.0, 1.0e+25)
    , lowAnglePrecision(LowAnglePrecisionInfo, 0.1, 0.0, 10.0)
    , highAnglePrecision(HighAnglePrecisionInfo, 0.05, 0.0, 10.0)
{
    lowDistancePrecision.setExponent(20.f);
    addProperty(lowDistancePrecision);
    highDistancePrecision.setExponent(20.f);
    addProperty(highDistancePrecision);
    addProperty(lowAnglePrecision);
    addProperty(highAnglePrecision);
}

void NodesTelemetry::update(const Camera* camera) {
    if (!_enabled) {
        return;
    }

    const SceneGraphNode* focusNode =
        global::navigationHandler->orbitalNavigator().anchorNode();

    if (!focusNode) {
        // Scene is likely not yet initialized
        return;
    }

    // Get the current angle settings
    TelemetryModule* module = global::moduleEngine->module<TelemetryModule>();
    if (!module) {
        LERROR("Could not find the TelemetryModule");
        return;
    }
    TelemetryModule::AngleCalculationMode angleMode = module->angleCalculationMode();
    bool includeElevation = module->includeElevationAngle();

    // Update data for all nodes
    for (int i = 0; i < _nodes.size(); ++i) {
        // Increase precision if the node is in focus
        if (focusNode->identifier() == _nodes[i].identifier) {
            _anglePrecision = _precisionProperties.highAnglePrecision;
            _distancePrecision = _precisionProperties.highDistancePrecision;
        }
        else {
            _anglePrecision = _precisionProperties.lowAnglePrecision;
            _distancePrecision = _precisionProperties.lowDistancePrecision;
        }

        const bool dataWasUpdated = updateData(camera, i, angleMode, includeElevation);

        if (dataWasUpdated) {
            sendData(i);
        }
    }
}

void NodesTelemetry::addNode(std::string node) {
    _nodes.push_back(std::move(node));
}

// Empty overidded functions
bool NodesTelemetry::updateData(const Camera*) {
    return false;
}
void NodesTelemetry::sendData() {}


bool NodesTelemetry::updateData(const Camera* camera, int nodeIndex,
                               TelemetryModule::AngleCalculationMode angleCalculationMode,
                                                                    bool includeElevation)
{
    double distance = calculateDistanceTo(
        camera,
        _nodes[nodeIndex].identifier,
        DistanceUnits[_distanceUnitOption]
    );

    if (std::abs(distance) < std::numeric_limits<double>::epsilon()) {
        // Scene is likely not yet initialized
        return false;
    }

    double horizontalAngle =
        calculateAngleTo(camera, _nodes[nodeIndex].identifier, angleCalculationMode);

    double verticalAngle = 0.0;
    if (includeElevation) {
        verticalAngle = calculateElevationAngleTo(
            camera,
            _nodes[nodeIndex].identifier,
            angleCalculationMode
        );
    }

    // Check if this data is new, otherwise don't update it
    double prevDistance = _nodes[nodeIndex].data[DistanceIndex];
    double prevHorizontalAngle = _nodes[nodeIndex].data[HorizontalAngleIndex];
    double prevVerticalAngle = _nodes[nodeIndex].data[VerticalAngleIndex];
    bool dataWasUpdated = false;

    if (std::abs(prevDistance - distance) > _distancePrecision) {
        _nodes[nodeIndex].data[DistanceIndex] = distance;
        dataWasUpdated = true;
    }

    if (std::abs(prevHorizontalAngle - horizontalAngle) > _anglePrecision) {
        _nodes[nodeIndex].data[HorizontalAngleIndex] = horizontalAngle;
        dataWasUpdated = true;
    }

    if (std::abs(prevVerticalAngle - verticalAngle) > _anglePrecision) {
        _nodes[nodeIndex].data[VerticalAngleIndex] = verticalAngle;
        dataWasUpdated = true;
    }

    return dataWasUpdated;
}

void NodesTelemetry::sendData(int nodeIndex) {
    if (nodeIndex < 0 || _nodes.size() <= nodeIndex) {
        LWARNING(std::format("Node list does not include index {}", nodeIndex));
        return;
    }

    std::string label = "/" + _nodes[nodeIndex].identifier;
    std::vector<OpenSoundControlDataType> data(NumDataItems);

    data[DistanceIndex] = _nodes[nodeIndex].data[DistanceIndex];
    data[HorizontalAngleIndex] = _nodes[nodeIndex].data[HorizontalAngleIndex];
    data[VerticalAngleIndex] = _nodes[nodeIndex].data[VerticalAngleIndex];
    data[DistanceUnitIndex] = _distanceUnitOption.getDescriptionByValue(
        _distanceUnitOption.value()
    );

    _connection->send(label, data);
}

scripting::LuaLibrary NodesTelemetry::luaLibrary() {
    return {
        "telemetry",
        {
            codegen::lua::AddNodes
        }
    };
}

} // namespace openspace
