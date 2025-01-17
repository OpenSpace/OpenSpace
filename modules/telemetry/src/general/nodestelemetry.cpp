/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

namespace {
    constexpr std::string_view _loggerCat = "NodesTelemetry";

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
        "over the OSC connection. This is the lower precision used, for nodes that are "
        "not the current focus.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo HighDistancePrecisionInfo = {
        "HighDistancePrecision",
        "Distance Precision (High)",
        "The precision in meters used to determine when to send updated distance data "
        "over the OSC connection. This is the higher precision used, for the current "
        "focus node.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LowAnglePrecisionInfo = {
        "LowAnglePrecision",
        "Angle Precision (Low)",
        "The precision in radians used to determine when to send updated angle data "
        "over the OSC connection. This is the lower precision used, for nodes that are "
        "not the current focus.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo HighAnglePrecisionInfo = {
        "HighAnglePrecision",
        "Angle Precision (High)",
        "The precision in radians used to determine when to send updated angle data "
        "over the OSC connection. This is the higher precision used, for the current "
        "focus node.",
        openspace::properties::Property::Visibility::User
    };

} // namespace
#include "nodestelemetry_lua.inl"

namespace openspace {

NodesTelemetry::NodesTelemetry(const std::string& ip, int port)
    : TelemetryBase(NodesTelemetryInfo, ip, port)
    , _distanceUnitOption(
        DistanceUnitInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _precisionProperty(NodesTelemetry::PrecisionProperty(PrecisionInfo))
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

NodesTelemetry::~NodesTelemetry() {
    stop();
}

NodesTelemetry::PrecisionProperty::PrecisionProperty(
                               properties::PropertyOwner::PropertyOwnerInfo precisionInfo)
    : properties::PropertyOwner(precisionInfo)
    , lowDistancePrecision(LowDistancePrecisionInfo, 10000.0, 0, 1e+25)
    , highDistancePrecision(HighDistancePrecisionInfo, 1000.0, 0, 1e+25)
    , lowAnglePrecision(LowAnglePrecisionInfo, 0.1, 0, 10)
    , highAnglePrecision(HighAnglePrecisionInfo, 0.05, 0, 10)
{
    lowDistancePrecision.setExponent(20.f);
    addProperty(lowDistancePrecision);

    highDistancePrecision.setExponent(20.f);
    addProperty(highDistancePrecision);

    addProperty(lowAnglePrecision);

    addProperty(highAnglePrecision);
}

void NodesTelemetry::sendData(int nodeIndex) {
    if (nodeIndex < 0 || _nodes.size() <= nodeIndex) {
        LWARNING(std::format("Node list does not include index {}", nodeIndex));
        return;
    }

    std::string label = "/" + _nodes[nodeIndex].identifier;
    std::vector<OpenSoundControlDataType> data(NumDataItems);

    // Distance
    data[DistanceIndex] = _nodes[nodeIndex].data[DistanceIndex];

    // Horizontal Angle
    data[HorizontalAngleIndex] = _nodes[nodeIndex].data[HorizontalAngleIndex];

    // Vertical Angle
    data[VerticalAngleIndex] = _nodes[nodeIndex].data[VerticalAngleIndex];

    // Distance Unit
    data[DistanceUnitIndex] = _distanceUnitOption.getDescriptionByValue(
        _distanceUnitOption.value()
    );

    _connection->send(label, data);
}

bool NodesTelemetry::getData(const Camera* camera, int nodeIndex,
                             TelemetryModule::AngleCalculationMode angleCalculationMode,
                             bool includeElevation)
{
    double distance = calculateDistanceTo(
        camera,
        _nodes[nodeIndex].identifier,
        DistanceUnits[_distanceUnitOption]
    );
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

    if (std::abs(distance) < std::numeric_limits<double>::epsilon()) {
        // Scene is likely not yet initialized
        return false;
    }

    // Check if this data is new, otherwise don't update it
    double prevDistance = _nodes[nodeIndex].data[DistanceIndex];
    double prevHorizontalAngle = _nodes[nodeIndex].data[HorizontalAngleIndex];
    double prevVerticalAngle = _nodes[nodeIndex].data[VerticalAngleIndex];
    bool shouldSendData = false;

    if (std::abs(prevDistance - distance) > _distancePrecision) {
        _nodes[nodeIndex].data[DistanceIndex] = distance;
        shouldSendData = true;
    }

    if (std::abs(prevHorizontalAngle - horizontalAngle) > _anglePrecision) {
        _nodes[nodeIndex].data[HorizontalAngleIndex] = horizontalAngle;
        shouldSendData = true;
    }

    if (std::abs(prevVerticalAngle - verticalAngle) > _anglePrecision) {
        _nodes[nodeIndex].data[VerticalAngleIndex] = verticalAngle;
        shouldSendData = true;
    }

    return shouldSendData;
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
            _anglePrecision = _precisionProperty.highAnglePrecision;
            _distancePrecision = _precisionProperty.highDistancePrecision;
        }
        else {
            _anglePrecision = _precisionProperty.lowAnglePrecision;
            _distancePrecision = _precisionProperty.lowDistancePrecision;
        }

        bool hasNewData = getData(camera, i, angleMode, includeElevation);

        // Only send data if something new has happened
        if (hasNewData) {
            sendData(i);
        }
    }
}

void NodesTelemetry::stop() {}

void NodesTelemetry::addNode(const std::string& node) {
    _nodes.push_back(std::move(node));
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
