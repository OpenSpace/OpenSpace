/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <modules/telemetry/include/general/pointcloudtelemetry.h>

#include <modules/opensoundcontrol/include/opensoundcontrolconnection.h>
#include <modules/telemetry/include/util.h>
#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/distanceconversion.h>
#include <openspace/query/query.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <cstdlib>
#include <limits>
#include <utility>

#include "pointcloudtelemetry_lua.inl"

namespace {
    using namespace openspace;

    constexpr std::string_view _loggerCat = "PointCloudTelemetry";

    // NOTE (malej 2026-06-04): OSC has a maximum message size in bytes; this limits the
    // number of points to send. This number was found through trial and error. If the
    // message data structure changes, this upper limit might need to be updated. If this
    // limit is exceeded, OpenSpace will crash.
    constexpr int MaxNumOscPoints = 64;

    // The label is considered part of the OSC message size, so we need to take it into
    // account when determining the maximum number of points to send. If the maximum
    // number of points is 64 (which is the maximum number of simultaneous sound processes
    // SuperCollider can handle), then the maximum label length before a crash is this.
    // OSC requires all message components to be a multiple of 4 bytes, so this is set to
    // a multiple of 4.
    constexpr int MaxLabelSize = 20;

    // Indices for data items
    constexpr int DistanceIndex = 0;
    constexpr int HorizontalAngleIndex = 1;
    constexpr int VerticalAngleIndex = 2;

    static const PropertyOwner::PropertyOwnerInfo PointCloudTelemetryInfo = {
        "PointCloudTelemetry",
        "Point Cloud Telemetry",
        "Telemetry for a point cloud that has been added by the user."
    };

    constexpr Property::PropertyInfo DistanceUnitInfo = {
        "DistanceUnit",
        "Distance unit",
        "The unit used for the distance part of the telemetry information.",
        Property::Visibility::User
    };

    static const PropertyOwner::PropertyOwnerInfo PrecisionInfo = {
        "Precision",
        "Precision",
        "Settings for the precision of the pointcloud telemetry information."
    };

    constexpr Property::PropertyInfo DistancePrecisionInfo = {
        "DistancePrecision",
        "Distance precision",
        "The precision in the specified distance unit used to determine when to send "
        "updated distance data to the Open Sound Control receiver.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo AnglePrecisionInfo = {
        "HighAnglePrecision",
        "Angle precision (high)",
        "The precision in radians used to determine when to send updated angle data to "
        "the Open Sound Control receiver.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo NumIncludedPointsInfo = {
        "NumIncludedPoints",
        "Number of Included Points",
        "Only the number of points specified will be included in the telemetry data. The "
        "points are selected based on distance. For example, if this value is 50, then "
        "only the 50 points closest to the camera will be included in the telemetry data "
        "sent to the Open Sound Control receiver.",
        Property::Visibility::User
    };
} // namespace

namespace openspace {

PointCloudTelemetry::PointCloudTelemetry(const std::string& ip, int port)
    : TelemetryBase(PointCloudTelemetryInfo, ip, port)
    , _distanceUnitOption(DistanceUnitInfo)
    , _precisionProperties(PointCloudTelemetry::PrecisionProperties(PrecisionInfo))
    , _numIncludedPoints(NumIncludedPointsInfo, MaxNumOscPoints, 0, MaxNumOscPoints)
{
    for (int i = 0; i < DistanceUnitNames.size(); i++) {
        _distanceUnitOption.addOption(i, DistanceUnitNames[i].singular.data());
    }

    _distanceUnitOption.setValue(static_cast<int>(DistanceUnit::Kilometer));
    addProperty(_distanceUnitOption);

    _distancePrecision = _precisionProperties.distancePrecision;
    _anglePrecision = _precisionProperties.anglePrecision; 
    addPropertySubOwner(_precisionProperties);

    _numIncludedPoints.onChange([this]() {
        if (_numIncludedPoints > MaxNumOscPoints) {
            LWARNING(std::format(
                "The number of included points cannot be larger than {} due to the "
                "message size limits of Open Sound Control. Reverting to the maximum "
                "allowed value to avoid a crash.",
                MaxNumOscPoints
            ));
            _numIncludedPoints = MaxNumOscPoints;
        }
    });
    addProperty(_numIncludedPoints);
}

PointCloudTelemetry::TelemetryPoint::TelemetryPoint(int idx)
    : index(std::move(idx))
{}

PointCloudTelemetry::PrecisionProperties::PrecisionProperties(
                                           PropertyOwner::PropertyOwnerInfo precisionInfo)
    : PropertyOwner(precisionInfo)
    , distancePrecision(
        DistancePrecisionInfo,
        250,
        0.0,
        std::numeric_limits<double>::max()
    )
    , anglePrecision(AnglePrecisionInfo, 0.05, 0.0, 10.0)
{
    
    distancePrecision.setExponent(20.f);
    addProperty(distancePrecision);
    addProperty(anglePrecision);
}

void PointCloudTelemetry::fetchPointCloud() {
    if (_pointCloudIdentifier.empty()) {
        LERROR("Point cloud identifier is empty");
        return;
    }

    // Find the node
    SceneGraphNode* node = sceneGraphNode(_pointCloudIdentifier);
    if (!node) {
        LERROR(std::format("Could not find point cloud {}", _pointCloudIdentifier));
        return;
    }

    // Find the RenderablePointCloud
    Renderable* renderable = node->renderable();

    if (!renderable) {
        LERROR(std::format(
            "Could not find renderable for node {}", _pointCloudIdentifier
        ));
        return;
    }

    if (renderable->typeAsString() != "RenderablePointCloud") {
        LERROR(std::format(
            "Not supported Renderable type {} detected", renderable->typeAsString()
        ));
        return;
    }

    _pointCloud = reinterpret_cast<RenderablePointCloud*>(renderable);

    if (!_pointCloud) {
        LERROR(std::format(
            "Could not fetch point cloud with identifier {}", _pointCloudIdentifier
        ));
        _pointCloud = nullptr;
        return;
    }
    else if (_pointCloud->dataset().entries.empty()) {
        LWARNING(std::format(
            "Dataset of point cloud {} does not have any entries", _pointCloudIdentifier
        ));
    }

    _points.reserve(_pointCloud->dataset().entries.size());
    for (int i = 0; i < _pointCloud->dataset().entries.size(); ++i) {
        _points.push_back(TelemetryPoint(i));
    }
    
    // The _iterators list is used to keep track of the order of the points based on
    // distance to the camera without needing to change the order of the _points list
    _iterators.reserve(_pointCloud->dataset().entries.size());
    for (auto itr = _points.begin(); itr != _points.end(); ++itr) {
        _iterators.push_back(itr);
    }
}

void PointCloudTelemetry::update(const Camera* camera) {
    if (!_enabled) {
        return;
    }

    if (!_pointCloud) {
        fetchPointCloud();

        if (!_pointCloud) {
            // fetchPointCloud already logs the error
            return;
        }
    }

    // Get the current angle settings
    TelemetryModule* module = global::moduleEngine->module<TelemetryModule>();
    if (!module) {
        LERROR("Could not find the TelemetryModule");
        return;
    }
    TelemetryModule::AngleCalculationMode angleMode = module->angleCalculationMode();
    bool includeElevation = module->includeElevationAngle();

    // Update data for all points in the point cloud
    bool anyDataWasUpdated = false;
    for (int i = 0; i < _pointCloud->dataset().entries.size(); ++i) {
        if (updateData(camera, i, angleMode, includeElevation)) {
            anyDataWasUpdated = true;
        }
    }

    if (anyDataWasUpdated && _numIncludedPoints > 0) {
        sendData();
    }
}

void PointCloudTelemetry::setPointCloudIdentifier(std::string identifier) {
    _pointCloudIdentifier = std::move(identifier);
    fetchPointCloud();
}

// Empty overidden function
bool PointCloudTelemetry::updateData(const Camera*) {
    return false;
}

void PointCloudTelemetry::sendData() {
    // The label can not be longer than the maximum size, cut it if it is too long
    // NOTE (malej 2026-06-16): The OSC receiver label must match this shortened version
    std::string label = "/" + _pointCloudIdentifier.substr(0, MaxLabelSize);
    std::vector<OpenSoundControlDataType> data;
    
    int numPoints =
        std::min(_numIncludedPoints.value(), static_cast<int>(_points.size()));

    // Reserve space for all points plus the distance unit
    data.reserve(numPoints * NumDataItemsPerPoint + 1);

    data.push_back(_distanceUnitOption.value());

    // Sort the _iterators list based on the distance from the point the iterator points
    // to, and the camera
    std::sort(
        _iterators.begin(),
        _iterators.end(),
        [](const auto& a, const auto& b) {
            return a->data[DistanceIndex] < b->data[DistanceIndex];
        }
    );

    for (int i = 0; i < numPoints; ++i) {
        data.push_back(_iterators[i]->index);
        data.push_back(_iterators[i]->data[DistanceIndex]);
        data.push_back(_iterators[i]->data[HorizontalAngleIndex]);
        data.push_back(_iterators[i]->data[VerticalAngleIndex]);
    }

    _connection->send(label, data);
}


bool PointCloudTelemetry::updateData(const Camera* camera, int index,
                               TelemetryModule::AngleCalculationMode angleCalculationMode,
                                                                    bool includeElevation)
{
    if (!_pointCloud) {
        return false;
    }

    if (index < 0 || _points.size() <= index) {
        LWARNING(std::format(
            "Point cloud dataset does not include index {}", index
        ));
        return false;
    }

    double distance = calculateDistanceTo(
        camera,
        _pointCloud->dataset().entries[index].position,
        DistanceUnits[_distanceUnitOption]
    );

    if (std::abs(distance) < std::numeric_limits<double>::epsilon()) {
        // Scene is likely not yet initialized
        return false;
    }

    double horizontalAngle = calculateAngleTo(
        camera,
        _pointCloud->dataset().entries[index].position,
        angleCalculationMode
    );

    double verticalAngle = 0.0;
    if (includeElevation) {
        verticalAngle = calculateElevationAngleTo(
            camera,
            _pointCloud->dataset().entries[index].position,
            angleCalculationMode
        );
    }

    // Check if this data is new, otherwise don't update it
    double prevDistance = _points[index].data[DistanceIndex];
    double prevHorizontalAngle = _points[index].data[HorizontalAngleIndex];
    double prevVerticalAngle = _points[index].data[VerticalAngleIndex];
    bool dataWasUpdated = false;

    if (std::abs(prevDistance - distance) > _distancePrecision) {
        _points[index].data[DistanceIndex] = distance;
        dataWasUpdated = true;
    }

    if (std::abs(prevHorizontalAngle - horizontalAngle) > _anglePrecision) {
        _points[index].data[HorizontalAngleIndex] = horizontalAngle;
        dataWasUpdated = true;
    }

    if (std::abs(prevVerticalAngle - verticalAngle) > _anglePrecision) {
        _points[index].data[VerticalAngleIndex] = verticalAngle;
        dataWasUpdated = true;
    }

    return dataWasUpdated;
}

LuaLibrary PointCloudTelemetry::luaLibrary() {
    return {
        "telemetry",
        {
            codegen::lua::SetPointCloudIdentifier
        }
    };
}

} // namespace openspace
