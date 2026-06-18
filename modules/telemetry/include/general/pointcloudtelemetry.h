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

#ifndef __OPENSPACE_MODULE_TELEMETRY___POINTCLOUDTELEMETRY___H__
#define __OPENSPACE_MODULE_TELEMETRY___POINTCLOUDTELEMETRY___H__

#include <modules/telemetry/include/telemetrybase.h>

#include <modules/base/rendering/pointcloud/renderablepointcloud.h>
#include <modules/telemetry/telemetrymodule.h>
#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>

namespace openspace {

class Camera;
struct LuaLibrary;

class PointCloudTelemetry : public TelemetryBase {
public:
    PointCloudTelemetry(const std::string& ip, int port);
    ~PointCloudTelemetry() override = default;

    /**
     * Main update function to gather telemetry information from the list of points in the
     * point cloud (point index, distance, horizontal angle, vertical angle) and send it
     * to the Open Sound Control receiver.
     *
     * \param camera The camera in the scene.
     */
    void update(const Camera* camera) override;

    /**
     * Set the given identifier as the identifier of the point cloud to gather telemetry
     * data for.
     *
     * \param identifier The identifier of the point cloud to gather telemetry data for.
     */
    void setPointCloudIdentifier(std::string identifier);

    /**
     * Returns the Lua library that contains all Lua functions available for the
     * point cloud telemetry.
     *
     * \return The Lua library that contains all Lua functions available for the
     *         point cloud telemetry.
     */
    static LuaLibrary luaLibrary();

private:
    static constexpr int NumDataItemsPerPoint = 4;

    /**
     * Struct to hold data for all the points.
     */
    struct TelemetryPoint {
        TelemetryPoint(int idx);

        int index = -1;

        /// Distance, horizontal angle, vertical angle (index is stored seperatly).
        std::vector<double> data = std::vector<double>(NumDataItemsPerPoint - 1, 0.0);
    };

    /**
     * Use the already set point cloud identifier to find it in the scene and store a
     * pointer to it for easy access.
     */
    void fetchPointCloud();

    /**
     * For this telemetry, a more advanced custom updateData function is needed with
     * additional arguments. Therefore, this implementation is left empty and the update
     * function is overriden to use the custom updateData function instead.
     *
     * \param camera The camera in the scene (not used in this case).
     * \return Always return `false` (this function is empty).
     */
    bool updateData(const Camera* camera) override;

    /**
     * Send the current telemetry data for the points in the point cloud to the Open Sound
     * Control receiver. The order of the sent data is as follows: the unit used for the
     * distance values, {point index, distance, horizontal angle, vertical angle}, where
     * the items in parentheses are repeated for the X closest points to the camera. X is
     * the value of the `NumIncludedPoints` property.
     */
    void sendData() override;

    /**
     * Update telemetry data (distance, horizontal angle, vertical angle) for the given
     * point index.
     *
     * \param camera The camera in the scene.
     * \param index The index to the point in the point cloud whose internally stored
     *        data should be updated.
     * \param angleCalculationMode The angle calculation mode to use. This determines
     *        which method to use when calculating the angle.
     * \param includeElevation Whether the additional elevation angle should be calculated.
     * 
     * \return `true` if the data is new compared to before, otherwise `false`.
     */
    bool updateData(const Camera* camera, int index,
        TelemetryModule::AngleCalculationMode angleCalculationMode,
        bool includeElevation);

    struct PrecisionProperties : PropertyOwner {
        PrecisionProperties(PropertyOwner::PropertyOwnerInfo precisionInfo);

        DoubleProperty distancePrecision;
        DoubleProperty anglePrecision;
    };

    OptionProperty _distanceUnitOption;
    PrecisionProperties _precisionProperties;
    IntProperty _numIncludedPoints;

    std::string _pointCloudIdentifier;
    RenderablePointCloud* _pointCloud = nullptr;
    std::vector<TelemetryPoint> _points;
    std::vector<std::vector<TelemetryPoint>::iterator> _iterators;
    double _anglePrecision = 0.0;
    double _distancePrecision = 0.0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_TELEMETRY___POINTCLOUDTELEMETRY___H__
