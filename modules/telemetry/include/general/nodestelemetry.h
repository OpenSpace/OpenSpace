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

#ifndef __OPENSPACE_MODULE_TELEMETRY___NODESTELEMETRY___H__
#define __OPENSPACE_MODULE_TELEMETRY___NODESTELEMETRY___H__

#include <modules/telemetry/include/telemetrybase.h>

#include <modules/telemetry/telemetrymodule.h>
#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>

namespace openspace {

namespace scripting { struct LuaLibrary; }

class NodesTelemetry : public TelemetryBase {
public:
    NodesTelemetry(const std::string& ip, int port);
    ~NodesTelemetry() override = default;

    /**
     * Main update function to gather telemetry information from a list of scene graph
     * nodes (distance, horizontal angle, vertical angle) and send it to the Open Sound
     * Control receiver.
     *
     * \param camera The camera in the scene
     */
    void update(const Camera* camera) override;

    /**
     * Add the given node to the list of nodes to gather telemetry data for.
     *
     * \param node The identifier of the node that should be added
     */
    void addNode(std::string node);

    /**
     * Returns the Lua library that contains all Lua functions available for the
     * nodes telemetry.
     *
     * \return The Lua library that contains all Lua functions available for the
     *         nodes telemetry
     */
    static scripting::LuaLibrary luaLibrary();

private:
    static constexpr int NumDataItems = 4;

    // Struct to hold data for all the nodes
    struct TelemetryNode {
        TelemetryNode(std::string id);

        std::string identifier;

        // Distance, horizontal angle, vertical angle (do not store the distance unit
        // here, the option property stores it instead)
        std::vector<double> data = std::vector<double>(NumDataItems - 1, 0.0);
    };

    /**
     * For this telemetry, a more advanced custom updateData function is needed with
     * additional arguments. Therefore, this implementation is left empty and the update
     * function is overriden to use the custom updateData function instead.
     *
     * \param camera The camera in the scene (not used in this case)
     * \return Always return `false` (this function is empty)
     */
    bool updateData(const Camera* camera) override;

    /**
     * For this telemetry, a more advanced custom sendData function is needed with
     * additional arguments. Therefore, this implementation is left empty and the update
     * function is overriden to use the custom updateData function instead.
     */
    void sendData() override;

    /**
     * Update telemetry data (distance, horizontal angle, vertical angle) for the given
     * node.
     *
     * \param camera The camera in the scene
     * \param nodeIndex The index to the internally stored node data that should be
     *        updated
     * \param angleCalculationMode The angle calculation mode to use. This determines
     *        which method to use when calculating the angle.
     * \param includeElevation Whether the additional elevation angle should be calculated
     *
     * \return `true` if the data is new compared to before, otherwise `false`
     */
    bool updateData(const Camera* camera, int nodeIndex,
        TelemetryModule::AngleCalculationMode angleCalculationMode,
        bool includeElevation);

    /**
     * Send current telemetry data for the indicated node to the Open Sound Control
     * receiver. The order of sent data is as follows: distance, horizontal angle,
     * vertical angle, and the unit used for the distance value
     */
    void sendData(int nodeIndex);

    struct PrecisionProperties : properties::PropertyOwner {
        PrecisionProperties(properties::PropertyOwner::PropertyOwnerInfo precisionInfo);

        // The low and high precision values are used in different situations. When the
        // node is the current focus node, then the high precision value is used. This
        // is due to the node being in the current focus and therfore needs better
        // precision. If the node is not the current focus node, then the low precision
        // value is used to save performance.
        properties::DoubleProperty lowDistancePrecision;
        properties::DoubleProperty highDistancePrecision;
        properties::DoubleProperty lowAnglePrecision;
        properties::DoubleProperty highAnglePrecision;
    };

    properties::OptionProperty _distanceUnitOption;
    PrecisionProperties _precisionProperties;

    std::vector<TelemetryNode> _nodes;
    double _anglePrecision = 0.0;
    double _distancePrecision = 0.0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_TELEMETRY___NODESTELEMETRY___H__
