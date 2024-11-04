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

#ifndef __OPENSPACE_MODULE_SONIFICATION___NODESSONIFICATION___H__
#define __OPENSPACE_MODULE_SONIFICATION___NODESSONIFICATION___H__

#include <modules/sonification/include/sonificationbase.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>

namespace openspace {

namespace scripting { struct LuaLibrary; }

class NodesSonification : public SonificationBase {
public:
    NodesSonification(const std::string& ip, int port);
    virtual ~NodesSonification() override;

    /**
     * Main update function for the sonification
     *
     * \param camera pointer to the camera in the scene
     */
    virtual void update(const Camera* camera) override;

    /**
     * Function to stop the sonification
     */
    virtual void stop() override;

    /**
    * Add the given node to the list of nodes
    *
    * \param dict the node that should be added
    */
    void addNode(ghoul::Dictionary dict);

    /**
     * Returns the Lua library that contains all Lua functions available to change the
     * nodes sonification.
     *
     * \return The Lua library that contains all Lua functions available to change the
     * nodes sonification
     */
    static scripting::LuaLibrary luaLibrary();

private:
    // Indices for data items
    static constexpr int NumDataItems = 4;
    static constexpr int DistanceIndex = 0;
    static constexpr int HAngleIndex = 1;
    static constexpr int VAngleIndex = 2;
    static constexpr int DistanceUnitIndex = 3;

    // Struct to hold data for all the nodes
    struct SonificationNode {
        SonificationNode(std::string id = "") {
            identifier = id;
        }

        std::string identifier;

        // Distance, horizontal angle, vertical angle
        std::vector<double> data = std::vector<double>(NumDataItems);
    };

    /**
     * Update distance and angle data for the given node
     *
     * \param camera pointer to the camera in the scene. Used to calculated the data for
     *        the node
     * \param nodeIndex index to the internally stored node data that should be updated
     *
     * \return true if the data is new compared to before, otherwise false
     */
    bool getData(const Camera* camera, int nodeIndex);

    /**
     * Send current sonification data for the indicated node over the osc connection
     * Order of data: distance, horizontal angle, vertical angle, unit used for the
     * distance value
     */
    void sendData(int nodeIndex);

    // Properties
    struct PrecisionProperty : properties::PropertyOwner {
        PrecisionProperty(properties::PropertyOwner::PropertyOwnerInfo precisionInfo);

        properties::DoubleProperty lowDistancePrecision;
        properties::DoubleProperty highDistancePrecision;
        properties::DoubleProperty lowAnglePrecision;
        properties::DoubleProperty highAnglePrecision;
    };

    properties::OptionProperty _distanceUnitOption;
    PrecisionProperty _precisionProperty;

    // Variables
    double _anglePrecision = 0.0;
    double _distancePrecision = 0.0;
    std::vector<SonificationNode> _nodes;
};

} // namespace openspace

#endif __OPENSPACE_MODULE_SONIFICATION___NODESSONIFICATION___H__
