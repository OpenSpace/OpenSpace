/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_MODULE_SONIFICATION___COSMICSONIFICATION___H__
#define __OPENSPACE_MODULE_SONIFICATION___COSMICSONIFICATION___H__

#include <modules/sonification/include/sonificationbase.h>

#include <modules/space/speckloader.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <map>

namespace openspace {

namespace scripting { struct LuaLibrary; }

class CosmicSonification : public SonificationBase {

public:
    CosmicSonification(const std::string& ip, int port);
    virtual ~CosmicSonification();

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
    * Add the given node to the list of nodes for the sonification
    *
    * \param dict the identifier for the node that should be added
    */
    void addNode(const std::string& nodeId);

    /**
    * Add the labels of the given node to the list of labels for the sonification
    *
    * \param dict the identifier for the node that should be added
    */
    void addLabelNode(const std::string& labelNodeId);

    /**
     * Returns the Lua library that contains all Lua functions available to change the
     * cosmic sonification.
     * \return The Lua library that contains all Lua functions available to change the
     * cosmic sonification
     */
    static scripting::LuaLibrary luaLibrary();

private:
    // Precision of distances and angles
    properties::DoubleProperty _anglePrecision;
    properties::DoubleProperty _distancePrecision;

    // Number of samples to smooth out the data for the sonification
    static const int NumSamples = 60;

    struct LabelsData {
    public:
        // Get a smoothed out value from the data
        double distance() const {
            return SonificationBase::calcMedian(distances);
        }
        double HAngle() const {
            return SonificationBase::calcMedian(horizontalAngles);
        }
        double VAngle() const {
            return SonificationBase::calcMedian(verticalAngles);
        }

        // Add values to ring buffers
        void addDistance(const double distance) {
            SonificationBase::addValueToRingBuffer(
                distances,
                indexDistances,
                NumSamples,
                distance
            );
        }
        void addHAngle(const double angle) {
            SonificationBase::addValueToRingBuffer(
                horizontalAngles,
                indexHAngles,
                NumSamples,
                angle
            );
        }
        void addVAngle(const double angle) {
            SonificationBase::addValueToRingBuffer(
                verticalAngles,
                indexVAngles,
                NumSamples,
                angle
            );
        }

    private:
        // Distance, horizontal angle, vertical angle
        std::vector<double> distances = std::vector<double>(NumSamples, 0.0);
        std::vector<double> horizontalAngles = std::vector<double>(NumSamples, 0.0);
        std::vector<double> verticalAngles = std::vector<double>(NumSamples, 0.0);

        // The first "empty" slot in the ring buffer order. The "oldest" value
        int indexDistances = 0;
        int indexHAngles = 0;
        int indexVAngles = 0;
    };

    struct Labels {
        Labels(std::string id = "") {
            identifier = id;
        }

        bool operator==(const std::string& id) {
            return identifier == id;
        }

        std::string identifier;
        speck::Labelset* labels = nullptr;
        DistanceUnit unit;
        bool isInitialized = false;
        std::vector<LabelsData> data;
    };

    struct NodeData {
    public:
        NodeData(std::string id = "") {
            identifier = id;
        }

        bool operator==(const std::string& id) {
            return identifier == id;
        }

        std::string identifier;

        // Get a smoothed out value from the data
        double distance() const {
            return SonificationBase::calcMedian(distances);
        }
        double HAngle() const {
            return SonificationBase::calcMedian(horizontalAngles);
        }
        double VAngle() const {
            return SonificationBase::calcMedian(verticalAngles);
        }

        // Add values to ring buffers
        void addDistance(const double distance) {
            SonificationBase::addValueToRingBuffer(
                distances,
                ringBufferIndexDist,
                NumSamples,
                distance
            );
        }
        void addHAngle(const double angle) {
            SonificationBase::addValueToRingBuffer(
                horizontalAngles,
                ringBufferIndexHAngle,
                NumSamples,
                angle
            );
        }
        void addVAngle(const double angle) {
            SonificationBase::addValueToRingBuffer(
                verticalAngles,
                ringBufferIndexVAngle,
                NumSamples,
                angle
            );
        }

    private:
        // Distance, horizontal angle, vertical angle
        std::vector<double> distances = std::vector<double>(NumSamples, 0.0);
        std::vector<double> horizontalAngles = std::vector<double>(NumSamples, 0.0);
        std::vector<double> verticalAngles = std::vector<double>(NumSamples, 0.0);

        // The first "empty" slot in the ring buffer order. The "oldest" value
        int ringBufferIndexDist = 0;
        int ringBufferIndexHAngle = 0;
        int ringBufferIndexVAngle = 0;
    };

    std::vector<NodeData> _nodes;
    std::vector<Labels> _labels;
};

} // openspace namespace

#endif // __OPENSPACE_MODULE_SONIFICATION___COSMICSONIFICATION___H__