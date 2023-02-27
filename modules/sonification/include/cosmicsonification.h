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
#include <map>

namespace openspace {

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

private:
    struct LabelsData {
        speck::Labelset* labels = nullptr;
        bool isInitialized = false;

        // std::vector<std::vector<distance, angle>>
        std::vector<std::vector<double>> prevValues;
    };

    double _anglePrecision;
    double _distancePrecision;

    // std::map<<Identifier, std::vector<distance, angle>>>
    std::map<std::string, std::vector<double>> _nodeData;

    // std::map<Identifier, Labels data>
    std::map<std::string, LabelsData> _labelsData;
};

} // openspace namespace

#endif // __OPENSPACE_MODULE_SONIFICATION___COSMICSONIFICATION___H__
