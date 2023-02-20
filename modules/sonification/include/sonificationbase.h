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

#ifndef __OPENSPACE_MODULE_SONIFICATION___SONIFICATIONBASE___H__
#define __OPENSPACE_MODULE_SONIFICATION___SONIFICATIONBASE___H__

#include <openspace/properties/propertyowner.h>

#include <modules/osc/include/oscconnection.h>
#include <openspace/camera/camera.h>
#include <openspace/scene/scene.h>
#include <openspace/util/distanceconversion.h>

namespace openspace {

class SonificationModule;

class SonificationBase : public properties::PropertyOwner {
public:
    SonificationBase(properties::PropertyOwner::PropertyOwnerInfo info,
        const std::string& ip, int port);
    virtual ~SonificationBase();

    /**
     * Main update function for the sonification
     *
     * \param scene pointer to the scene
     * \param camera pointer to the camera in the scene
     */
    virtual void update(const Scene* scene, const Camera* camera) = 0;

    /**
     * Calculate the distance from the camera to the node with the given identifier, in
     * the given distance unit
     *
     * \param camera pointer to the camera in the scene that the distance should be
     *               calculated from
     * \param identifier the identifier of the node that the distance should be
     *                   calculate to
     * \param unit the distance unit the answer should be in, default is meter
     *
     * \return distance from the camera to the node with the given identifier in the
     *         given distance unit
     */
    static double calculateDistanceTo(const Camera* camera, const std::string& identifier,
       DistanceUnit unit = DistanceUnit::Meter);

    /**
     * Calculate the angle from the camera to the node with the given identifier,
     * in radians
     *
     * \param camera pointer to the camera in the scene that the angle should be
     *               calculated from
     * \param identifier the identifier of the node that the angle should be calculate to
     *
     * \return angle from the camera to the node with the given identifier in radians
     */
    static double calculateAngleTo(const Camera* camera, const std::string& identifier);

    /**
     * Calculate the angle from the first node with the given identifier to the second
     * node with the given identifier, in radians
     *
     * \param camera pointer to the camera in the scene
     * \param idA the identifier of the first node that the angle should be
                  calculated from
     * \param idB the identifier of the second node that the angle should be calculated to
     *
     * \return angle from the first node with the given identifier to the second node with
     *               the given identifier in radians
     */
    static double calculateAngleFromAToB(const Camera* camera, const std::string& idA,
        const std::string& idB);

protected:
    OscConnection* _connection = nullptr;
};

} // namespace openspace

#endif __OPENSPACE_MODULE_SONIFICATION___SONIFICATIONBASE___H__
