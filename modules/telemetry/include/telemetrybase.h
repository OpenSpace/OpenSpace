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

#ifndef __OPENSPACE_MODULE_TELEMETRY___TELEMETRYBASE___H__
#define __OPENSPACE_MODULE_TELEMETRY___TELEMETRYBASE___H__

#include <openspace/properties/propertyowner.h>

#include <modules/opensoundcontrol/include/opensoundcontrolconnection.h>
#include <openspace/camera/camera.h>
#include <openspace/properties/scalar/boolproperty.h>

namespace openspace {

class TelemetryBase : public properties::PropertyOwner {
public:
    TelemetryBase(properties::PropertyOwner::PropertyOwnerInfo info,
        const std::string& ip, int port);
    virtual ~TelemetryBase();

    /**
     * Main update function to gather telemetry data and send it to the Open Sound Control
     * receiver.
     *
     * \param camera The camera in the scene
     */
    virtual void update(const Camera* camera);

    /**
     * Function to stop the gathering of telemetry data.
     */
    virtual void stop();

    /**
     * \return The identifier of the telemetry class, the type of data that is gathered
     *         and sent
     */
    std::string identifier() const;

    /**
     * \return Whether the telemetry monitoring is enabled or not for this type of data
     */
    bool enabled() const;

protected:
    /**
     * Gather telemetry information and update any internal data storage.
     *
     * \param camera The camera in the scene
     * \return `true` if the data was updated, otherwise `false`
     */
    virtual bool updateData(const Camera* camera) = 0;

    /**
     * Send the current telemetry information to the Open Sound Control receiver.
     */
    virtual void sendData() = 0;

    std::string _identifier;
    properties::BoolProperty _enabled;
    OpenSoundControlConnection* _connection = nullptr;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_TELEMETRY___TELEMETRYBASE___H__
