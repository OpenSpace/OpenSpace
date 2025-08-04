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

#ifndef __OPENSPACE_MODULE_TELEMETRY___FOCUSTELEMETRY___H__
#define __OPENSPACE_MODULE_TELEMETRY___FOCUSTELEMETRY___H__

#include <modules/telemetry/include/telemetrybase.h>

#include <openspace/properties/misc/optionproperty.h>

namespace openspace {

class FocusTelemetry : public TelemetryBase {
public:
    FocusTelemetry(const std::string& ip, int port);
    ~FocusTelemetry() override = default;

private:
    /**
     * Gather focus telemetry information (current focus node).
     *
     * \param camera The camera in the scene (not used in this case)
     * \return `true` if the data is new compared to before, otherwise `false`
     */
    bool updateData(const Camera* camera) override;

    /**
     * Send the current focus telemetry information to the Open Sound Control
     * receiver. The order of sent data is as follows: Current focus node.
     */
    void sendData() override;

    std::string _currentFocus;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_TELEMETRY___FOCUSTELEMETRY___H__
