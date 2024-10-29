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

#ifndef __OPENSPACE_MODULE_SONIFICATION___FOCUSSONIFICATION___H__
#define __OPENSPACE_MODULE_SONIFICATION___FOCUSSONIFICATION___H__

#include <modules/sonification/include/sonificationbase.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/util/timeconversion.h>

namespace openspace {

class FocusSonification : public SonificationBase {
public:
    FocusSonification(const std::string& ip, int port);
    virtual ~FocusSonification() override = default;

    /**
     * Main update function for the sonification. Checks the current focus node and sends
     * it via the osc connection.
     *
     * \param camera pointer to the camera in the scene (not used in this sonification)
     */
    virtual void update(const Camera*) override;

    /**
     * Function to stop the sonification
     */
    virtual void stop() override;

private:
    // Indices for data items
    static constexpr int NumDataItems = 1;
    static constexpr int FocusNodeIndex = 0;

    /**
     * Update focus node data
     *
     * \return true if the data is new compared to before, otherwise false
     */
    bool getData();

    /**
     * Send current sonification data for the indicated node over the osc connection
     * Order of data: distance, horizontal angle, vertical angle
     */
    void sendData();

    // Variables
    std::string _currentFocus;
};

} // namespace openspace

#endif __OPENSPACE_MODULE_SONIFICATION___FOCUSSONIFICATION___H__
