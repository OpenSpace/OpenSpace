/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __OPENSPACE_CORE___DEVICEIDENTIFIER___H__
#define __OPENSPACE_CORE___DEVICEIDENTIFIER___H__

// std includes
#include <array>
#include <mutex>
#include <memory>

namespace openspace {

#define MAXDEVICES 16

enum class InputDevice {NONE, UNKNOWN, SPACENAVIGATOR, XBOX}; 

class DeviceIdentifier {
public:
    static DeviceIdentifier& ref();
    virtual ~DeviceIdentifier();

    static void init();
    static void deinit();
    static bool isInitialized();
    
    void scanDevices();
    const int numberOfDevices() const;
    const InputDevice type(const int device) const;
    
    void update();
    void update(const int device);

    const int getButtons(const int device, unsigned char **buttons = nullptr) const;
    const int getAxes(const int device, float **axespos = nullptr) const;
    void get(const int device, unsigned char **buttons, float **axespos) const;
    
private:
    // singleton
    static DeviceIdentifier* this_;
    DeviceIdentifier(void);
    DeviceIdentifier(const DeviceIdentifier& src);
    DeviceIdentifier& operator=(const DeviceIdentifier& rhs);


    // member variables
    int devices_;
    std::array<InputDevice, MAXDEVICES> inputDevice_;
    std::array<int, MAXDEVICES> numberOfAxes_;
    std::array<int, MAXDEVICES> numberOfButtons_;
    std::array<float *, MAXDEVICES> axesPos_;
    std::array<unsigned char *, MAXDEVICES> buttons_;

};

} // namespace openspace

#endif // __OPENSPACE_CORE___DEVICEIDENTIFIER___H__
