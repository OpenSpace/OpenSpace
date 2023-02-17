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

#ifndef __OPENSPACE_MODULE_SONIFICATION___SONIFICATIONMODULE___H__
#define __OPENSPACE_MODULE_SONIFICATION___SONIFICATIONMODULE___H__

#include "openspace/util/openspacemodule.h"

#include <modules/sonification/include/sonificationbase.h>
#include <atomic>

namespace openspace {

class SonificationBase;

class SonificationModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "SonificationModule";

    SonificationModule();
    ~SonificationModule();

    virtual void internalInitialize(const ghoul::Dictionary&) override;
    virtual void internalDeinitialize() override;

private:
    void update(std::atomic<bool>& isRunning);

    properties::BoolProperty _enabled;
    std::thread _updateThread;
    std::atomic<bool> _isRunning = false;
    std::vector<SonificationBase*> _sonifications;
};

} // namespace openspace

#endif __OPENSPACE_MODULE_SONIFICATION___SONIFICATIONMODULE___H__
