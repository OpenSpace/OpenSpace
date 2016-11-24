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

#ifndef __SETTINGSENGINE_H__
#define __SETTINGSENGINE_H__

#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalarproperty.h>
#include <openspace/properties/optionproperty.h>

#include <vector>

namespace openspace {
    
class OpenSpaceModule;

class SettingsEngine : public properties::PropertyOwner {
public:
    SettingsEngine();

    void initialize();
    
    void setModules(std::vector<OpenSpaceModule*> modules);

    bool busyWaitForDecode();
    bool logSGCTOutOfOrderErrors();
    bool useDoubleBuffering();

private:
    void initEyeSeparation();
    void initSceneFiles();
    void initShowFrameNumber();
    void initBusyWaitForDecode();
    void initLogSGCTOutOfOrderErrors();
    void initUseDoubleBuffering();

    properties::FloatProperty _eyeSeparation;
    properties::OptionProperty _scenes;
    properties::BoolProperty _showFrameNumber;
    properties::BoolProperty _busyWaitForDecode;
    properties::BoolProperty _logSGCTOutOfOrderErrors;
    properties::BoolProperty _useDoubleBuffering;
    properties::BoolProperty _spiceUseExceptions;

};

} // namespace openspace

#endif //#ifndef __SETTINGSENGINE_H__
