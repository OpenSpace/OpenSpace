/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_SPOUT___SCREENSPACESPOUT___H__
#define __OPENSPACE_MODULE_SPOUT___SCREENSPACESPOUT___H__

#ifdef WIN32

#include <openspace/rendering/screenspacerenderable.h>

#include <modules/spout/spoutlibrary.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/triggerproperty.h>

namespace openspace {

namespace documentation { struct Documentation; }

class ScreenSpaceSpout : public ScreenSpaceRenderable {
public:
    ScreenSpaceSpout(const ghoul::Dictionary& dictionary);

    bool deinitializeGL() override;

    bool isReady() const override;

    void update() override;

    static documentation::Documentation Documentation();

private:
    void bindTexture() override;
    void unbindTexture() override;

    properties::StringProperty _spoutName;
    properties::OptionProperty _spoutSelection;
    properties::TriggerProperty _updateSelection;

    SPOUTHANDLE _receiver;

    bool _isSpoutDirty = true;
    char _currentSenderName[256] = {};
    bool _isFirstUpdate = true;
    bool _isErrorMessageDisplayed = false;
};

} // namespace openspace

#endif // WIN32

#endif // __OPENSPACE_MODULE_SPOUT___SCREENSPACESPOUT___H__
