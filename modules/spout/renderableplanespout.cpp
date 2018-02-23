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

#ifdef WIN32

#include <modules/spout/renderableplanespout.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

#include <ghoul/filesystem/filesystem>
#include <ghoul/misc/defer.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>

namespace {
    const char* KeyName = "Name";

    static const openspace::properties::Property::PropertyInfo NameInfo = {
        "SpoutName",
        "Spout Sender Name",
        "This value explicitly sets the Spout receiver to use a specific name. If this "
        "is not a valid name, an empty image is used."
    };

    static const openspace::properties::Property::PropertyInfo SelectionInfo = {
        "SpoutSelection",
        "Spout Selection",
        "This property displays all available Spout sender on the system. If one them is "
        "selected, its value is stored in the 'SpoutName' property, overwriting its "
        "previous value."
    };
    
    static const openspace::properties::Property::PropertyInfo UpdateInfo = {
        "UpdateSelection",
        "Update Selection",
        "If this property is trigged, the 'SpoutSelection' options will be refreshed."
    };

} // namespace

namespace openspace {

documentation::Documentation RenderablePlaneSpout::Documentation() {
    using namespace openspace::documentation;
    return {
        "ScreenSpace Spout",
        "spout_screenspace_spout",
        {
            {
                KeyName,
                new StringVerifier,
                Optional::Yes,
                "Specifies the GUI name of the ScreenspaceSpout"
            },
            {
                NameInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                NameInfo.description
            }
        }
    };
}

RenderablePlaneSpout::RenderablePlaneSpout(const ghoul::Dictionary& dictionary)
    : RenderablePlane(dictionary)
    , _spoutName(NameInfo)
    , _spoutSelection(SelectionInfo)
    , _updateSelection(UpdateInfo)
    , _receiver(GetSpout())
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderablePlaneSpout"
    );

    if (dictionary.hasKey(KeyName)) {
        setName(dictionary.value<std::string>(KeyName));
    }
    else {
        static int id = 0;
        if (id == 0) {
            setName("ScreenSpaceSpout");
        }
        else {
            setName("ScreenSpaceSpout  " + std::to_string(id));
        }
        ++id;
    }
    _isSpoutDirty = true;

    if (dictionary.hasKey(NameInfo.identifier)) {
        _spoutName = dictionary.value<std::string>(NameInfo.identifier);
        _isSpoutDirty = true;
    }

    _spoutName.onChange([this]() {
        _isSpoutDirty = true; 
        _isErrorMessageDisplayed = false;

        _receiver->SetActiveSender(_spoutName.value().c_str());
    });
    addProperty(_spoutName);

    _spoutSelection.onChange([this](){
        _spoutName = _spoutSelection.option().description;
    });
    _spoutSelection.addOption(0, "");
    addProperty(_spoutSelection);

    _updateSelection.onChange([this]() {
        std::string currentValue = _spoutSelection.options().empty() ?
            "" :
            _spoutSelection.option().description;

        _spoutSelection.clearOptions();
        _spoutSelection.addOption(0, "");

        int nSenders = _receiver->GetSenderCount();

        int idx = 0;

        for (int i = 0; i < nSenders; ++i) {
            char Name[256];
            _receiver->GetSenderName(i, Name, 256);

            _spoutSelection.addOption(i + 1, Name);

            if (currentValue == Name) {
                idx = i + 1;
            }
        }

        _spoutSelection = idx;
    });
    addProperty(_updateSelection);
}

void RenderablePlaneSpout::deinitializeGL() {
    _receiver->ReleaseReceiver();
    _receiver->Release();

    RenderablePlane::deinitializeGL();
}

void RenderablePlaneSpout::update(const UpdateData& data) {
    RenderablePlane::update(data);

    if (_isFirstUpdate) {
        defer { _isFirstUpdate = false; };

        // Trigger an update; the value is a dummy that is ignored
        _updateSelection.set(0);

        // #0 is the empty string and we just pick the first one after that (if it exists)
        if (_spoutSelection.options().size() > 1) {
            _spoutSelection = 1;
        }
    }

    if (_spoutName.value().empty()) {
        return;
    }

    if (_isSpoutDirty) {
        defer { _isSpoutDirty = false; };

        std::memset(_currentSenderName, 0, 256);
        unsigned int width;
        unsigned int height;

        _receiver->ReleaseReceiver();

        _receiver->GetActiveSender(_currentSenderName);

        bool createSuccess = _receiver->CreateReceiver(
            _currentSenderName,
            width,
            height
        );

        if (!createSuccess) {
            LWARNINGC(
                "ScreenSpaceSpout",
                "Could not create receiver for " << _currentSenderName
            );
            return;
        }
    }

    unsigned int width;
    unsigned int height;
    
    bool receiveSuccess = _receiver->ReceiveTexture(
        _currentSenderName,
        width,
        height
    );

    if (!receiveSuccess && !_isErrorMessageDisplayed) {
        LWARNINGC(
            "ScreenSpaceSpout",
            "Could not receive texture for " << _currentSenderName
        );
        _isErrorMessageDisplayed = true;
    }
}

void RenderablePlaneSpout::bindTexture() {
    _receiver->BindSharedTexture();
}

void RenderablePlaneSpout::unbindTexture() {
    _receiver->UnBindSharedTexture();
}


} // namespace openspace

#endif // WIN32
