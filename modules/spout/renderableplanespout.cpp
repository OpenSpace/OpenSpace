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
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* LoggerCat = "ScreenSpaceSpout";

    constexpr const char* KeyName = "Name";

    constexpr openspace::properties::Property::PropertyInfo NameInfo = {
        "SpoutName",
        "Spout Sender Name",
        "This value explicitly sets the Spout receiver to use a specific name. If this "
        "is not a valid name, an empty image is used."
    };

    constexpr openspace::properties::Property::PropertyInfo SelectionInfo = {
        "SpoutSelection",
        "Spout Selection",
        "This property displays all available Spout sender on the system. If one them is "
        "selected, its value is stored in the 'SpoutName' property, overwriting its "
        "previous value."
    };

    constexpr openspace::properties::Property::PropertyInfo UpdateInfo = {
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

    int iIdentifier = 0;
    if (_identifier.empty()) {
        static int id = 0;
        iIdentifier = id;

        if (iIdentifier == 0) {
            setIdentifier("ScreenSpaceSpout");
        }
        else {
            setIdentifier("ScreenSpaceSpout" + std::to_string(iIdentifier));
        }
        ++id;
    }

    if (_guiName.empty()) {
        // Adding an extra space to the user-facing name as it looks nicer
        setGuiName("ScreenSpaceSpout " + std::to_string(iIdentifier));
    }

    if (dictionary.hasKey(NameInfo.identifier)) {
        _spoutName = dictionary.value<std::string>(NameInfo.identifier);
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
        const std::string& currentValue = _spoutSelection.options().empty() ?
            "" :
            _spoutSelection.option().description;

        _spoutSelection.clearOptions();
        _spoutSelection.addOption(0, "");

        const int nSenders = _receiver->GetSenderCount();
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
        // Trigger an update; the value is a dummy that is ignored
        _updateSelection.set(0);

        // #0 is the empty string and we just pick the first one after that (if it exists)
        if (_spoutSelection.options().size() > 1) {
            _spoutSelection = 1;
        }

        _isFirstUpdate = false;
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

        bool hasCreated = _receiver->CreateReceiver(_currentSenderName, width, height);
        if (!hasCreated) {
            LWARNINGC(
                LoggerCat,
                fmt::format("Could not create receiver for {}", _currentSenderName)
            );
            return;
        }
    }

    unsigned int width;
    unsigned int height;
    const bool hasReceived = _receiver->ReceiveTexture(_currentSenderName, width, height);

    if (!hasReceived && !_isErrorMessageDisplayed) {
        LWARNINGC(
            LoggerCat,
            fmt::format("Could not receive texture for {}", _currentSenderName)
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
