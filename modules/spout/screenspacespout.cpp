/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/spout/screenspacespout.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/logging/logmanager.h>
#include <optional>

namespace {
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

    struct [[codegen::Dictionary(ScreenSpaceSpout)]] Parameters {
        // [[codegen::verbatim(NameInfo.description)]]
        std::optional<std::string> spoutName;
    };
#include "screenspacespout_codegen.cpp"

} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceSpout::Documentation() {
    return codegen::doc<Parameters>("spout_screenspace_spout");
}

ScreenSpaceSpout::ScreenSpaceSpout(const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , _spoutName(NameInfo)
    , _spoutSelection(SelectionInfo)
    , _updateSelection(UpdateInfo)
    , _receiver(GetSpout())
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    std::string identifier;
    if (dictionary.hasValue<std::string>(KeyIdentifier)) {
        identifier = dictionary.value<std::string>(KeyIdentifier);
    }
    else {
        identifier = "ScreenSpaceSpout";
    }
    identifier = makeUniqueIdentifier(identifier);
    setIdentifier(std::move(identifier));

    _spoutName = p.spoutName.value_or(_spoutName);
    _spoutName.onChange([this]() {
        _isSpoutDirty = true;
        _isErrorMessageDisplayed = false;

        _receiver->SetActiveSender(_spoutName.value().c_str());
    });
    addProperty(_spoutName);

    _spoutSelection.onChange([this]() {
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

bool ScreenSpaceSpout::deinitializeGL() {
    _receiver->ReleaseReceiver();
    _receiver->Release();

    return ScreenSpaceRenderable::deinitializeGL();
}

bool ScreenSpaceSpout::isReady() const {
    return ScreenSpaceRenderable::isReady() && !_spoutName.value().empty();
}

void ScreenSpaceSpout::update() {
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

        _receiver->ReleaseReceiver();
        _receiver->GetActiveSender(_currentSenderName);

        unsigned int width;
        unsigned int height;
        const bool hasCreated = _receiver->CreateReceiver(
            _currentSenderName,
            width,
            height
        );
        _objectSize = { width, height };

        if (!hasCreated) {
            LWARNINGC(
                "ScreenSpaceSpout",
                fmt::format("Could not create receiver for {}", _currentSenderName)
            );
            return;
        }
    }

    unsigned int width;
    unsigned int height;
    const bool receiveSuccess = _receiver->ReceiveTexture(
        _currentSenderName,
        width,
        height
    );

    if (!receiveSuccess && !_isErrorMessageDisplayed) {
        LWARNINGC(
            "ScreenSpaceSpout",
            fmt::format("Could not receive texture for {}", _currentSenderName)
        );
        _isErrorMessageDisplayed = true;
    }
}

void ScreenSpaceSpout::bindTexture() {
    _receiver->BindSharedTexture();
}

void ScreenSpaceSpout::unbindTexture() {
    _receiver->UnBindSharedTexture();
}

} // namespace openspace

#endif // WIN32
