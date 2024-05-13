/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
        "is not a valid name, an empty image is used.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SelectionInfo = {
        "SpoutSelection",
        "Spout Selection",
        "This property displays all available Spout sender on the system. If one them is "
        "selected, its value is stored in the 'SpoutName' property, overwriting its "
        "previous value.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo UpdateInfo = {
        "UpdateSelection",
        "Update Selection",
        "If this property is trigged, the 'SpoutSelection' options will be refreshed.",
        openspace::properties::Property::Visibility::AdvancedUser
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
    , _spoutReceiver(*this, dictionary)
{
    std::string identifier;
    if (dictionary.hasValue<std::string>(KeyIdentifier)) {
        identifier = dictionary.value<std::string>(KeyIdentifier);
    }
    else {
        identifier = "ScreenSpaceSpout";
    }
    identifier = makeUniqueIdentifier(identifier);
    setIdentifier(std::move(identifier));
}

bool ScreenSpaceSpout::deinitializeGL() {
    _spoutReceiver.release();

    return ScreenSpaceRenderable::deinitializeGL();
}

bool ScreenSpaceSpout::isReady() const {
    return ScreenSpaceRenderable::isReady() && !_spoutReceiver.isReceiving();
}

void ScreenSpaceSpout::update() {
    ScreenSpaceRenderable::update();
    _spoutReceiver.updateReceiver();
}

void ScreenSpaceSpout::bindTexture() {
    _spoutReceiver.saveGLTextureState();
    glBindTexture(GL_TEXTURE_2D, _spoutReceiver.spoutTexture());
}

void ScreenSpaceSpout::unbindTexture() {
    _spoutReceiver.restoreGLTextureState();
}

} // namespace openspace

#endif // WIN32
