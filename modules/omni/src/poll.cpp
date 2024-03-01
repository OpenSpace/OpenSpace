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

#include <modules/omni/include/poll.h>

#include <modules/server/include/jsonconverters.h> // TODO: move this outside server module?
#include <openspace/documentation/documentation.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/json.h>


namespace {
    constexpr std::string_view _loggerCat = "Poll";

    // Explanation of Parameters
    struct [[codegen::Dictionary(Poll)]] Parameters {

        // Explanation of Options

        struct Option {
            // Identifier of something
            std::string identifier;

            std::string name;

            std::optional<std::filesystem::path> url;

            glm::vec3 color;

            std::string script;
        };

        // Unique identifier for this scene
        std::string identifier;

        // Type of scene 
        std::string sceneType;

        // The poll options to vote for
        std::vector<Option> options;

        // Description of this poll
        std::string description;

        // What should happen when we enable this scene
        std::string onEnable;

        // What should happen we we disable this scene
        std::string onDisable;
    };

#include "poll_codegen.cpp"
}

namespace openspace::omni {

documentation::Documentation Poll::Documentation() {
    return codegen::doc<Parameters>("poll");
}

Poll::Poll(const ghoul::Dictionary& dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _dictionary.setValue("Scene", dictionary);
}

nlohmann::json Poll::getAssetInformation() {
    return _dictionary;
}

void Poll::enableScene() {

    onEnable();
}

void Poll::disableScene() {

    onDisable();
}

void Poll::onEnable() {
    // TODO: call or add onEnable script to script queue?
}

void Poll::onDisable() {
    // TODO: call or add onDisable script to script queue?

}

} // namespace openspace::omni
