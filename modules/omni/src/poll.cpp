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

#include <modules/omni/include/utility.h>
#include <modules/server/include/jsonconverters.h> // TODO: move this outside server module?
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/json.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/json_helper.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>


namespace {
    constexpr std::string_view _loggerCat = "Poll";

    constexpr std::string_view PollMessageKeyVote = "vote";
    constexpr std::string_view PollMessageKeyPollResult = "pollResult";

    // Explanation of Poll
    struct [[codegen::Dictionary(Poll)]] Parameters {
        // Explanation of Options
        struct Option {
            // Identifier of something
            std::string identifier;

            std::string name;

            std::string script;

            std::optional<std::filesystem::path> url;

            std::optional<glm::vec3> color;
        };

        // The poll options to vote for
        std::vector<Option> options;

        // Description of this poll. Will show up on the web page above the options.
        std::optional<std::string> description;

        // If set to true, allow users to change their vote after casting the first time.
        // If false, users cannot change their vote.
        std::optional<int> allowMultipleVoting;

        std::optional<bool> allowChangeVote;
    };
#include "poll_codegen.cpp"
}

namespace openspace::omni {

/**
 * Json converter for an option.
 */
void to_json(nlohmann::json& j, const Poll::Option& o) {
    j = {
        { "identifier", o.identifier },
        { "name", escapedJson(o.name) }
        // Note that the GUI does not need to know about the script, so we don't
        // include it here
    };

    if (o.color.has_value()) {
        // Format the color in a wass that is CSS compatible
        glm::ivec3 c = glm::ivec3(*o.color * 255.f);
        j["color"] = fmt::format("rgb({}, {}, {})", c.r, c.g, c.b);
    }

    if (o.url.has_value()) {
        j["url"] = escapedJson(o.url->string());
    }
}

documentation::Documentation Poll::Documentation() {
    return codegen::doc<Parameters>(
        "omni_poll",
        Scenario::Documentation()
    );
}

Poll::Poll(const ghoul::Dictionary& dictionary)
    : Scenario(dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    if (p.allowMultipleVoting.has_value()) {
        _allowMultipleVoting = p.allowMultipleVoting.value();
    }
    _allowChangeVote = p.allowChangeVote.value_or(false);
    _description = p.description.value_or("");

    for (const auto& option : p.options) {
        _options.push_back({
            .identifier = option.identifier,
            .name = option.name,
            .script = option.script,
            .url = option.url,
            .color = option.color
        });
    }
}

nlohmann::json Poll::jsonPayload() const {
    nlohmann::json j = {
        { "identifier", _identifier },
        { "inputMode", "poll" },
        { "options", _options },
        { "description", _description },
        { "allowChangingVote", _allowChangeVote },
        { "nAllowedVotes", _allowMultipleVoting.value_or(1) }
    };
    return j;
}

void Poll::handleMessage(const nlohmann::json& json) {
    auto voteJson = json.find(PollMessageKeyVote);
    auto userJson = json.find(details::MessageKeyUser);

    const bool isValidJsonObject =
        voteJson != json.end() &&
        voteJson->is_string() &&
        userJson != json.end() &&
        userJson->is_string();

    if (!isValidJsonObject) {
        LWARNING(fmt::format("{}: Ignored user vote, missing key '{}' or '{}'",
            identifier(), PollMessageKeyVote, details::MessageKeyUser
        ));
        return;
    }

    const std::string newVote = voteJson->get<std::string>();
    const User user = omni::details::convertToUserID(*userJson);

    // User may have casted a vote previously, so we look through all vote options
    // if the user exists in any set
    for (auto& [vote, users] : _votes) {
        auto it = users.find(user);

        if (it != users.end()) {
            if (!_allowChangeVote) {
                // User already voted on something ignore new vote
                return;
            }

            // User can only exist in one vote option at a time
            users.erase(it);
            break;
        }
    }

    // add user to new vote set
    _votes[newVote].insert(user);

    // Get the option object user voted for
    auto optionIt = std::find_if(_options.begin(), _options.end(),
        [&newVote](const Option& option) {
            return option.identifier == newVote;
        }
    );

    // Execute corresponding script user voted for
    global::scriptEngine->queueScript(
        optionIt->script,
        openspace::scripting::ScriptEngine::ShouldBeSynchronized::Yes,
        openspace::scripting::ScriptEngine::ShouldSendToRemote::Yes
    );

    // Send back information to client/guests
    onHandleMessage();
}

void Poll::onHandleMessage() {
    // Fetch the current poll result and send it to server
    ghoul::Dictionary result;

    for (const auto& [voteId, users] : _votes) {
        LDEBUG(fmt::format("Vote: {} has {} number of votes", voteId, users.size()));
        result.setValue(voteId, static_cast<int>(users.size()));
    }
    ghoul::Dictionary dictionary;
    dictionary.setValue("result", result);

    sendJson(wrappedPayload(PollMessageKeyPollResult, dictionary));
}

} // namespace openspace::omni
