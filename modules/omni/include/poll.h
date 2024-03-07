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

#ifndef __OPENSPACE_MODULE_OMNI___POLL___H__
#define __OPENSPACE_MODULE_OMNI___POLL___H__

#include <modules/omni/include/scenario.h>

#include <openspace/json.h>
#include <ghoul/misc/dictionary.h>
#include <map>
#include <set>

namespace openspace::documentation { struct Documentation; }

namespace openspace::omni {

class Poll : public Scenario {
public:
    using User = int;
    using VoteOption = std::string;

    struct Option {
        const std::string identifier;
        const std::string name;
        const std::string script;

        const std::optional<std::filesystem::path> url;
        const std::optional<glm::vec3> color;
    };

    explicit Poll(const ghoul::Dictionary& dictionary);
    virtual ~Poll() = default;

    nlohmann::json jsonPayload() const override;

    void handleMessage(const nlohmann::json& json) override;
    void onHandleMessage() override;

    static documentation::Documentation Documentation();

private:
    std::string _description;
    std::vector<Option> _options;
    std::map<VoteOption, std::set<User>> _votes;

    std::optional<int> _allowMultipleVoting;
    bool _allowChangeVote = false;
};

} // namespace openspace::omni


#endif // __OPENSPACE_MODULE_OMNI___POLL___H__
