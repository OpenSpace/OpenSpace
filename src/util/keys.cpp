/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/util/keys.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/misc.h>

#include <algorithm>
#include <vector>

namespace {
    const char* _loggerCat = "Keys";
} // namespace

namespace openspace {

bool hasKeyModifier(KeyAction lhs, KeyAction rhs) {
    return
        static_cast<std::underlying_type_t<KeyAction>>(lhs) &
        static_cast<std::underlying_type_t<KeyAction>>(rhs);
}

KeyAction operator|(KeyAction lhs, KeyAction rhs) {
    return static_cast<KeyAction>(
        static_cast<std::underlying_type_t<KeyAction>>(lhs) |
        static_cast<std::underlying_type_t<KeyAction>>(rhs)
    );
}

KeyAction operator|=(KeyAction& lhs, KeyAction rhs) {
    lhs = (lhs | rhs);
    return lhs;
}

bool hasKeyModifier(KeyModifier lhs, KeyModifier rhs) {
    return
        static_cast<std::underlying_type_t<KeyModifier>>(lhs) &
        static_cast<std::underlying_type_t<KeyModifier>>(rhs);
}


KeyModifier operator|(KeyModifier lhs, KeyModifier rhs) {
    return static_cast<KeyModifier>(
        static_cast<std::underlying_type_t<KeyModifier>>(lhs) |
        static_cast<std::underlying_type_t<KeyModifier>>(rhs)
    );
}

KeyModifier operator|=(KeyModifier& lhs, KeyModifier rhs) {
    lhs = (lhs | rhs);
    return lhs;
}

KeyWithModifier stringToKey(std::string str) {
    // key only uppercase
    std::transform(
        str.begin(),
        str.end(),
        str.begin(),
        [](char v) { return static_cast<char>(toupper(v)); }
    );

    std::vector<std::string> tokens = ghoul::tokenizeString(str, '+');

    // default is unknown
    Key k = Key::Unknown;
    auto it = KeyMapping.find(tokens.back());
    if (it != KeyMapping.end()) {
        k = it->second;
    }


    KeyModifier m = KeyModifier::NoModifier;
    std::for_each(
        tokens.begin(),
        tokens.end() - 1,
        [&m](const std::string& s) {
            auto it = KeyModifierMapping.find(s);
            if (it != KeyModifierMapping.end()) {
                m |= it->second;
            }
            else {
                LERROR("Unknown modifier key '" << s << "'");
            }
        }
    );

    return { k, m };
}

bool operator<(const KeyWithModifier& lhs, const KeyWithModifier& rhs) {
    if (lhs.modifier == rhs.modifier) {
        return lhs.key < rhs.key;
    }
    else {
        return lhs.modifier < rhs.modifier;
    }
}

} // namespace openspace

namespace std {
 
std::string to_string(openspace::Key key) {
    for (const auto& p : openspace::KeyMapping) {
        if (p.second == key) {
            return p.first;
        }
    }
    throw ghoul::MissingCaseException();
}

std::string to_string(openspace::KeyModifier mod) {
    using namespace openspace;
    
    if (mod == KeyModifier::NoModifier) {
        return "";
    }
    
    std::string result;
    for (const auto& p : KeyModifierMapping) {
        if (hasKeyModifier(mod, p.second)) {
            result += p.first + "+";
        }
    }
    // The last addition has added an additional '+' that we
    // should remove
    return result.substr(0, result.size() - 1);
}

std::string to_string(openspace::KeyWithModifier key) {
    if (key.modifier == openspace::KeyModifier::NoModifier) {
        return to_string(key.key);
    }
    else {
        return to_string(key.modifier) + "+" + to_string(key.key);
    }
}
    
} // namespace std
