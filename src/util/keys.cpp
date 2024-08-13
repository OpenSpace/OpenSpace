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

#include <openspace/util/keys.h>

#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/stringhelper.h>
#include <algorithm>
#include <vector>

namespace openspace {

KeyWithModifier stringToKey(const std::string& str) {
    std::vector<std::string> tokens = ghoul::tokenizeString(str, '+');
    // "Keypad +" will tokenize into "Keypad " + ""
    if (tokens.size() == 2 && tokens[0] == "Keypad " && tokens[1].empty()) {
        tokens = { std::string("Keypad +") };
    }

    std::vector<std::string> originalTokens = tokens;
    for (std::string& t : tokens) {
        t = ghoul::toUpperCase(t);
    }

    // default is unknown
    Key key = Key::Unknown;
    std::string keyName = tokens.back();
    const std::string keyNameOriginal = originalTokens.back();
    for (const KeyInfo& ki : KeyInfos) {
        if (ki.identifier == keyName || ki.name == keyName ||
            ki.identifier == keyNameOriginal || ki.name == keyNameOriginal)
        {
            key = ki.key;
            break;
        }
    }
    if (key == Key::Unknown) {
        throw ghoul::RuntimeError(std::format("Could not find key for '{}'", keyName));
    }

    KeyModifier m = KeyModifier::None;

    std::for_each(
        tokens.begin(),
        tokens.end() - 1,
        [&m](const std::string& s) {
            bool found = false;
            for (const KeyModifierInfo& kmi : KeyModifierInfos) {
                if (kmi.identifier == s || kmi.name == s) {
                    m |= kmi.modifier;
                    found = true;
                    break;
                }
            }
            if (!found) {
                throw ghoul::RuntimeError(std::format("Unknown modifier key '{}'", s));
            }
        }
    );

    return { key, m };
}

// Returns the 'identifier' of the key (compared to the ghoul::to_string which returns the
// 'name' of the key
std::string keyToString(KeyWithModifier keyWithModifier) {
    using namespace openspace;

    std::string modifier;
    if (keyWithModifier.modifier != KeyModifier::None) {
        for (const openspace::KeyModifierInfo& kmi : openspace::KeyModifierInfos) {
            // No need for an extra check for the empty modifier since that is mapped
            // to 0, meaning that the `hasKeyModifier` will always fail for it since it
            // checks internally against != 0

            if (hasKeyModifier(keyWithModifier.modifier, kmi.modifier)) {
                modifier += std::format("{}+", kmi.identifier);
            }
        }
    }

    std::string key;
    for (const openspace::KeyInfo& ki : openspace::KeyInfos) {
        if (ki.key == keyWithModifier.key) {
            key = std::string(ki.identifier);
            break;
        }
    }

    // The modifier has a residual + at the end that we use here
    return modifier + key;
}

} // namespace openspace

namespace ghoul {

template <>
std::string to_string(const openspace::Key& value) {
    for (const openspace::KeyInfo& ki : openspace::KeyInfos) {
        if (ki.key == value) {
            return std::string(ki.name);
        }
    }

    throw ghoul::MissingCaseException();
}

template <>
std::string to_string(const openspace::KeyModifier& value) {
    using namespace openspace;

    if (value == KeyModifier::None) {
        return "";
    }

    std::string result;
    for (const KeyModifierInfo& kmi : KeyModifierInfos) {
        // No need for an extra check for the empty modifier since that is mapped to 0,
        // meaning that the `hasKeyModifier` will always fail for it since it checks
        // internally against != 0

        if (hasKeyModifier(value, kmi.modifier)) {
            result += std::format("{}+", kmi.name);
        }

    }
    // The last addition has added an additional '+' that we
    // should remove
    result.pop_back();
    return result;
}

template <>
std::string to_string(const openspace::KeyWithModifier& value) {
    if (value.modifier == openspace::KeyModifier::None) {
        return to_string(value.key);
    }
    else {
        return std::format("{}+{}", to_string(value.modifier), to_string(value.key));
    }
}

} // namespace ghoul
