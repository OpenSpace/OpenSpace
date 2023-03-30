/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/misc.h>
#include <algorithm>
#include <vector>

namespace openspace {

KeyWithModifier stringToKey(std::string str) {
    std::vector<std::string> tokens = ghoul::tokenizeString(str, '+');
    // "Keypad +" will tokenize into "Keypad " + ""
    if (tokens.size() == 2 && tokens[0] == "Keypad " && tokens[1].empty()) {
        tokens = { std::string("Keypad +") };
    }

    std::vector<std::string> originalTokens = tokens;
    for (std::string& t : tokens) {
        std::transform(
            t.begin(), t.end(),
            t.begin(),
            [](char v) { return static_cast<char>(toupper(v)); }
        );

    }

    // default is unknown
    Key key = Key::Unknown;
    std::string keyName = tokens.back();
    std::string keyNameOriginal = originalTokens.back();
    for (const KeyInfo& ki : KeyInfos) {
        if (ki.identifier == keyName || ki.name == keyName ||
            ki.identifier == keyNameOriginal || ki.name == keyNameOriginal)
        {
            key = ki.key;
            break;
        }
    }
    if (key == Key::Unknown) {
        throw ghoul::RuntimeError(fmt::format("Could not find key for '{}'", keyName));
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
                throw ghoul::RuntimeError(fmt::format("Unknown modifier key '{}'", s));
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
                modifier += fmt::format("{}+", kmi.identifier);
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
std::string to_string(const openspace::Key& key) {
    for (const openspace::KeyInfo& ki : openspace::KeyInfos) {
        if (ki.key == key) {
            return std::string(ki.name);
        }
    }

    throw ghoul::MissingCaseException();
}

template <>
std::string to_string(const openspace::KeyModifier& mod) {
    using namespace openspace;

    if (mod == KeyModifier::None) {
        return "";
    }

    std::string result;
    for (const openspace::KeyModifierInfo& kmi : openspace::KeyModifierInfos) {
        // No need for an extra check for the empty modifier since that is mapped to 0,
        // meaning that the `hasKeyModifier` will always fail for it since it checks
        // internally against != 0

        if (hasKeyModifier(mod, kmi.modifier)) {
            result += fmt::format("{}+", kmi.name);
        }

    }
    // The last addition has added an additional '+' that we
    // should remove
    result.pop_back();
    return result;
}

template <>
std::string to_string(const openspace::KeyWithModifier& key) {
    if (key.modifier == openspace::KeyModifier::None) {
        return to_string(key.key);
    }
    else {
        return fmt::format("{}+{}", to_string(key.modifier), to_string(key.key));
    }
}

} // namespace ghoul
