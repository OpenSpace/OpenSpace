/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <openspace/properties/misc/optionproperty.h>

#include <openspace/util/json_helper.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/lua_helper.h>
#include <limits>

namespace {
    constexpr std::string_view _loggerCat = "OptionProperty";

    constexpr std::string_view OptionsKey = "options";

    using Option = openspace::properties::OptionProperty::Option;

    bool addOptionInternal(int value, std::string desc, std::vector<Option>& options) {
        Option option = { .value = value, .description = std::move(desc) };

        for (const Option& o : options) {
            if (o.value == option.value) {
                LWARNING(std::format(
                    "The value of option {{ {} -> {} }} was already registered when "
                    "trying to add option {{ {} -> {} }}",
                    o.value, o.description, option.value, option.description

                ));
                return false;
            }
        }
        options.push_back(std::move(option));
        return true;
    }
} // namespace

namespace openspace::properties {


OptionProperty::OptionProperty(PropertyInfo info)
    : NumericalProperty<int>(
        std::move(info),
        0,
        std::numeric_limits<int>::lowest(),
        std::numeric_limits<int>::max(),
        1
    )
{}

std::string_view OptionProperty::className() const {
    return "OptionProperty";
}

ghoul::lua::LuaTypes OptionProperty::typeLua() const {
    return ghoul::lua::LuaTypes(
        ghoul::lua::LuaTypes::Number | ghoul::lua::LuaTypes::String
    );
}

const std::vector<OptionProperty::Option>& OptionProperty::options() const {
    return _options;
}

void OptionProperty::addOption(int value, std::string description) {
    bool success = addOptionInternal(value, description, _options);
    if (success) {
        // Set default value to option added first
        NumericalProperty::setValue(_options[0].value);
    }
    notifyMetaDataChangeListeners();
}

void OptionProperty::addOptions(std::vector<std::pair<int, std::string>> options) {
    for (std::pair<int, std::string>& p : options) {
        addOptionInternal(p.first, std::move(p.second), _options);
    }
    // Set default value to option added first
    NumericalProperty::setValue(_options[0].value);
    notifyMetaDataChangeListeners();
}

void OptionProperty::addOptions(std::vector<std::string> options) {
    for (int i = 0; i < static_cast<int>(options.size()); i++) {
        addOptionInternal(i, std::move(options[i]), _options);
    }
    // Set default value to option added first
    NumericalProperty::setValue(_options[0].value);
    notifyMetaDataChangeListeners();
}

void OptionProperty::clearOptions() {
    NumericalProperty::setValue(0);
    _options.clear();
    notifyMetaDataChangeListeners();
}

void OptionProperty::setValue(int value) {
    // Check if the passed value belongs to any option
    for (const Option& option : _options) {
        if (option.value == value) {
            // If it does, set it by calling the superclasses setValue method
            // @TODO(abock): This should be setValue(value) instead or otherwise the
            //               stored indices and option values start to drift if the
            //               operator T of the OptionProperty is used
            NumericalProperty::setValue(static_cast<int>(value));
            return;
        }
    }

    // Otherwise, log an error
    LERROR(std::format("Could not find an option for value '{}'", value));
}

bool OptionProperty::hasOption() const {
    auto it = std::find_if(
        _options.begin(),
        _options.end(),
        [setValue = value()](const Option& option) {
            return option.value == setValue;
        }
    );
    return it !=_options.end();
}


const OptionProperty::Option& OptionProperty::option() const {
    auto it = std::find_if(
        _options.begin(),
        _options.end(),
        [setValue = value()](const Option& option) {
            return option.value == setValue;
        }
    );
    return *it;
}

std::string OptionProperty::getDescriptionByValue(int value) {
    auto it = std::find_if(
        _options.begin(),
        _options.end(),
        [value](const Option& option) {
            return option.value == value;
        }
    );

    if (it != _options.end()) {
        return it->description;
    }
    else {
        return "";
    }
}

void OptionProperty::getLuaValue(lua_State* state) const {
    ghoul::lua::push(state, _value);
}

void OptionProperty::setLuaValue(lua_State* state) {
    int thisValue = 0;

    if (ghoul::lua::hasValue<double>(state)) {
        thisValue = static_cast<int>(ghoul::lua::value<double>(state));
    }
    else if (ghoul::lua::hasValue<int>(state)) {
        thisValue = ghoul::lua::value<int>(state);
    }
    else if (ghoul::lua::hasValue<std::string>(state)) {
        std::string value = ghoul::lua::value<std::string>(state);
        const auto it = std::find_if(
            _options.cbegin(),
            _options.cend(),
            [&value](const Option& option) { return option.description == value; }
        );
        if (it == _options.cend()) {
            throw ghoul::RuntimeError(
                std::format("Could not find option '{}'", value),
                std::string(uri())
            );
        }
        thisValue = it->value;
    }
    else {
        throw ghoul::RuntimeError("Error extracting value in OptionProperty");
    }

    setValue(thisValue);
}

std::string OptionProperty::stringValue() const {
    return formatJson(_value);
}


nlohmann::json OptionProperty::generateAdditionalJsonDescription() const {
    nlohmann::json data;

    for (const Option& option : _options) {
        data[std::to_string(option.value)] = option.description;
    }

    return { { OptionsKey, data } };
}

int OptionProperty::toValue(lua_State* state) const {
    if (ghoul::lua::hasValue<double>(state)) {
        return static_cast<int>(ghoul::lua::value<double>(state));
    }
    else if (ghoul::lua::hasValue<int>(state)) {
        return ghoul::lua::value<int>(state);
    }
    else {
        throw ghoul::RuntimeError("Error extracting value in IntProperty");
    }
}

} // namespace openspace::properties
