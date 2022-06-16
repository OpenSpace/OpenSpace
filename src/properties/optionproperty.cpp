/*****************************************************************************************

 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <openspace/properties/optionproperty.h>

#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "OptionProperty";
} // namespace

namespace openspace::properties {

const std::string OptionProperty::OptionsKey = "Options";

OptionProperty::OptionProperty(PropertyInfo info)
    : IntProperty(std::move(info))
    , _displayType(DisplayType::Radio)
{}

OptionProperty::OptionProperty(PropertyInfo info, DisplayType displayType)
    : IntProperty(std::move(info))
    , _displayType(displayType)
{}

std::string OptionProperty::className() const {
    return "OptionProperty";
}

OptionProperty::DisplayType OptionProperty::displayType() const {
    return _displayType;
}

const std::vector<OptionProperty::Option>& OptionProperty::options() const {
    return _options;
}

void OptionProperty::addOption(int value, std::string desc) {
    Option option = { std::move(value), std::move(desc) };

    for (const Option& o : _options) {
        if (o.value == option.value) {
            LWARNING(fmt::format(
                "The value of option {{ {} -> {} }} was already registered when trying "
                "to add option {{ {} -> {} }}",
                o.value, o.description, option.value, option.description

            ));
            return;
        }
    }
    _options.push_back(std::move(option));
}

void OptionProperty::addOptions(std::vector<std::pair<int, std::string>> options) {
    for (std::pair<int, std::string>& p : options) {
        addOption(std::move(p.first), std::move(p.second));
    }
}

void OptionProperty::addOptions(std::vector<std::string> options) {
    for (int i = 0; i < static_cast<int>(options.size()); ++i) {
        addOption(i, std::move(options[i]));
    }
}

void OptionProperty::clearOptions() {
    _options.clear();
}

void OptionProperty::setValue(int value) {
    // Check if the passed value belongs to any option
    for (Option& o : _options) {
        if (o.value == value) {
            NumericalProperty::setValue(value);
            return;
        }
    }

    // Otherwise, log an error
    LERROR(fmt::format("Could not find an option for value '{}'", value));
}

bool OptionProperty::hasOption() const {
    return _options.size() != 0;
}


const OptionProperty::Option& OptionProperty::option() const {
    auto it = std::find_if(
        _options.begin(),
        _options.end(),
        [val = value()](const Option& option) {
            return option.value == val;
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

std::string OptionProperty::generateAdditionalJsonDescription() const {
    // @REFACTOR from selectionproperty.cpp, possible refactoring? ---abock
    std::string result =
        "{ \"" + OptionsKey + "\": [";
    for (size_t i = 0; i < _options.size(); ++i) {
        const Option& o = _options[i];
        std::string v = std::to_string(o.value);
        std::string vSan = sanitizeString(v);
        std::string d = o.description;
        std::string dSan = sanitizeString(d);

        result += '{';
        result += fmt::format(R"("{}": "{}")", vSan, dSan);
        result += '}';

        if (i != _options.size() - 1) {
            result += ",";
        }
    }

    result += "] }";
    return result;
}

} // namespace openspace::properties
