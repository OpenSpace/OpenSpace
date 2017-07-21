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

#include <openspace/properties/optionproperty.h>

namespace {
    const char* _loggerCat = "OptionProperty";
} // namespace

namespace openspace::properties {
    
const std::string OptionProperty::OptionsKey = "Options";

OptionProperty::OptionProperty(std::string identifier, std::string guiName,
                               std::string desc, Property::Visibility visibility)
    : IntProperty(std::move(identifier), std::move(guiName), std::move(desc), visibility)
    , _displayType(DisplayType::Radio)
{}

OptionProperty::OptionProperty(std::string identifier, std::string guiName,
                               std::string desc, DisplayType displayType,
                               Property::Visibility visibility)
    : IntProperty(std::move(identifier), std::move(guiName), std::move(desc), visibility)
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
            LWARNING("The value of option {" << o.value << " -> " << o.description <<
                "} was already registered when trying to add option {" << option.value <<
                " -> " << option.description << "}");
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

void OptionProperty::setValue(int value) {
    // Check if the passed value belongs to any option
    for (const Option& o : _options) {
        if (o.value == value) {
            // If it does, set it by calling the superclasses setValue method
            NumericalProperty::setValue(value);
            return;
        }
    }

    // Otherwise, log an error
    LERROR("Could not find an option for value '" << value << "' in OptionProperty");
}

std::string OptionProperty::getDescriptionByValue(int value) {
    for (const Option& option : _options) {
        if (option.value == value) {
            return option.description;
        }
    }
    return "";
}

std::string OptionProperty::generateAdditionalDescription() const {
    // @REFACTOR from selectionproperty.cpp, possible refactoring? ---abock
    std::string result;
    result += OptionsKey + " = {";
    for (size_t i = 0; i < _options.size(); ++i) {
        const Option& o = _options[i];
        result += "[\"" + std::to_string(o.value) + "\"] = \"" + o.description + "\"";
        if (i != _options.size() - 1)
            result += ",";
    }

    result += "}";
    return result;
}

} // namespace openspace::properties
