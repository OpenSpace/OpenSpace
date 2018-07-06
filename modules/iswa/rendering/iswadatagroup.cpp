/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/iswa/rendering/iswadatagroup.h>

#include <modules/iswa/rendering/dataplane.h>
#include <modules/iswa/rendering/datasphere.h>
#include <modules/iswa/rendering/kameleonplane.h>
#include <modules/iswa/util/dataprocessor.h>
#include <modules/iswa/util/dataprocessortext.h>
#include <modules/iswa/util/dataprocessorjson.h>
#include <modules/iswa/util/dataprocessorkameleon.h>
#include <openspace/json.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "IswaDataGroup";
    using json = nlohmann::json;

    constexpr openspace::properties::Property::PropertyInfo UseLogInfo = {
        "UseLog",
        "Use Logarithm",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo UseHistogramInfo = {
        "UseHistogram",
        "Auto Contrast",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo AutoFilterInfo = {
        "AutoFilter",
        "Auto Filter",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo NormalizeValues = {
        "NormValues",
        "Normalize Values",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo BackgroundInfo = {
        "BackgroundValues",
        "Background Values",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo TransferFunctionInfo = {
        "Transferfunctions",
        "Transfer Functions",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo DataOptionsInfo = {
        "DataOptions",
        "Data Options",
        "" // @TODO Missing documentation
    };

} // namespace

namespace openspace{
IswaDataGroup::IswaDataGroup(std::string name, std::string type)
    : IswaBaseGroup(name, type)
    , _useLog(UseLogInfo, false)
    , _useHistogram(UseHistogramInfo, false)
    , _autoFilter(AutoFilterInfo, true)
    , _normValues(NormalizeValues, glm::vec2(1.f), glm::vec2(0.f), glm::vec2(5.f))
    , _backgroundValues(BackgroundInfo, glm::vec2(0.f), glm::vec2(0.f), glm::vec2(1.f))
    , _transferFunctionsFile(TransferFunctionInfo, "${SCENE}/iswa/tfs/default.tf")
    , _dataOptions(DataOptionsInfo)
{
    addProperty(_useLog);
    addProperty(_useHistogram);
    addProperty(_autoFilter);
    addProperty(_normValues);
    addProperty(_backgroundValues);
    addProperty(_transferFunctionsFile);
    addProperty(_dataOptions);

    createDataProcessor();
    registerProperties();
}

IswaDataGroup::~IswaDataGroup() {}

void IswaDataGroup::registerProperties() {
    _useLog.onChange([this]() {
        LDEBUG("Group " + identifier() + " published useLogChanged");
        _groupEvent.publish(
            "useLogChanged",
            ghoul::Dictionary({ { "useLog", _useLog.value() } })
        );
    });

    _useHistogram.onChange([this]() {
        LDEBUG("Group " + identifier() + " published useHistogramChanged");
        _groupEvent.publish(
            "useHistogramChanged",
            ghoul::Dictionary({ { "useHistogram", _useHistogram.value() } })
        );
    });

    //If autofiler is on, background values property should be hidden
    _autoFilter.onChange([this]() {
        LDEBUG("Group " + identifier() + " published autoFilterChanged");
        // If autofiler is selected, use _dataProcessor to set backgroundValues
        // and unregister backgroundvalues property.
        if (_autoFilter) {
            _backgroundValues = _dataProcessor->filterValues();
            _backgroundValues.setVisibility(properties::Property::Visibility::Hidden);
            //_backgroundValues.setVisible(false);
        // else if autofilter is turned off, register backgroundValues
        } else {
            _backgroundValues.setVisibility(properties::Property::Visibility::All);
            //_backgroundValues.setVisible(true);
        }
        _groupEvent.publish(
            "autoFilterChanged",
            ghoul::Dictionary({ { "autoFilter", _autoFilter.value() } })
        );
    });

    _normValues.onChange([this]() {
        LDEBUG("Group " + identifier() + " published normValuesChanged");
        _groupEvent.publish(
            "normValuesChanged",
            ghoul::Dictionary({ { "normValues", _normValues.value() } })
        );
    });

    _backgroundValues.onChange([this]() {
        LDEBUG("Group " + identifier() + " published backgroundValuesChanged");
        _groupEvent.publish(
            "backgroundValuesChanged",
            ghoul::Dictionary({ { "backgroundValues", _backgroundValues.value() } })
        );
    });

    _transferFunctionsFile.onChange([this]() {
        LDEBUG("Group " + identifier() + " published transferFunctionsChanged");
        _groupEvent.publish(
            "transferFunctionsChanged",
            ghoul::Dictionary({ { "transferFunctions", _transferFunctionsFile.value() } })
        );
    });

    _dataOptions.onChange([this]() {
        LDEBUG("Group " + identifier() + " published dataOptionsChanged");
        ghoul::Dictionary dict;
        dict.setValue<std::vector<int>>("dataOptions", _dataOptions.value());
        _groupEvent.publish("dataOptionsChanged", dict);
    });
}

void IswaDataGroup::registerOptions(
                        const std::vector<properties::SelectionProperty::Option>& options)
{
    if (!_registered) {
        registerProperties();
    }

    if (_dataOptions.options().empty()) {
        for (properties::SelectionProperty::Option option : options) {
            _dataOptions.addOption({ option.value, option.description });
        }
        _dataOptions.setValue(std::vector<int>(1, 0));
    }
}

void IswaDataGroup::createDataProcessor() {
    if (_type == typeid(DataPlane).name()) {
        _dataProcessor = std::make_shared<DataProcessorText>();
    }else if (_type == typeid(DataSphere).name()) {
        _dataProcessor = std::make_shared<DataProcessorJson>();
    }else if (_type == typeid(KameleonPlane).name()) {
        _dataProcessor = std::make_shared<DataProcessorKameleon>();
    }
}

std::vector<int> IswaDataGroup::dataOptionsValue() const {
    return _dataOptions;
}

} //namespace openspace
