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

#include <modules/iswa/rendering/iswadatagroup.h>

#include <fstream>
#include <modules/iswa/ext/json/json.hpp>

#include <modules/iswa/util/dataprocessortext.h>
#include <modules/iswa/util/dataprocessorjson.h>
#include <modules/iswa/util/dataprocessorkameleon.h>

#include <modules/iswa/rendering/dataplane.h>
#include <modules/iswa/rendering/datasphere.h>
#include <modules/iswa/rendering/kameleonplane.h>

namespace {
    const char* _loggerCat = "IswaDataGroup";
    using json = nlohmann::json;
} // namespace

namespace openspace{
IswaDataGroup::IswaDataGroup(std::string name, std::string type)
    : IswaBaseGroup(name, type)    
    , _useLog("useLog","Use Logarithm", false)
    , _useHistogram("useHistogram", "Auto Contrast", false)
    , _autoFilter("autoFilter", "Auto Filter", true)
    , _normValues("normValues", "Normalize Values", glm::vec2(1.0,1.0), glm::vec2(0), glm::vec2(5.0))
    , _backgroundValues("backgroundValues", "Background Values", glm::vec2(0.0), glm::vec2(0), glm::vec2(1.0))
    , _transferFunctionsFile("transferfunctions", "Transfer Functions", "${SCENE}/iswa/tfs/default.tf")
    , _dataOptions("dataOptions", "Data Options")
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
 
IswaDataGroup::~IswaDataGroup(){}

void IswaDataGroup::registerProperties(){
    //OsEng.gui()._iswa.registerProperty(&_useLog);
    //OsEng.gui()._iswa.registerProperty(&_useHistogram);
    //OsEng.gui()._iswa.registerProperty(&_autoFilter);
    //if(!_autoFilter.value())
    //    OsEng.gui()._iswa.registerProperty(&_backgroundValues);
    //// OsEng.gui()._iswa.registerProperty(&_autoFilter);
    //OsEng.gui()._iswa.registerProperty(&_normValues);
    //OsEng.gui()._iswa.registerProperty(&_transferFunctionsFile);
    //OsEng.gui()._iswa.registerProperty(&_dataOptions);


    _useLog.onChange([this]{
        LDEBUG("Group " + name() + " published useLogChanged");
        _groupEvent->publish("useLogChanged", ghoul::Dictionary({{"useLog", _useLog.value()}}));
    });

    _useHistogram.onChange([this]{
        LDEBUG("Group " + name() + " published useHistogramChanged");
        _groupEvent->publish("useHistogramChanged", ghoul::Dictionary({{"useHistogram", _useHistogram.value()}}));
    });

    //If autofiler is on, background values property should be hidden
    _autoFilter.onChange([this](){
        LDEBUG("Group " + name() + " published autoFilterChanged");
        // If autofiler is selected, use _dataProcessor to set backgroundValues 
        // and unregister backgroundvalues property.
        if(_autoFilter.value()){
            _backgroundValues.setValue(_dataProcessor->filterValues());
            _backgroundValues.setVisibility(properties::Property::Visibility::Hidden);
            //_backgroundValues.setVisible(false);
        // else if autofilter is turned off, register backgroundValues 
        } else {
            _backgroundValues.setVisibility(properties::Property::Visibility::All);
            //_backgroundValues.setVisible(true);
        }
        _groupEvent->publish("autoFilterChanged", ghoul::Dictionary({{"autoFilter", _autoFilter.value()}}));
    });

    _normValues.onChange([this]{
        LDEBUG("Group " + name() + " published normValuesChanged");
        _groupEvent->publish("normValuesChanged", ghoul::Dictionary({{"normValues", _normValues.value()}}));
    });

    _backgroundValues.onChange([this]{
        LDEBUG("Group " + name() + " published backgroundValuesChanged");
        _groupEvent->publish("backgroundValuesChanged", ghoul::Dictionary({{"backgroundValues", _backgroundValues.value()}}));
    });

    _transferFunctionsFile.onChange([this]{
        LDEBUG("Group " + name() + " published transferFunctionsChanged");
        _groupEvent->publish("transferFunctionsChanged", ghoul::Dictionary({{"transferFunctions", _transferFunctionsFile.value()}}));
    });

    _dataOptions.onChange([this]{
        LDEBUG("Group " + name() + " published dataOptionsChanged");
        ghoul::Dictionary dict;
        dict.setValue<std::vector<int>>("dataOptions", _dataOptions.value());
        _groupEvent->publish("dataOptionsChanged", dict);
    });
}

void IswaDataGroup::registerOptions(const std::vector<properties::SelectionProperty::Option>& options){
    if(!_registered)
        registerProperties();

    if(_dataOptions.options().empty()){
        for(auto option : options){
            _dataOptions.addOption({option.value, option.description});
        }
        _dataOptions.setValue(std::vector<int>(1,0));
    }
}

void IswaDataGroup::createDataProcessor(){
    if(_type == typeid(DataPlane).name()){
        _dataProcessor = std::make_shared<DataProcessorText>();
    }else if(_type == typeid(DataSphere).name()){
        _dataProcessor = std::make_shared<DataProcessorJson>();
    }else if(_type == typeid(KameleonPlane).name()){
        _dataProcessor = std::make_shared<DataProcessorKameleon>();
    }
}

std::vector<int> IswaDataGroup::dataOptionsValue(){
    return _dataOptions.value();
}

} //namespace openspace
