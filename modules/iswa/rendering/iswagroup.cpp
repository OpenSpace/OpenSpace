/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2015                                                               *
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
#include <modules/iswa/rendering/iswagroup.h>
#include <modules/iswa/rendering/dataplane.h>

namespace {
    const std::string _loggerCat = "IswaGroup";
}

namespace openspace {

IswaGroup::IswaGroup(std::string name, IswaManager::CygnetType type)
    :_enabled("enabled", "Enabled", true)
    ,_useLog("useLog","Use Logarithm", false)
    ,_useHistogram("_useHistogram", "Use Histogram", false)
    ,_normValues("normValues", "Normalize Values", glm::vec2(1.0,1.0), glm::vec2(0), glm::vec2(5.0))
    ,_backgroundValues("backgroundValues", "Background Values", glm::vec2(0.0), glm::vec2(0), glm::vec2(1.0))
    ,_transferFunctionsFile("transferfunctions", "Transfer Functions", "${SCENE}/iswa/tfs/hot.tf")
    ,_delete("delete", "Delete")
    ,_dataOptions("dataOptions", "Data Options")
    // ,_id(id)
    ,_type(type)
    ,_registered(false)
    // ,_dataProcessor(nullptr)
{
    setName(name);

    addProperty(_enabled);

    addProperty(_useLog);
    addProperty(_useHistogram);
    addProperty(_normValues);
    addProperty(_backgroundValues);
    addProperty(_transferFunctionsFile);
    addProperty(_dataOptions);

    addProperty(_delete);

    _dataProcessor = std::make_shared<DataProcessor>(
            _useLog.value(),
            _useHistogram.value(),
            _normValues
    );
    _groupEvent = std::make_shared<ghoul::Event<ghoul::Dictionary> >();
    registerProperties();
}

IswaGroup::~IswaGroup(){
    //_cygnets.clear();
}

// void IswaGroup::registerCygnet(IswaCygnet* cygnet, IswaManager::CygnetType type){
//     if(_cygnets.empty()){
//         _type = type;
//         registerProperties();
//     }

//     if(type != _type){
//         LWARNING("Can't register cygnet with a different type from the group");
//         return;
//     }

//     if(type == IswaManager::CygnetType::Data){
//         DataPlane* dataplane = static_cast<DataPlane*>(cygnet);
        
//         dataplane->useLog(_useLog.value());
//         dataplane->useHistogram(_useHistogram.value());
//         dataplane->normValues(_normValues.value());
//         dataplane->backgroundValues(_backgroundValues.value());
//         dataplane->transferFunctionsFile(_transferFunctionsFile.value());
//         dataplane->dataOptions(_dataOptions.value());
//     }
//     _cygnets.push_back(cygnet);
// }

void IswaGroup::registerOptions(const std::vector<properties::SelectionProperty::Option>& options){
    if(!_registered)
        registerProperties();

    if(_type == IswaManager::CygnetType::Data){
        if(_dataOptions.options().empty()){
            for(auto option : options){

                std::stringstream memorystream(option.description);
                std::string optionName;
                getline(memorystream, optionName, '/');
                getline(memorystream, optionName, '/');

                _dataOptions.addOption({option.value, name()+"/"+optionName});
            }
            _dataOptions.setValue(std::vector<int>(1,0));
        }
    }
}

bool IswaGroup::isType(IswaManager::CygnetType type){
    if(_type == IswaManager::CygnetType::NoType) return true;
    return (_type == type);
}

void IswaGroup::registerProperties(){
    OsEng.gui()._iswa.registerProperty(&_enabled);

    _enabled.onChange([this]{
        LDEBUG("Group " + name() + " published enabledChanged");
        _groupEvent->publish("enabledChanged", ghoul::Dictionary({{"enabled", _enabled.value()}}));
    });


    if(_type == IswaManager::CygnetType::Data){
        OsEng.gui()._iswa.registerProperty(&_useLog);
        OsEng.gui()._iswa.registerProperty(&_useHistogram);
        OsEng.gui()._iswa.registerProperty(&_normValues);
        OsEng.gui()._iswa.registerProperty(&_backgroundValues);
        OsEng.gui()._iswa.registerProperty(&_transferFunctionsFile);
        OsEng.gui()._iswa.registerProperty(&_dataOptions);

        _useLog.onChange([this]{
            LDEBUG("Group " + name() + " published useLogChanged");
            _groupEvent->publish("useLogChanged", ghoul::Dictionary({{"useLog", _useLog.value()}}));
        });

        _useHistogram.onChange([this]{
            LDEBUG("Group " + name() + " published useHistogramChanged");
            _groupEvent->publish("useHistogramChanged", ghoul::Dictionary({{"useHistogram", _useHistogram.value()}}));
        });

        _normValues.onChange([this]{
            LDEBUG("Group " + name() + " published normValuesChanged");
            _groupEvent->publish("normValuesChanged", ghoul::Dictionary({{"normValues", std::make_shared<glm::vec2>(_normValues.value())}}));
        });

        _backgroundValues.onChange([this]{
            LDEBUG("Group " + name() + " published backgroundValuesChanged");
            _groupEvent->publish("backgroundValuesChanged", ghoul::Dictionary({{"backgroundValues", std::make_shared<glm::vec2>(_backgroundValues.value())}}));
        });

        _transferFunctionsFile.onChange([this]{
            LDEBUG("Group " + name() + " published transferFunctionsChanged");
            _groupEvent->publish("transferFunctionsChanged", ghoul::Dictionary({{"transferFunctions", _transferFunctionsFile.value()}}));
        });


        _dataOptions.onChange([this]{
            LDEBUG("Group " + name() + " published dataOptionsChanged");
            _groupEvent->publish("dataOptionsChanged", ghoul::Dictionary({{"dataOptions", std::make_shared<std::vector<int> >(_dataOptions.value())}}));
        });
    }

    OsEng.gui()._iswa.registerProperty(&_delete);
    _delete.onChange([this]{
        clearGroup();
    }); 
    _registered = true;  
}

void IswaGroup::unregisterProperties(){
    // _dataOptions.removeOptions();
    OsEng.gui()._iswa.unregisterProperties(name());
    _registered = false;
    // _type = IswaManager::CygnetType::NoType;
}

void IswaGroup::clearGroup(){
    _groupEvent->publish("clearGroup", ghoul::Dictionary());
    unregisterProperties();
}

std::shared_ptr<DataProcessor> IswaGroup::dataProcessor(){
    return _dataProcessor;
}

} //namespace openspace