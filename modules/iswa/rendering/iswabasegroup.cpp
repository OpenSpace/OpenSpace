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
#include <modules/iswa/rendering/iswabasegroup.h>

#include <fstream>
#include <modules/iswa/ext/json/json.hpp>

#include <modules/iswa/util/dataprocessortext.h>
#include <modules/iswa/util/dataprocessorjson.h>
#include <modules/iswa/util/dataprocessorkameleon.h>

#include <modules/iswa/rendering/dataplane.h>
#include <modules/iswa/rendering/datasphere.h>
#include <modules/iswa/rendering/kameleonplane.h>

namespace {
    const std::string _loggerCat = "IswaBaseGroup";
}

namespace openspace {
IswaBaseGroup::IswaBaseGroup(std::string name, IswaManager::CygnetType type)
    :_enabled("01enabled", "Enabled", true)
    ,_delete("02delete", "Delete")
    ,_alpha("03alpha", "Alpha", 0.9f, 0.0f, 1.0f)
    ,_type(type)
    ,_dataProcessor(nullptr)
{
    setName(name);

    addProperty(_enabled);
    addProperty(_alpha);
    addProperty(_delete);

    _groupEvent = std::make_shared<ghoul::Event<ghoul::Dictionary> >();
    registerProperties();
}

IswaBaseGroup::~IswaBaseGroup(){}

bool IswaBaseGroup::isType(IswaManager::CygnetType type){
    return (_type == type);
}

void IswaBaseGroup::updateGroup(){
    LDEBUG("Group " + name() + " published updateGroup");
    _groupEvent->publish("updateGroup", ghoul::Dictionary());
    
}

void IswaBaseGroup::clearGroup(){
    _groupEvent->publish("clearGroup", ghoul::Dictionary());
    LDEBUG("Group " + name() + " published clearGroup");
    
    unregisterProperties();
    IswaManager::ref().unregisterGroup(name());
}

std::shared_ptr<DataProcessor> IswaBaseGroup::dataProcessor(){
    return _dataProcessor;
}

std::shared_ptr<ghoul::Event<ghoul::Dictionary> > IswaBaseGroup::groupEvent(){ 
    return _groupEvent; 
};


void IswaBaseGroup::registerProperties(){
    _enabled.onChange([this]{
        LDEBUG("Group " + name() + " published enabledChanged");
        _groupEvent->publish("enabledChanged", ghoul::Dictionary({{"enabled", _enabled.value()}}));
    });

    _alpha.onChange([this]{
        LDEBUG("Group " + name() + " published alphaChanged");
        _groupEvent->publish("alphaChanged", ghoul::Dictionary({{"alpha", _alpha.value()}}));
    });


    _delete.onChange([this]{
        clearGroup();
    });  
}

void IswaBaseGroup::unregisterProperties(){
}

std::unique_ptr<ghoul::Dictionary> IswaBaseGroup::propertyValues() const{
    return std::make_unique<ghoul::Dictionary>(ghoul::Dictionary({
       {"enabled", _enabled.value()},
       {"alpha", _alpha.value()}
    }));
}

} //namespace openspace