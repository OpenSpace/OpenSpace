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

#include <fstream>
#include <modules/iswa/ext/json/json.hpp>

namespace {
    const std::string _loggerCat = "IswaGroup";
    using json = nlohmann::json;

}

namespace openspace {

IswaGroup::IswaGroup(std::string name, IswaManager::CygnetType type)
    :_enabled("enabled", "Enabled", true)
    ,_alpha("alpha", "Alpha", 0.9f, 0.0f, 1.0f)
    ,_useLog("useLog","Use Logarithm", false)
    ,_useHistogram("useHistogram", "Use Histogram", false)
    ,_autoFilter("autoFilter", "Auto Filter", true)
    ,_normValues("normValues", "Normalize Values", glm::vec2(1.0,1.0), glm::vec2(0), glm::vec2(5.0))
    ,_backgroundValues("backgroundValues", "Background Values", glm::vec2(0.0), glm::vec2(0), glm::vec2(1.0))
    ,_transferFunctionsFile("transferfunctions", "Transfer Functions", "${SCENE}/iswa/tfs/hot.tf")
    ,_delete("delete", "Delete")
    ,_dataOptions("dataOptions", "Data Options")
    ,_fieldlines("fieldlineSeedsIndexFile", "Fieldline Seedpoints")
    ,_fieldlineIndexFile("")
    // ,_id(id)
    ,_type(type)
    ,_registered(false)
    // ,_dataProcessor(nullptr)
{
    setName(name);

    addProperty(_enabled);
    addProperty(_alpha);

    addProperty(_useLog);
    addProperty(_useHistogram);
    addProperty(_autoFilter);
    addProperty(_normValues);
    addProperty(_backgroundValues);
    addProperty(_transferFunctionsFile);
    addProperty(_dataOptions);
    addProperty(_fieldlines);

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
    if(!_registered){
        registerProperties();    }
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

// void IswaGroup::registerFieldLineOptions(const std::vector<properties::SelectionProperty::Option>& options){
//     // std::cout << "Register fieldlines" << std::endl; 
//     if(_type == IswaManager::CygnetType::Data){
//         if(_fieldlines.options().empty()){
//             for(auto option : options){

//                 std::stringstream memorystream(option.description);
//                 std::string optionName;
//                 getline(memorystream, optionName, '/');
//                 getline(memorystream, optionName, '/');

//                 _fieldlines.addOption({option.value, name()+"/"+optionName});
//             }
//         }
//     }
// }

void IswaGroup::setFieldlineInfo(std::string fieldlineIndexFile, std::string kameleonPath){

    if(fieldlineIndexFile != _fieldlineIndexFile){
        _fieldlineIndexFile = fieldlineIndexFile;
        readFieldlinePaths(_fieldlineIndexFile);
    }

    if(kameleonPath != _kameleonPath){
        _kameleonPath = kameleonPath;
        std::vector<int> fieldLinesValue = _fieldlines.value();
        _fieldlines.setValue(std::vector<int>()); //clear the existing fieldlines
        _fieldlines.setValue(fieldLinesValue);    //create fieldlines from the same seed points but with different cdf file
    }
}

bool IswaGroup::isType(IswaManager::CygnetType type){
    if(_type == IswaManager::CygnetType::NoType) return true;
    return (_type == type);
}

void IswaGroup::registerProperties(){
    OsEng.gui()._iswa.registerProperty(&_enabled);
    OsEng.gui()._iswa.registerProperty(&_alpha);

    _enabled.onChange([this]{
        LDEBUG("Group " + name() + " published enabledChanged");
        _groupEvent->publish("enabledChanged", ghoul::Dictionary({{"enabled", _enabled.value()}}));
    });

    _alpha.onChange([this]{
        LDEBUG("Group " + name() + " published alphaChanged");
        _groupEvent->publish("alphaChanged", ghoul::Dictionary({{"alpha", _alpha.value()}}));
    });


    if(_type == IswaManager::CygnetType::Data){
        OsEng.gui()._iswa.registerProperty(&_useLog);
        OsEng.gui()._iswa.registerProperty(&_useHistogram);
        OsEng.gui()._iswa.registerProperty(&_autoFilter);
        OsEng.gui()._iswa.registerProperty(&_normValues);
        OsEng.gui()._iswa.registerProperty(&_backgroundValues);
        OsEng.gui()._iswa.registerProperty(&_transferFunctionsFile);
        OsEng.gui()._iswa.registerProperty(&_fieldlines);
        OsEng.gui()._iswa.registerProperty(&_dataOptions);

        _useLog.onChange([this]{
            LDEBUG("Group " + name() + " published useLogChanged");
            _groupEvent->publish("useLogChanged", ghoul::Dictionary({{"useLog", _useLog.value()}}));
        });

        _useHistogram.onChange([this]{
            LDEBUG("Group " + name() + " published useHistogramChanged");
            _groupEvent->publish("useHistogramChanged", ghoul::Dictionary({{"useHistogram", _useHistogram.value()}}));
        });

        _autoFilter.onChange([this]{
            LDEBUG("Group " + name() + " published autoFilterChanged");
            _groupEvent->publish("autoFilterChanged", ghoul::Dictionary({{"autoFilter", _autoFilter.value()}}));
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

        _fieldlines.onChange([this]{
            updateFieldlineSeeds();
            // LDEBUG("Group " + name() + " published fieldlinesChanged");
            // _groupEvent->publish("fieldlinesChanged", ghoul::Dictionary({{"fieldlines", std::make_shared<std::vector<int> >(_fieldlines.value())}}));
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

void IswaGroup::updateGroup(){
    _groupEvent->publish("updateGroup", ghoul::Dictionary());
}

void IswaGroup::clearGroup(){
    _groupEvent->publish("clearGroup", ghoul::Dictionary());
    unregisterProperties();
    _fieldlines.setValue(std::vector<int>());
}

std::shared_ptr<DataProcessor> IswaGroup::dataProcessor(){
    return _dataProcessor;
}

void IswaGroup::updateFieldlineSeeds(){
    std::vector<int> selectedOptions = _fieldlines.value();

    // SeedPath == map<int selectionValue, tuple< string name, string path, bool active > >
    for (auto& seedPath: _fieldlineState) {
        // if this option was turned off
        if( std::find(selectedOptions.begin(), selectedOptions.end(), seedPath.first)==selectedOptions.end() && std::get<2>(seedPath.second)){
            // if(OsEng.renderEngine().scene()->sceneGraphNode(std::get<0>(seedPath.second)) == nullptr) return;
            
            LDEBUG("Removed fieldlines: " + std::get<0>(seedPath.second));
            OsEng.scriptEngine().queueScript("openspace.removeSceneGraphNode('" + std::get<0>(seedPath.second) + "')");
            std::get<2>(seedPath.second) = false;
        // if this option was turned on
        } else if( std::find(selectedOptions.begin(), selectedOptions.end(), seedPath.first)!=selectedOptions.end() && !std::get<2>(seedPath.second)) {
            // if(OsEng.renderEngine().scene()->sceneGraphNode(std::get<0>(seedPath.second)) != nullptr) return;
            LDEBUG("Created fieldlines: " + std::get<0>(seedPath.second));
            IswaManager::ref().createFieldline(std::get<0>(seedPath.second), _kameleonPath, std::get<1>(seedPath.second));
            std::get<2>(seedPath.second) = true;
        }
    }
}

void IswaGroup::readFieldlinePaths(std::string indexFile){
    LINFO("Reading seed points paths from file '" << indexFile << "'");

    // Read the index file from disk
    std::ifstream seedFile(indexFile);
    if (!seedFile.good())
        LERROR("Could not open seed points file '" << indexFile << "'");
    else {
        std::string line;
        std::string fileContent;
        while (std::getline(seedFile, line)) {
            fileContent += line;
        }

        try{
            //Parse and add each fieldline as an selection
            json fieldlines = json::parse(fileContent);
            int i = 0;

            for (json::iterator it = fieldlines.begin(); it != fieldlines.end(); ++it) {
                _fieldlines.addOption({i, name()+"/"+it.key()});
                _fieldlineState[i] = std::make_tuple(name()+"/"+it.key(), it.value(), false);
                i++;
            }

        } catch(const std::exception& e) {
            LERROR("Error when reading json file with paths to seedpoints: " + std::string(e.what()));
        }
    }
}

} //namespace openspace