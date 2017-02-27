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

#include <modules/iswa/rendering/iswakameleongroup.h>

#include <fstream>
#include <modules/iswa/ext/json/json.hpp>

#include <modules/iswa/util/dataprocessortext.h>
#include <modules/iswa/util/dataprocessorjson.h>
#include <modules/iswa/util/dataprocessorkameleon.h>

#include <modules/iswa/rendering/dataplane.h>
#include <modules/iswa/rendering/datasphere.h>
#include <modules/iswa/rendering/kameleonplane.h>

namespace {
    const std::string _loggerCat = "IswaDataGroup";
    using json = nlohmann::json;
}

namespace openspace{
IswaKameleonGroup::IswaKameleonGroup(std::string name, std::string type)
    :IswaDataGroup(name, type)
    ,_resolution("resolution", "Resolution%", 100.0f, 10.0f, 200.0f)
    ,_fieldlines("fieldlineSeedsIndexFile", "Fieldline Seedpoints")
    ,_fieldlineIndexFile("")
    ,_kameleonPath("")
{
    addProperty(_resolution);
    addProperty(_fieldlines);
    registerProperties();
}

IswaKameleonGroup::~IswaKameleonGroup(){}

void IswaKameleonGroup::clearGroup(){
    IswaBaseGroup::clearGroup();
    clearFieldlines();
}

std::vector<int> IswaKameleonGroup::fieldlineValue(){
    return _fieldlines.value();
}

void IswaKameleonGroup::setFieldlineInfo(std::string fieldlineIndexFile, std::string kameleonPath){
    if(fieldlineIndexFile != _fieldlineIndexFile){
        _fieldlineIndexFile = fieldlineIndexFile;
        readFieldlinePaths(_fieldlineIndexFile);
    }

    if(kameleonPath != _kameleonPath){
        _kameleonPath = kameleonPath;
        clearFieldlines();
        updateFieldlineSeeds();
    }
}


void IswaKameleonGroup::registerProperties(){
    //OsEng.gui()._iswa.registerProperty(&_resolution);
    //OsEng.gui()._iswa.registerProperty(&_fieldlines);
    
    _resolution.onChange([this]{
        LDEBUG("Group " + name() + " published resolutionChanged");
        _groupEvent->publish("resolutionChanged", ghoul::Dictionary({{"resolution", _resolution.value()}}));
    });

    _fieldlines.onChange([this]{
        updateFieldlineSeeds();
    });
}

void IswaKameleonGroup::readFieldlinePaths(std::string indexFile){
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
                _fieldlines.addOption({i, it.key()});
                _fieldlineState[i] = std::make_tuple(name()+"/"+it.key(), it.value(), false);
                i++;
            }

        } catch(const std::exception& e) {
            LERROR("Error when reading json file with paths to seedpoints: " + std::string(e.what()));
        }
    }
}

void IswaKameleonGroup::updateFieldlineSeeds(){
    std::vector<int> selectedOptions = _fieldlines.value();

    // SeedPath == map<int selectionValue, tuple< string name, string path, bool active > >
    for (auto& seedPath: _fieldlineState) {
        // if this option was turned off
        if( std::find(selectedOptions.begin(), selectedOptions.end(), seedPath.first)==selectedOptions.end() && std::get<2>(seedPath.second)){
            LDEBUG("Removed fieldlines: " + std::get<0>(seedPath.second));
            OsEng.scriptEngine().queueScript(
                "openspace.removeSceneGraphNode('" + std::get<0>(seedPath.second) + "')",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
            std::get<2>(seedPath.second) = false;
        // if this option was turned on
        } else if( std::find(selectedOptions.begin(), selectedOptions.end(), seedPath.first)!=selectedOptions.end() && !std::get<2>(seedPath.second)) {
            LDEBUG("Created fieldlines: " + std::get<0>(seedPath.second));
            IswaManager::ref().createFieldline(std::get<0>(seedPath.second), _kameleonPath, std::get<1>(seedPath.second));
            std::get<2>(seedPath.second) = true;
        }
    }
}

void IswaKameleonGroup::clearFieldlines(){
        // SeedPath == map<int selectionValue, tuple< string name, string path, bool active > >
    for (auto& seedPath: _fieldlineState) {
        if(std::get<2>(seedPath.second)){
            LDEBUG("Removed fieldlines: " + std::get<0>(seedPath.second));
            OsEng.scriptEngine().queueScript(
                "openspace.removeSceneGraphNode('" + std::get<0>(seedPath.second) + "')",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
            std::get<2>(seedPath.second) = false;
        }
    }
}

void IswaKameleonGroup::changeCdf(std::string path){
    _kameleonPath = path;
    clearFieldlines();
    updateFieldlineSeeds();

    _groupEvent->publish("cdfChanged", ghoul::Dictionary({{"path", path}}));
}

} //namespace openspace
