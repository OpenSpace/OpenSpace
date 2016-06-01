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

#ifndef __ISWAGROUP_H__
#define __ISWAGROUP_H__

#include <ghoul/designpattern/event.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/selectionproperty.h>
#include <openspace/properties/vectorproperty.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/engine/openspaceengine.h>
// #include <modules/iswa/rendering/iswacygnet.h>
#include <openspace/properties/triggerproperty.h>
#include <modules/iswa/util/iswamanager.h>
#include <modules/iswa/util/dataprocessor.h>


namespace openspace{
class IswaCygnet;

class IswaGroup : public properties::PropertyOwner{
public:
	IswaGroup(std::string name, IswaManager::CygnetType type);
	IswaGroup(std::string name, std::string type);
	~IswaGroup();
	void registerOptions(const std::vector<properties::SelectionProperty::Option>& options);
	void registerFieldLineOptions(const std::vector<properties::SelectionProperty::Option>& options);
	bool isType(std::string type);

	void clearGroup();
	void updateGroup();

	std::shared_ptr<DataProcessor> dataProcessor();
	std::shared_ptr<ghoul::Event<ghoul::Dictionary> > groupEvent(){ return _groupEvent; };
	std::vector<int> fieldlineValue() {return _fieldlines.value();}
	std::vector<int> dataOptionsValue() {return _dataOptions.value();}
	void setFieldlineInfo(std::string fieldlineIndexFile, std::string kameleonPath);

private:
	void createDataProcessor();

	void registerProperties();
	void unregisterProperties();

    void readFieldlinePaths(std::string indexFile);
    void updateFieldlineSeeds();
    void clearFieldlines();

	properties::BoolProperty _enabled;
	properties::FloatProperty _alpha;
    properties::BoolProperty _useLog;
    properties::BoolProperty _useHistogram;
    properties::BoolProperty _autoFilter;
    properties::Vec2Property _normValues;
    properties::Vec2Property _backgroundValues;
    properties::StringProperty _transferFunctionsFile;
	properties::SelectionProperty _dataOptions;
	properties::SelectionProperty _fieldlines;
	properties::TriggerProperty _delete;

	int _id;
	std::shared_ptr<ghoul::Event<ghoul::Dictionary> > _groupEvent;
    std::shared_ptr<DataProcessor> _dataProcessor;

	bool _registered;

	std::string _fieldlineIndexFile;
	std::string _kameleonPath;
	std::map<int, std::tuple<std::string, std::string, bool> > _fieldlineState;
	std::string _type;
};

} //namespace openspace
#endif