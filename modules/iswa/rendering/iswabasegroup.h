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

#ifndef __OPENSPACE_MODULE_ISWA___ISWABASEGROUP___H__
#define __OPENSPACE_MODULE_ISWA___ISWABASEGROUP___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <ghoul/designpattern/event.h>

namespace openspace {

class DataProcessor;
class IswaCygnet;

class IswaBaseGroup : public properties::PropertyOwner {
public:
    IswaBaseGroup(std::string name, std::string type);
    ~IswaBaseGroup();
    bool isType(const std::string& type) const;

    void updateGroup();
    virtual void clearGroup();

    std::shared_ptr<DataProcessor> dataProcessor();
    ghoul::Event<ghoul::Dictionary>& groupEvent();

protected:
    void registerProperties();
    void unregisterProperties();

    properties::BoolProperty _enabled;
    properties::FloatProperty _alpha;
    properties::TriggerProperty _delete;

    ghoul::Event<ghoul::Dictionary> _groupEvent;
    std::shared_ptr<DataProcessor> _dataProcessor;

    bool _registered = false;
    std::string _type;
};

} //namespace openspace

#endif // __OPENSPACE_MODULE_ISWA___ISWABASEGROUP___H__
