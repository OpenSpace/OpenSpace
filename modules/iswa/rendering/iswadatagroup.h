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

#ifndef __OPENSPACE_MODULE_ISWA___ISWADATAGROUP___H__
#define __OPENSPACE_MODULE_ISWA___ISWADATAGROUP___H__

#include <modules/iswa/rendering/iswabasegroup.h>

#include <openspace/properties/selectionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vector/vec2property.h>

namespace openspace {

class IswaDataGroup : public IswaBaseGroup {
public:
    IswaDataGroup(std::string name, std::string type);
    ~IswaDataGroup();

    void registerOptions(
        const std::vector<properties::SelectionProperty::Option>& options);
    std::vector<int> dataOptionsValue() const;

protected:
    void registerProperties();
    void createDataProcessor();

    properties::BoolProperty _useLog;
    properties::BoolProperty _useHistogram;
    properties::BoolProperty _autoFilter;
    properties::Vec2Property _normValues;
    properties::Vec2Property _backgroundValues;
    properties::StringProperty _transferFunctionsFile;
    properties::SelectionProperty _dataOptions;
};

} //namespace openspace

#endif // __OPENSPACE_MODULE_ISWA___ISWADATAGROUP___H__
