/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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

#ifndef __DATAPLANE_H__
#define __DATAPLANE_H__

#include <modules/iswa/rendering/cygnetplane.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/properties/vectorproperty.h>
#include <openspace/properties/selectionproperty.h>
#include <modules/iswa/util/dataprocessor.h>

namespace openspace{
class IswaGroup;

class DataPlane : public CygnetPlane {
friend class IswaGroup;
 public:
     DataPlane(const ghoul::Dictionary& dictionary);
     ~DataPlane();

     bool initialize() override;

 private:
    virtual bool loadTexture() override;
    virtual bool updateTexture() override;

    virtual bool readyToRender() const override;
    virtual void setUniformAndTextures() override;

    void setTransferFunctions(std::string tfPath);
    void fillOptions();

    properties::SelectionProperty _dataOptions;
    properties::StringProperty _transferFunctionsFile;
    properties::Vec2Property _backgroundValues;

    properties::Vec2Property _normValues;

    properties::BoolProperty _useLog;
    properties::BoolProperty _useHistogram;
    properties::BoolProperty _autoFilter;

    std::string _dataBuffer;

    std::shared_ptr<DataProcessor> _dataProcessor; 

 };
 
 } // namespace openspace

#endif //__DATAPLANE_H__