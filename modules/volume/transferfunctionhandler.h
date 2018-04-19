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

#ifndef __OPENSPACE_MODULE_VOLUME___TRANSFERFUNCTIONHANDLER___H__
#define __OPENSPACE_MODULE_VOLUME___TRANSFERFUNCTIONHANDLER___H__

#include <functional>
#include <ghoul/opengl/texture.h>
#include <ghoul/glm.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/binaryproperty.h>
#include <openspace/properties/scalarproperty.h>
#include <openspace/util/histogram.h>
#include <modules/volume/transferfunctionproperty.h>
#include <modules/volume/transferfunction.h>
#include <openspace/rendering/transferfunction.h>

#include <vector>
#include <memory>

namespace openspace::volume {

class Envelope;

class TransferFunctionHandler : public properties::PropertyOwner{
public:
    TransferFunctionHandler(const properties::StringProperty& prop);

    void initialize();

    void setHistogramProperty(std::shared_ptr<openspace::Histogram> histogram);

    void setTexture();
    void loadStoredEnvelopes();
    void saveEnvelopes();
    void setFilepath(const std::string& path);
    void setUnit(const std::string& unit);
    void setMinAndMaxValue(const float& min, const float& max);

    ghoul::opengl::Texture& getTexture();
    void uploadTexture();
    bool hasTexture();

    std::shared_ptr<openspace::TransferFunction> getTransferFunction();

private:
    bool useTxtTexture = true;
    properties::StringProperty _transferFunctionPath;
    properties::StringProperty _dataUnit;
    properties::StringProperty _minValue;
    properties::StringProperty _maxValue;
    properties::TriggerProperty _saveTransferFunction;
    std::string _filePath;
    properties::TransferFunctionProperty _transferFunctionProperty;
    properties::BinaryProperty _histogramProperty;
    std::shared_ptr<openspace::TransferFunction> _transferFunction;
    std::shared_ptr<ghoul::opengl::Texture> _texture = nullptr;
};

} //namespace openspace::volume

#endif // __OPENSPACE_MODULE_VOLUME___TRANSFERFUNCTIONHANDLER___H__
