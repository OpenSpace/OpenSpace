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

#include <openspace/properties/propertyowner.h>

#include <modules/volume/transferfunctionproperty.h>
#include <openspace/properties/binaryproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <memory>
#include <string>

namespace openspace {
    class Histogram;
    class TransferFunction;
} // namespace openspace

namespace openspace::volume {

class Envelope;

class TransferFunctionHandler : public properties::PropertyOwner {
public:
    TransferFunctionHandler(const properties::StringProperty& prop);

    void initialize();

    void setHistogramProperty(openspace::Histogram& histogram);

    void setTexture();
    void loadStoredEnvelopes();
    void saveEnvelopes();
    void setFilepath(std::string path);
    void setUnit(std::string unit);
    void setMinAndMaxValue(float min, float max);

    ghoul::opengl::Texture& texture();
    void uploadTexture();
    bool hasTexture();

    std::shared_ptr<openspace::TransferFunction> transferFunction();

private:
    properties::StringProperty _transferFunctionPath;
    properties::StringProperty _dataUnit;
    properties::StringProperty _minValue;
    properties::StringProperty _maxValue;
    properties::TriggerProperty _saveTransferFunction;
    std::string _filePath;
    properties::TransferFunctionProperty _transferFunctionProperty;
    properties::BinaryProperty _histogramProperty;
    std::shared_ptr<openspace::TransferFunction> _transferFunction;
    std::shared_ptr<ghoul::opengl::Texture> _texture;
};

} //namespace openspace::volume

#endif // __OPENSPACE_MODULE_VOLUME___TRANSFERFUNCTIONHANDLER___H__
