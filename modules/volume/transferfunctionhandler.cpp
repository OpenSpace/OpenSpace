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

#include <modules/volume/transferfunctionhandler.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/logging/logmanager.h>
#include <openspace/properties/scalarproperty.h>
#include <iostream>
#include <fstream>

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/filesystem/filesystem.h>

static const openspace::properties::Property::PropertyInfo TransferFunctionInfo = {
    "TransferFunction",
    "TransferFunction",
    "All the envelopes used in the transfer function"
};

static const openspace::properties::Property::PropertyInfo HistogramInfo = {
    "Histogram",
    "Histogram",
    "All the data"
};

static const openspace::properties::Property::PropertyInfo DataUnitInfo = {
    "DataUnit",
    "DataUnit",
    "Unit of the data"
};

static const openspace::properties::Property::PropertyInfo MinValueInfo = {
    "MinValue",
    "MinValue",
    "Minimum value in the data"
};

static const openspace::properties::Property::PropertyInfo MaxValueInfo = {
    "MaxValue",
    "MaxValue",
    "Maximum value in the data"
};

static const openspace::properties::Property::PropertyInfo SaveTransferFunctionInfo = {
    "SaveTransferFunction",
    "Save Transfer Function",
    "Save your transfer function"
};

namespace openspace::volume {

ghoul::opengl::Texture::FilterMode filtermode = ghoul::opengl::Texture::FilterMode::Linear;
ghoul::opengl::Texture::WrappingMode wrappingmode = ghoul::opengl::Texture::WrappingMode::ClampToEdge;


TransferFunctionHandler::TransferFunctionHandler(const properties::StringProperty& prop)
    : properties::PropertyOwner({ "TransferFunctionHandler" }),
    _transferFunctionPath(prop),
    _transferFunctionProperty(TransferFunctionInfo),
    _dataUnit(DataUnitInfo),
    _minValue(MinValueInfo),
    _maxValue(MaxValueInfo),
    _histogramProperty(HistogramInfo),
    _saveTransferFunction(SaveTransferFunctionInfo)
{
    _transferFunction = std::make_shared<openspace::TransferFunction>(_transferFunctionPath);
    LINFOC("TF_HANDLER", "handler constructor called");
}

void TransferFunctionHandler::initialize() {
    addProperty(_transferFunctionPath);
    addProperty(_transferFunctionProperty);
    addProperty(_histogramProperty);
    addProperty(_dataUnit);
    addProperty(_minValue);
    addProperty(_maxValue);
    addProperty(_saveTransferFunction);

    this->addTag("TF");
    _texture = std::make_shared<ghoul::opengl::Texture>(
        glm::size3_t(1024, 1, 1), ghoul::opengl::Texture::Format::RGBA,
        GL_RGBA, GL_FLOAT, filtermode, wrappingmode);

    if (_filePath != "") {
        loadStoredEnvelopes();
    }

    _transferFunctionProperty.onChange([this]() {
        setTexture();
    });

    _saveTransferFunction.onChange([this]() {
        saveEnvelopes();
    });

    // Use core package tf as well
    _transferFunction->updateTexture();

    LINFOC("TF_HANDLER", "handler initialized");
}

void TransferFunctionHandler::setHistogramProperty(std::shared_ptr<openspace::Histogram> histogram) {
    // histogram->print();
    _histogramProperty.setValue(histogram->getBinaryData());
}

void TransferFunctionHandler::setTexture() {
    if (_transferFunctionProperty.value().createTexture(_texture)) {
        uploadTexture();
    }
}

void TransferFunctionHandler::setUnit(const std::string& unit) {
    _dataUnit.set(unit);
}

void TransferFunctionHandler::setMinAndMaxValue(const float& min, const float& max) {
    std::stringstream s_min;
    s_min << min;
    std::string s = s_min.str();
    _minValue.set(s);
    std::stringstream s_max;
    s_max << max;
    std::string t = s_max.str();
    _maxValue.set(t);
}


void TransferFunctionHandler::loadStoredEnvelopes() {
    TransferFunction tf;
    tf.loadEnvelopesFromFile(_filePath);
    if (tf.hasEnvelopes()) {
        _transferFunctionProperty.setValue(tf);
        setTexture();
    }
}

void TransferFunctionHandler::saveEnvelopes() {
    _transferFunctionProperty.value().saveEnvelopesToFile(_filePath);
}

void TransferFunctionHandler::setFilepath(const std::string& path) {
    _filePath = path;
    // _transferFunction->updateTexture();
}

ghoul::opengl::Texture& TransferFunctionHandler::getTexture() {
    return *_texture.get();
}

void TransferFunctionHandler::uploadTexture() {
    _texture->uploadTexture();
}

bool TransferFunctionHandler::hasTexture() {
    if (_texture == nullptr)
        return false;
    return true;
}

std::shared_ptr<openspace::TransferFunction> TransferFunctionHandler::getTransferFunction() {
    return _transferFunction;
}

} // namespace openspace::volume
