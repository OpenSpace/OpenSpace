/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/rendering/transferfunction.h>
#include <openspace/util/histogram.h>
#include <ghoul/opengl/texture.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo TransferFunctionInfo = {
        "TransferFunction",
        "TransferFunction",
        "All the envelopes used in the transfer function",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DataUnitInfo = {
        "DataUnit",
        "DataUnit",
        "Unit of the data",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MinValueInfo = {
        "MinValue",
        "MinValue",
        "Minimum value in the data",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MaxValueInfo = {
        "MaxValue",
        "MaxValue",
        "Maximum value in the data",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SaveTransferFunctionInfo = {
        "SaveTransferFunction",
        "Save Transfer Function",
        "Save your transfer function",
        // @VISIBILITY(3.5)
        openspace::properties::Property::Visibility::AdvancedUser
    };
} // namespace

namespace openspace::volume {

TransferFunctionHandler::TransferFunctionHandler(properties::StringProperty prop)
    : properties::PropertyOwner({ "TransferFunctionHandler", "Tranfer Function Handler" })
    , _transferFunctionPath(std::move(prop))
    , _dataUnit(DataUnitInfo)
    , _minValue(MinValueInfo)
    , _maxValue(MaxValueInfo)
    , _saveTransferFunction(SaveTransferFunctionInfo)
    , _transferFunctionProperty(TransferFunctionInfo)
{
    _transferFunction = std::make_shared<openspace::TransferFunction>(
        _transferFunctionPath.value()
    );
}

void TransferFunctionHandler::initialize() {
    addProperty(_transferFunctionPath);
    addProperty(_transferFunctionProperty);
    addProperty(_dataUnit);
    addProperty(_minValue);
    addProperty(_maxValue);
    addProperty(_saveTransferFunction);

    addTag("TF");
    _texture = std::make_shared<ghoul::opengl::Texture>(
        glm::uvec3(1024, 1, 1),
        GL_TEXTURE_1D,
        ghoul::opengl::Texture::Format::RGBA,
        GL_RGBA,
        GL_FLOAT,
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::ClampToEdge
    );

    if (!_filePath.empty()) {
        loadStoredEnvelopes();
    }

    _transferFunctionProperty.onChange([this]() { setTexture(); });

    _saveTransferFunction.onChange([this]() { saveEnvelopes(); });
}

void TransferFunctionHandler::setTexture() {
    if (_transferFunctionProperty.value().createTexture(*_texture)) {
        uploadTexture();
    }
}

void TransferFunctionHandler::setUnit(std::string unit) {
    _dataUnit = std::move(unit);
}

void TransferFunctionHandler::setMinAndMaxValue(float min, float max) {
    _minValue = std::to_string(min);
    _maxValue = std::to_string(max);
}

void TransferFunctionHandler::loadStoredEnvelopes() {
    TransferFunction tf;
    tf.loadEnvelopesFromFile(_filePath);
    if (tf.hasEnvelopes()) {
        _transferFunctionProperty = std::move(tf);
        setTexture();
    }
}

void TransferFunctionHandler::saveEnvelopes() {
    _transferFunctionProperty.value().saveEnvelopesToFile(_filePath);
}

void TransferFunctionHandler::setFilepath(std::string path) {
    _filePath = std::move(path);
}

ghoul::opengl::Texture& TransferFunctionHandler::texture() {
    return *_texture;
}

void TransferFunctionHandler::uploadTexture() {
    _texture->uploadTexture();
}

bool TransferFunctionHandler::hasTexture() {
    return _texture != nullptr;
}

std::shared_ptr<openspace::TransferFunction> TransferFunctionHandler::transferFunction() {
    return _transferFunction;
}

} // namespace openspace::volume
