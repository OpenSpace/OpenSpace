/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <ghoul/opengl/texture.h>
#include <utility>

namespace {
    using namespace openspace;

    constexpr Property::PropertyInfo TransferFunctionInfo = {
        "TransferFunction",
        "Transfer function",
        "All the envelopes used in the transfer function.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo DataUnitInfo = {
        "DataUnit",
        "Data unit",
        "Unit of the data.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo MinValueInfo = {
        "MinValue",
        "Min value",
        "Minimum value in the data.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo MaxValueInfo = {
        "MaxValue",
        "Max value",
        "Maximum value in the data.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo SaveTransferFunctionInfo = {
        "SaveTransferFunction",
        "Save transfer function",
        "Save your transfer function.",
        Property::Visibility::AdvancedUser
    };
} // namespace

namespace openspace {

TransferFunctionHandler::TransferFunctionHandler(StringProperty prop)
    : PropertyOwner({ "TransferFunctionHandler", "Tranfer Function Handler" })
    , _transferFunctionPath(std::move(prop))
    , _dataUnit(DataUnitInfo)
    , _minValue(MinValueInfo)
    , _maxValue(MaxValueInfo)
    , _saveTransferFunction(SaveTransferFunctionInfo)
    , _transferFunctionProperty(TransferFunctionInfo)
    , _transferFunction(std::make_shared<TransferFunction>(_transferFunctionPath.value()))
{}

void TransferFunctionHandler::initialize() {
    addProperty(_transferFunctionPath);
    addProperty(_transferFunctionProperty);
    addProperty(_dataUnit);
    addProperty(_minValue);
    addProperty(_maxValue);
    addProperty(_saveTransferFunction);

    addTag("TF");
    _texture = std::make_shared<ghoul::opengl::Texture>(
        ghoul::opengl::Texture::FormatInit{
            .dimensions = glm::uvec3(1024, 1, 1),
            .type = GL_TEXTURE_1D,
            .format = ghoul::opengl::Texture::Format::RGBA,
            .dataType = GL_FLOAT
        },
        ghoul::opengl::Texture::SamplerInit{
            .wrapping = ghoul::opengl::Texture::WrappingMode::ClampToEdge
        }
    );

    if (!_filePath.empty()) {
        loadStoredEnvelopes();
    }

    _transferFunctionProperty.onChange([this]() { setTexture(); });
    _saveTransferFunction.onChange([this]() { saveEnvelopes(); });
}

void TransferFunctionHandler::setTexture() {
    _transferFunctionProperty.value().createTexture(*_texture);
}

void TransferFunctionHandler::setUnit(std::string unit) {
    _dataUnit = std::move(unit);
}

void TransferFunctionHandler::setMinAndMaxValue(float min, float max) {
    _minValue = std::to_string(min);
    _maxValue = std::to_string(max);
}

void TransferFunctionHandler::loadStoredEnvelopes() {
    VolumeTransferFunction tf;
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

bool TransferFunctionHandler::hasTexture() const {
    return _texture != nullptr;
}

std::shared_ptr<TransferFunction> TransferFunctionHandler::transferFunction() {
    return _transferFunction;
}

} // namespace openspace
