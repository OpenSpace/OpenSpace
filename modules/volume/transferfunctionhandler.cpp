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

#include <modules/volume/transferfunctionhandler.h>
#include <ghoul/misc/dictionary.h>
#include <openspace/properties/scalarproperty.h>

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/filesystem/filesystem.h>

static const openspace::properties::Property::PropertyInfo TransferFunctionInfo = {
    "TransferFunction",
    "TransferFunction",
    "All the envelopes"
};

namespace openspace {
    namespace volume {
        
        TransferFunctionHandler::TransferFunctionHandler(const properties::StringProperty& prop)
            : properties::PropertyOwner({ "TransferFunctionHandler" }),
            _transferFunctionPath(prop),
            _transferFunctionProperty(TransferFunctionInfo)
        {
        }

        void TransferFunctionHandler::initialize() {
            addProperty(_transferFunctionPath);
            addProperty(_transferFunctionProperty);
        }

        void TransferFunctionHandler::update() {
            if (_needsUpdate) {
                if (!_transferFunctionProperty.value().isTextureSet()) {
                    setTexture();
                    uploadTexture();
                }
            }
        }

        void TransferFunctionHandler::buildHistogram(float *data) {
//            _histogram = std::make_shared<Histogram>(0, 0.1, 1000, data);
        }

        void TransferFunctionHandler::setTexture() {
           _transferFunctionProperty.value().createTexture(&_texture);
           _needsUpdate = false;
        }

        ghoul::opengl::Texture& TransferFunctionHandler::getTexture() {
            ghoul_assert(_texture != nullptr, "Transfer function is null");
            return *_texture.get();
        }

        void TransferFunctionHandler::uploadTexture() {
            //_texture->uploadTexture();
        }

        bool TransferFunctionHandler::hasTexture() {
            return !_texture;
        }

    } //namespace volume
} // namespace openspace
