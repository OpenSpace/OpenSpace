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

#ifndef __OPENSPACE_MODULE_VOLUME___TRANSFERFUNCTIONHANDLER___H__
#define __OPENSPACE_MODULE_VOLUME___TRANSFERFUNCTIONHANDLER___H__
#include <functional>
//#include <openspace/rendering/transferfunction.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/glm.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/util/histogram.h>
#include <modules/volume/transferfunctionproperty.h>

#include <vector>
#include <memory>

namespace ghoul { class Dictionary; }

namespace openspace {
    namespace volume {
        class Envelope;
        
        class TransferFunctionHandler : public properties::PropertyOwner{
        public:
            TransferFunctionHandler(const properties::StringProperty& prop);
            
            void initialize();
            void update();
            
            void buildHistogram(float *data);

            void setTexture();

            ghoul::opengl::Texture& getTexture();
            void uploadTexture();
            void bindTexture();
            bool hasTexture();

        private:
            bool _needsUpdate = true;
            properties::StringProperty _transferFunctionPath;
            properties::TransferFunctionProperty _transferFunctionProperty;
            std::shared_ptr<Histogram> _histogram;
            std::unique_ptr<ghoul::opengl::Texture> _texture = nullptr;
        };
    }//namespace volume
}//namespace openspace
#endif __OPENSPACE_MODULE_VOLUME___TRANSFERFUNCTIONHANDLER___H__
