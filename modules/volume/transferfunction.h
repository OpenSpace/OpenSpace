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

#ifndef __OPENSPACE_MODULE_VOLUME___TRANSFERFUNCTION___H__
#define __OPENSPACE_MODULE_VOLUME___TRANSFERFUNCTION___H__

#include <ghoul/glm.h>
#include <modules/volume/envelope.h>

namespace openspace {
    namespace volume {
        class TransferFunction {
        public:
            TransferFunction();
            TransferFunction(int s);
            TransferFunction(std::string);
            TransferFunction(const TransferFunction& tf);

            TransferFunction(TransferFunction&& tf);

            TransferFunction& operator=(TransferFunction&& tf);
            bool operator!=(const TransferFunction& tf);

            void update();
            void createTexture(std::unique_ptr<ghoul::opengl::Texture> *ptr);
            bool isTextureSet() const;
            std::string getSerializedToString() const;

            

        private:
            int width = 512;
            double lower = 0.0;
            double upper = 1.0;
            bool _textureSet = false;
            std::vector<Envelope> _envelopes;
        };

    }//namespace volume
}//namespace openspace
#endif __OPENSPACE_MODULE_VOLUME___TRANSFERFUNCTION___H__
