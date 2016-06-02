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

#ifndef __LAYERED_TEXTURE_SHADER_PROVIDER__
#define __LAYERED_TEXTURE_SHADER_PROVIDER__

#include <vector>
#include <string>
#include "ghoul/opengl/programobject.h"

//////////////////////////////////////////////////////////////////////////////////////////
//							  LAYERED TEXTURE SHADER PROVIDER						    //
//////////////////////////////////////////////////////////////////////////////////////////

namespace openspace {

    using namespace ghoul::opengl;

    struct LayeredTextureInfo
    {
        std::string keyLastLayerIndex;
        int lastLayerIndex;

        bool operator==(const LayeredTextureInfo& other) const;
    };

    struct LayeredTexturePreprocessingData
    {
        std::vector<LayeredTextureInfo> layeredTextureInfo;

        bool operator==(const LayeredTexturePreprocessingData& other) const;
    };

    class LayeredTextureShaderProvider
    {
    public:
        LayeredTextureShaderProvider(
            const std::string& shaderName,
            const std::string& vsPath,
            const std::string& fsPath);
        ~LayeredTextureShaderProvider();

        ProgramObject* getUpdatedShaderProgram(
            LayeredTexturePreprocessingData preprocessingData);

    private:
        void recompileShaderProgram(LayeredTexturePreprocessingData preprocessingData);

        std::unique_ptr<ProgramObject> _programObject;
        LayeredTexturePreprocessingData _preprocessingData;

        const std::string _shaderName;
        const std::string _vsPath;
        const std::string _fsPath;
    };

}  // namespace openspace

#endif  // __LAYERED_TEXTURE_SHADER_PROVIDER__