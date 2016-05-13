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

#include <modules/globebrowsing/other/layeredtextureshaderprovider.h>

#include <openspace/rendering/renderengine.h>
#include <openspace/engine/openspaceengine.h>

#include "ghoul\misc\dictionary.h"

#include <string>

namespace {
    const std::string _loggerCat = "LayeredTextureShaderProvider";
}

namespace openspace {

    bool LayeredTextureInfo::operator==(const LayeredTextureInfo& other) const
    {
        return numLayers == other.numLayers && keyNumLayers == other.keyNumLayers;
    }

    bool LayeredTexturePreprocessingData::operator==(
        const LayeredTexturePreprocessingData& other) const
    {
        
        if (layeredTextureInfo.size() != other.layeredTextureInfo.size())
        {
            return false;
        }
        else
        {
            bool equal = true;
            for (size_t i = 0; i < layeredTextureInfo.size(); i++)
            {
                equal = equal && (layeredTextureInfo[i] == other.layeredTextureInfo[i]);
            }
            return equal;
        }
    }

    LayeredTextureShaderProvider::LayeredTextureShaderProvider(
        const std::string& shaderName,
        const std::string& vsPath,
        const std::string& fsPath)
        : _shaderName(shaderName)
        , _vsPath(vsPath)
        , _fsPath(fsPath)
    {
    
    }
    
    LayeredTextureShaderProvider::~LayeredTextureShaderProvider()
    {
        if (_programObject) {
            RenderEngine& renderEngine = OsEng.renderEngine();
            renderEngine.removeRenderProgram(_programObject);
            _programObject = nullptr;
        }
    }

    ProgramObject* LayeredTextureShaderProvider::getUpdatedShaderProgram(
        LayeredTexturePreprocessingData preprocessingData)
    {
        if (!(preprocessingData == _preprocessingData) || _programObject == nullptr)
        { // No need to recompile shader. Shader is up to date and can be returned.
            recompileShaderProgram(preprocessingData);
        }
        return _programObject.get();
    }

    void LayeredTextureShaderProvider::recompileShaderProgram(
        LayeredTexturePreprocessingData preprocessingData)
    {
        _preprocessingData = preprocessingData;
        ghoul::Dictionary shaderDictionary;
        
        // Different texture types can be height maps or color texture for example
        // These are used differently within the shaders.
        auto textureTypes = _preprocessingData.layeredTextureInfo;
        for (size_t i = 0; i < textureTypes.size(); i++)
        {
            shaderDictionary.setValue(
                textureTypes[i].keyNumLayers, textureTypes[i].numLayers);
        }
        
        _programObject = OsEng.renderEngine().buildRenderProgram(
            _shaderName,
            _vsPath,
            _fsPath);
        
        /*
        try {
            _programObject = ProgramObject::Build(
                _shaderName,
                _vsPath,
                _fsPath,
                shaderDictionary);
        }
        catch (ghoul::RuntimeError& error) {

            LERROR(error.message);
        }
        */
        ghoul_assert(_programObject != nullptr, "Failed to initialize programObject!");
        using IgnoreError = ProgramObject::IgnoreError;
        _programObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    }

}  // namespace openspace
