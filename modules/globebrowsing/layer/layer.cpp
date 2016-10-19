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

#include <modules/globebrowsing/layer/layer.h>

#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>
#include <modules/globebrowsing/tile/tileselector.h>

#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/assert.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/programobject.h>

#include <string>
#include <memory>

namespace {
    const std::string _loggerCat = "Layer";
    
    const std::string KEY_NAME = "Name";
    const std::string KEY_ENABLED = "Enabled";
    
}

namespace openspace {
namespace globebrowsing {
    
    Layer::Layer(const ghoul::Dictionary& dict){
        dict.getValue(KEY_NAME, _name);
        dict.getValue(KEY_ENABLED, _enabled);
        _tileProvider = std::unique_ptr<TileProvider>(TileProvider::createFromDictionary(dict));
        
        
    }

    void Layer::bind(ProgramObject* programObject, const TileIndex& tileIndex){
        /*
        ChunkTile tat = TileSelector::getHighestResolutionTile(_tileProvider.get(), tileIndex);
        if (tat.tile.status == Tile::Status::Unavailable) {
            tat.tile = _tileProvider->getDefaultTile();
            tat.uvTransform.uvOffset = { 0, 0 };
            tat.uvTransform.uvScale = { 1, 1 };
        }

        activateTileAndSetTileUniforms(
            programUniformHandler,
            LayeredTextures::TextureCategory(category),
            LayeredTextures::BlendLayerSuffixes::none,
            i,
            texUnits[category][i].blendTexture0,
            tat);

        // If blending is enabled, two more textures are needed
        if (layeredTexturePreprocessingData.layeredTextureInfo[category].layerBlendingEnabled) {
            tat tatParent1 = TileSelector::getHighestResolutionTile(_tileProvider.get(), tileIndex, 1);
            if (tatParent1.tile.status == Tile::Status::Unavailable) {
                tatParent1 = tat;
            }
            activateTileAndSetTileUniforms(
                programUniformHandler,
                LayeredTextures::TextureCategory(category),
                LayeredTextures::BlendLayerSuffixes::Parent1,
                i,
                texUnits[category][i].blendTexture1,
                tatParent1);

            ChunkTile tatParent2 = TileSelector::getHighestResolutionTile(_tileProvider.get(), tileIndex, 2);
            if (tatParent2.tile.status == Tile::Status::Unavailable) {
                tatParent2 = tatParent1;
            }
            activateTileAndSetTileUniforms(
                programUniformHandler,
                LayeredTextures::TextureCategory(category),
                LayeredTextures::BlendLayerSuffixes::Parent2,
                i,
                texUnits[category][i].blendTexture2,
                tatParent2);
        }
         */
        
    }
    /*
    void Layer::ensureIdsAreUpdated(LayeredTextureShaderProvider* shaderProvider){
        if (shaderProvider->updatedOnLastCall())
        {
            _shaderProvider = shaderProvider;
            // Ignore errors since this loops through even uniforms that does not exist.
            _shaderProvider->_programObject->setIgnoreUniformLocationError(
                                                                           ProgramObject::IgnoreError::Yes);
            for (size_t i = 0; i < LayeredTextures::NUM_TEXTURE_CATEGORIES; i++)
            {
                for (size_t j = 0; j < LayeredTextures::NUM_BLEND_TEXTURES; j++)
                {
                    for (size_t k = 0; k < LayeredTextures::MAX_NUM_TEXTURES_PER_CATEGORY;
                         k++)
                    {
                        for (size_t l = 0; l < LayeredTextures::NUM_TILE_DATA_VARIABLES; l++)
                        {
                            _tileUniformIds[i][j][k][l] =
                            _shaderProvider->_programObject->uniformLocation(
                                                                             LayeredTextures::TEXTURE_CATEGORY_NAMES[i] +
                                                                             LayeredTextures::blendLayerSuffixes[j] +
                                                                             "[" + std::to_string(k) + "]." +
                                                                             LayeredTextures::glslTileDataNames[l]);
                        }
                    }
                }
            }
            for (size_t i = 0; i < LayeredTextures::NUM_TEXTURE_CATEGORIES; i++)
            {
                for (size_t k = 0; k < LayeredTextures::MAX_NUM_TEXTURES_PER_CATEGORY;
                     k++)
                {
                    for (size_t l = 0; l < LayeredTextures::NUM_LAYER_SETTINGS_VARIABLES; l++)
                    {
                        _layerSettingsUniformIds[i][k][l] =
                        _shaderProvider->_programObject->uniformLocation(
                                                                         LayeredTextures::TEXTURE_CATEGORY_NAMES[i] +
                                                                         "Settings" +
                                                                         "[" + std::to_string(k) + "]." +
                                                                         LayeredTextures::layerSettingsIds[l]);
                    }
                }
            }
            // Reset ignore errors
            _shaderProvider->_programObject->setIgnoreUniformLocationError(
                                                                           ProgramObject::IgnoreError::No);
        }
    }
    */

}  // namespace globebrowsing
}  // namespace openspace
