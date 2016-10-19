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

#ifndef LAYER_H
#define LAYER_H

#include <memory>
#include <string>

#include <array>

#include <ghoul/opengl/programobject.h>
#include <openspace/properties/propertyowner.h>

#include <modules/globebrowsing/layered_rendering/perlayersetting.h>
#include <modules/globebrowsing/tile/tileindex.h>

namespace openspace {
namespace globebrowsing {

/*
class TileProvider;
    
class Layer : public properties::PropertyOwner {
public:
    Layer(const ghoul::Dictionary& dict);
    
    void bind(ProgramObject* program, const TileIndex& tileIndex);


    static const size_t NUM_LAYER_SETTINGS_VARIABLES = 3;
    static const size_t NUM_TILE_DATA_VARIABLES = 3;
    static const size_t NUM_BLEND_TEXTURES = 3;

private:
    
    //void ensureIdsAreUpdated(LayeredTextureShaderProvider* shaderProvider);


    std::string _name;
    std::unique_ptr<TileProvider> _tileProvider;

    std::array<std::array<GLint, NUM_TILE_DATA_VARIABLES>,NUM_BLEND_TEXTURES> _tileUniformIds;
    std::array<GLint, NUM_LAYER_SETTINGS_VARIABLES> _layerSettingsUniformIds;

    bool _enabled;
    
};
*/
} // namespace globebrowsing
} // namespace openspace
#endif  // LAYER_H
