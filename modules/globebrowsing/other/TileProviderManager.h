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

#ifndef __TILE_PROVIDER_MANAGER_H__
#define __TILE_PROVIDER_MANAGER_H__

#include "modules/globebrowsing/other/tileprovider.h"

#include <memory>
#include <vector>
#include <string>

namespace openspace {

    class TileProviderManager
    {
    public:
        TileProviderManager();
        ~TileProviderManager();

        void addHeightMap(std::string name, std::shared_ptr<TileProvider> tileProvider);
        void addColorTexture(std::string name, std::shared_ptr<TileProvider> tileProvider);

        std::shared_ptr<TileProvider> getHeightMap(std::string name);
        std::shared_ptr<TileProvider> getColorTexture(std::string name);

        const std::map<std::string, std::shared_ptr<TileProvider> >& heightMapProviders();
        const std::map<std::string, std::shared_ptr<TileProvider> >& colorTextureProviders();
    private:
        std::map<std::string, std::shared_ptr<TileProvider> > _heightMapProviders;
        std::map<std::string, std::shared_ptr<TileProvider> > _colorTextureProviders;
    };

} // namespace openspace
#endif  // __TILE_PROVIDER_MANAGER_H__