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


#include <modules/globebrowsing/other/temporaltileprovider.h>
#include <modules/globebrowsing/other/tileprovider.h>
#include <modules/globebrowsing/other/threadpool.h>

#include <ghoul/misc/dictionary.h>


#include <memory>
#include <vector>
#include <string>


namespace openspace {

    class TileProviderManager {
    public:
        struct TileProviderWithName {
            std::string name;
            std::shared_ptr<TileProvider> tileProvider;
            bool isActive;
        };

        TileProviderManager(const ghoul::Dictionary& dict);
        ~TileProviderManager();

        static ThreadPool tileRequestThreadPool;

        const std::vector<std::shared_ptr<TileProvider> > getActiveHeightMapProviders();
        const std::vector<std::shared_ptr<TileProvider> > getActiveColorTextureProviders();

        std::vector<TileProviderWithName>& heightMapProviders();
        std::vector<TileProviderWithName>& colorTextureProviders();

    private:
        static void initTexures(std::vector<TileProviderWithName>& destination,
            const ghoul::Dictionary& dict, const TileProviderInitData& initData);

        static std::shared_ptr<TileProvider> initProvider(const std::string& file, 
            const TileProviderInitData& initData);

        std::vector<TileProviderWithName> _heightMapProviders;
        std::vector<TileProviderWithName> _colorTextureProviders;
    };

} // namespace openspace
#endif  // __TILE_PROVIDER_MANAGER_H__