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

#ifndef __TILE_PROVIDER_FACTORY_H__
#define __TILE_PROVIDER_FACTORY_H__


#include <modules/globebrowsing/tile/temporaltileprovider.h>
#include <modules/globebrowsing/tile/tileprovider.h>

#include <ghoul/misc/dictionary.h>

#include <memory>
#include <string>
#include <functional>


namespace openspace {

    class TileProviderFactory {
    public:

        static std::shared_ptr<TileProviderFactory> ref();

        std::shared_ptr<TileProvider> create(const std::string& type, const std::string& desc, const TileProviderInitData& initData);

    private:

        TileProviderFactory();
        void initialize();

        typedef std::function<std::shared_ptr<TileProvider>(const std::string&, const TileProviderInitData&)> ConcreteFactory;

        std::unordered_map<std::string, ConcreteFactory> _factoryMap;

        static std::shared_ptr<TileProviderFactory> _ref;
    };

  
} // namespace openspace
#endif  // __TILE_PROVIDER_FACTORY_H__