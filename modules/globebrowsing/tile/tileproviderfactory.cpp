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

#include <modules/globebrowsing/tile/tileproviderfactory.h>

#include <ghoul/logging/logmanager.h>

#include "cpl_minixml.h"


namespace {
    const std::string _loggerCat = "TileProviderFactory";
}


namespace openspace {

    std::shared_ptr<TileProviderFactory> TileProviderFactory::_ref = nullptr;


    TileProviderFactory::TileProviderFactory() {
        initialize();
    }


    std::shared_ptr<TileProviderFactory> TileProviderFactory::ref() {
        if (_ref == nullptr) {
            // Need to explicitly use new here, since constructor is private
            TileProviderFactory* ptr = new TileProviderFactory();
            _ref = std::shared_ptr<TileProviderFactory>(ptr);
        }
        return _ref;
    }

    std::shared_ptr<TileProvider> TileProviderFactory::create(const std::string& type,
        const std::string& desc, const TileProviderInitData& initData) 
    {
        auto concreteFactoryIterator = _factoryMap.find(type);
        
        if (concreteFactoryIterator == _factoryMap.end()) {
            LERROR("Unknown type: " << type);
            return nullptr;
        }
        return concreteFactoryIterator->second(desc, initData);
    }

    void TileProviderFactory::initialize() {
        _factoryMap.insert({"LRUCaching", [](const std::string& desc, const TileProviderInitData& initData) {
            auto tileDataset = std::make_shared<TileDataset>(desc, initData.minimumPixelSize, initData.preprocessTiles);
            auto threadPool = std::make_shared<ThreadPool>(1);
            auto tileReader = std::make_shared<AsyncTileDataProvider>(tileDataset, threadPool);
            auto tileCache = std::make_shared<TileCache>(initData.cacheSize);
            auto tileProvider = std::make_shared<CachingTileProvider>(tileReader, tileCache, initData.framesUntilRequestQueueFlush);
            return tileProvider;
        }});
        
        _factoryMap.insert({ "Temporal", [](const std::string& file, const TileProviderInitData& initData) {
            CPLXMLNode * node = CPLParseXMLFile(file.c_str());
            if (!node) {
                throw ghoul::RuntimeError("Unable to parse file:\n" + file);
            }
            if (std::string(node->pszValue) == "OpenSpaceTemporalGDALDataset") {
                auto tileProvider = std::make_shared<TemporalTileProvider>(file, initData);
                return tileProvider;
            }
        }});
        
        _factoryMap.insert({ "SingleImage", [](const std::string& file, const TileProviderInitData& initData) {
            auto tileProvider = std::make_shared<SingleImagePrivoder>(file);
            return tileProvider;
        } });
    }


}  // namespace openspace
