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

#include <modules/globebrowsing/globes/renderableglobe.h>

#include <modules/globebrowsing/globes/globemesh.h>
#include <modules/globebrowsing/globes/clipmapglobe.h>
#include <modules/globebrowsing/globes/chunklodglobe.h>

// open space includes
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/scene/scenegraphnode.h>

// ghoul includes
#include <ghoul/misc/assert.h>

namespace {
    const std::string _loggerCat = "RenderableGlobe";

    // Keys for the dictionary
    const std::string keyRadii = "Radii";
    const std::string keyTextures = "Textures";
    const std::string keyColorTextures = "ColorTextures";
    const std::string keyHeightMaps = "HeightMaps";
}

namespace openspace {


    RenderableGlobe::RenderableGlobe(const ghoul::Dictionary& dictionary)
        : DistanceSwitch()
        , _tileProviderManager(std::shared_ptr<TileProviderManager>(new TileProviderManager))
    {
        // Read the radii in to its own dictionary
        Vec3 radii;
        dictionary.getValue(keyRadii, radii);
        _ellipsoid = Ellipsoid(radii);

        ghoul::Dictionary texturesDictionary;
        dictionary.getValue(keyTextures, texturesDictionary);

        ghoul::Dictionary colorTexturesDictionary;
        texturesDictionary.getValue(keyColorTextures, colorTexturesDictionary);

        
        // Create TileProviders for all color textures
        for (size_t i = 1; i < colorTexturesDictionary.size() + 1; i++)
        {
            std::string name, path;
            ghoul::Dictionary colorTextureDictionary =
                colorTexturesDictionary.value<ghoul::Dictionary>(std::to_string(i));
            colorTextureDictionary.getValue("Name", name);
            colorTextureDictionary.getValue("FilePath", path);
            std::shared_ptr<TileProvider> colorTextureProvider =
                std::shared_ptr<TileProvider>(new TileProvider(
                    path, 5000, 1024));
            _tileProviderManager->addColorTexture(name, colorTextureProvider);
        }

        ghoul::Dictionary heightMapsDictionary;
        texturesDictionary.getValue(keyHeightMaps, heightMapsDictionary);

        // Create TileProviders for all height maps
        for (size_t i = 1; i < heightMapsDictionary.size() + 1; i++)
        {
            std::string name, path;
            ghoul::Dictionary heightMapDictionary =
                heightMapsDictionary.value<ghoul::Dictionary>(std::to_string(i));
            heightMapDictionary.getValue("Name", name);
            heightMapDictionary.getValue("FilePath", path);
            std::shared_ptr<TileProvider> heightMapProvider =
                std::shared_ptr<TileProvider>(new TileProvider(
                    path, 5000, 256));
            _tileProviderManager->addHeightMap(name, heightMapProvider);
        }

        //addSwitchValue(std::shared_ptr<ClipMapGlobe>(
        //    new ClipMapGlobe(_ellipsoid, _tileProviderManager)), 1e8);
        addSwitchValue(std::shared_ptr<ChunkLodGlobe>(
            new ChunkLodGlobe(_ellipsoid, _tileProviderManager)), 1e9);
        addSwitchValue(std::shared_ptr<GlobeMesh>(new GlobeMesh()), 1e10);
    }

    RenderableGlobe::~RenderableGlobe() {
    }

    void RenderableGlobe::update(const UpdateData& data) {
        _time = data.time;
        DistanceSwitch::update(data);
    }

}  // namespace openspace
