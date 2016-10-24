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


#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>
#include <modules/globebrowsing/tile/chunktile.h>
#include <modules/globebrowsing/tile/tileselector.h>
#include <modules/globebrowsing/layered_rendering/layeredtextures.h>

#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/assert.h>

#include <openspace/util/gpudata.h>

#include <memory>
#include <vector>
#include <string>

namespace openspace {
namespace globebrowsing {

    /**
     * Interface for uniform values that automatically may update its
     * uniform locations and setting values within the shader.
     * This interface may easily be implemented using a templated 
     * <code>GPUData</code> in combination with a regular 
     * <code>properties::Propery</code>. See GPUFloatProperty as an example.
     */
    class GPUProperty{
    public:

        /** 
         * Updates the uniform value within the shader program. Make
         * sure <code>updateUniformLocation</code> has been called
         * before calling this method.
         * @param programObject shader program to update uniform value within.
         */
        virtual void updateValue(ProgramObject* programObject) = 0;

        /** 
         * Updates the uniform location ID of the GPUProperty. This 
         * must be done before updating values within the shader program.
         * @param programObject shader program to update uniform location within.
         * @param nameBase may be used if defining uniform within glsl structs
         */
        virtual void updateUniformLocation(ProgramObject* programObject, const std::string& nameBase) = 0;

        /** 
         * Allows deactivation of certain GPUProperties after rendering.
         * This is usedful when dealing with Textures, as one may
         * need unassign texture units after rendering.
         */
        virtual void deactivate() { };

        /** 
         * Defines an implicity cast to a regular Property pointer, 
         * for convenience.
         */
        virtual operator properties::Property*() = 0;
    };

    /**
     * Defines a FloatProperty which automatically updates the uniform
     * float value within a shader program from the value defined within
     * property.
     */
    class GPUFloatProperty : public GPUProperty {
    public:
        GPUFloatProperty(properties::FloatProperty&&);
        virtual void updateValue(ProgramObject* programObject);
        virtual void updateUniformLocation(ProgramObject* programObject, const std::string& nameBase);
        virtual operator properties::Property*() {
            return &_property;
        };
    private:
        properties::FloatProperty _property;
        GPUData<float> gpuData;
    };

    /**
     * A container class for dealing multiple GPUProperties instances
     * within the same shader program. This is a general, convenient 
     * class which more specific GPUProperty collections may in order
     * to reduce boilerplate code.
     */
    class GPUPropertyCollection {
    public:

        /**
         * Updates the uniform values of all 
         * <code>GPUProperty</code>s within the collection.
         * @param programObject shader program to update uniform locations within.
         */
        void updateValues(ProgramObject* programObject) const;

        /**
         * Updates the uniform locations of all 
         * <code>GPUProperty</code>s within the collection.
         * @param programObject shader program to update uniform locations within.
         */
        void updateUniformLocations(ProgramObject* programObject, const std::string& nameBase) const;

        /**
        * @returns a const vector of all GPU properties
        */
        const std::vector<std::shared_ptr<GPUProperty>>& gpuProperties() const;
    protected:
        std::vector<std::shared_ptr<GPUProperty>> _gpuProperties;
    };


    /**
     * Rendering configurations implements the default constructor in 
     * which the settings are added
     */
    struct LayerRenderConfig : public GPUPropertyCollection {
        LayerRenderConfig();
    };

    /**
    * Simple struct which is used to enable/disable <code>TileProvider</code> 
    * and associate is with a name. It also holds layer specific information
    * which is used in rendering of layer.
    */
    struct Layer {
        std::string name;
        std::shared_ptr<TileProvider> tileProvider;
        bool isActive;
        
        LayerRenderConfig renderConfig;
    
        ChunkTilePile getChunkTilePile(const TileIndex& tileIndex, int pileSize) const;
    };

    /**
     * Convenience class for dealing with multiple <code>Layer</code>s.
     */
    struct LayerGroup {

        /// Updates all layers tile providers within this group
        void update();

        /// @returns const vector of all active layers
        const std::vector<Layer>& activeLayers() const;

        /// @returns the size of the pile to be used in rendering of this layer
        int pileSize() const;


        std::vector<Layer> layers;
        bool levelBlendingEnabled;

    private:
        std::vector<Layer> _activeLayers;
    };

    /**
    * Manages multiple LayerGroups.
    */
    class LayerManager {
    public:

        LayerManager(
            const ghoul::Dictionary& textureCategoriesDictionary,
            const ghoul::Dictionary& textureInitDictionary);
        ~LayerManager();

        LayerGroup& layerGroup(size_t groupId);
        LayerGroup& layerGroup(LayeredTextures::TextureCategory);

        void update();
        void reset(bool includingInactive = false);
    private:
        static void initTexures(
            std::vector<Layer>& destination, 
            const ghoul::Dictionary& dict, 
            const TileProviderInitData& initData);

        std::array<LayerGroup, LayeredTextures::NUM_TEXTURE_CATEGORIES> layerGroups;

    };

} // namespace globebrowsing
} // namespace openspace

#endif  // __TILE_PROVIDER_MANAGER_H__
