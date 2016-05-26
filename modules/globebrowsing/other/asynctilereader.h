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


#ifndef __ASYNC_TILE_DATA_PROVIDER_H__
#define __ASYNC_TILE_DATA_PROVIDER_H__

//#include <modules/globebrowsing/other/tileprovider.h>

#include <ghoul/opengl/texture.h>

#include <modules/globebrowsing/geodetics/geodetic2.h>

#include <modules/globebrowsing/other/concurrentjobmanager.h>
#include <modules/globebrowsing/other/threadpool.h>
#include <modules/globebrowsing/other/tiledataset.h>

#include <memory>
#include <queue>
#include <unordered_map>



namespace openspace {
   


    struct TileLoadJob : public Job<TileIOResult> {
        TileLoadJob(std::shared_ptr<TileDataset> textureDataProvider, 
            const ChunkIndex& chunkIndex)
            : _textureDataProvider(textureDataProvider)
            , _chunkIndex(chunkIndex) 
        {

        }

        virtual ~TileLoadJob() { }

        virtual void execute() {
            _uninitedTexture = _textureDataProvider->readTileData(_chunkIndex);
        }



        virtual std::shared_ptr<TileIOResult> product() {
            return _uninitedTexture;
        }


    private:
        ChunkIndex _chunkIndex;
        std::shared_ptr<TileDataset> _textureDataProvider;
        std::shared_ptr<TileIOResult> _uninitedTexture;
    };





    class AsyncTileDataProvider {
    public:

        AsyncTileDataProvider(const std::string& filename, int minNumPixels, ThreadPool& pool);
        AsyncTileDataProvider(std::shared_ptr<TileDataset> textureDataProvider, 
            ThreadPool& pool);

        ~AsyncTileDataProvider();



        bool enqueueTextureData(const ChunkIndex& chunkIndex);
        bool hasLoadedTextureData() const;
        std::shared_ptr<TileIOResult> nextTileIOResult();
        
        void clearRequestQueue();

        std::shared_ptr<TileDataset> getTextureDataProvider() const;

    protected:

        virtual bool satisfiesEnqueueCriteria(const ChunkIndex&) const;

    private:

        std::shared_ptr<TileDataset> _textureDataProvider;
        ConcurrentJobManager<TileIOResult> _concurrentJobManager;
        std::unordered_map<HashKey, ChunkIndex> _enqueuedTileRequests;


    };



} // namespace openspace





#endif  // __ASYNC_TILE_DATA_PROVIDER_H__