/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_ROVER_TERRAIN___ROVER_TERRAIN_TASK___H__
#define __OPENSPACE_MODULE_ROVER_TERRAIN___ROVER_TERRAIN_TASK___H__

#include <openspace/util/task.h>

#include <openspace/scene/asset.h>
#include <openspace/scene/assetlistener.h>
#include <openspace/scene/assetloader.h>

#include <string>
#include <memory>
#include <unordered_set>

namespace openspace {

class RoverTerrainTask : public Task {
public:
    class RequestListener : public AssetListener {
    public:
        virtual ~RequestListener() = default;
        void assetStateChanged(std::shared_ptr<Asset> asset,
            Asset::State state) override;

        void assetRequested(std::shared_ptr<Asset>,
            std::shared_ptr<Asset>) override {};

        void assetUnrequested(std::shared_ptr<Asset>,
            std::shared_ptr<Asset>) override {};
    };

    RoverTerrainTask(const ghoul::Dictionary& dictionary);

    std::string description() override;
    void perform(const Task::ProgressCallback& progressCallback) override;
    static documentation::Documentation documentation();
private:
    std::string _asset;
    std::string _outputPath;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_ROVER_TERRAIN___ROVER_TERRAIN_TASK___H__
