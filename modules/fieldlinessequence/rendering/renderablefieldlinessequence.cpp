/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/fieldlinessequence/rendering/renderablefieldlinessequence.h>
#include <modules/fieldlinessequence/util/fieldlinessequencemanager.h>

#include <ghoul/misc/assert.h>

#include <openspace/engine/openspaceengine.h>
// #include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>

namespace {
    std::string _loggerCat = "RenderableFieldlinesSequence";
}

namespace {
    const char* keyVectorVolume = "VectorVolume";
    const char* keyFieldlines = "Fieldlines";
    const char* keySeedPoints = "SeedPoints";

    const char* keyVectorVolumeDirectory = "Directory";

    const char* keySeedPointsFile = "File";

    // const char* keySeedPointsDirectory = "Directory"; // TODO: allow for varying seed points?

}

namespace openspace {

RenderableFieldlinesSequence::RenderableFieldlinesSequence(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary) {

    std::string name;
    dictionary.getValue(SceneGraphNode::KeyName, name);

    _loggerCat = "RenderableFieldlines [" + name + "]";

    ghoul::Dictionary vectorVolumeInfo;
    ghoul::Dictionary fieldlineInfo;
    ghoul::Dictionary seedPointsInfo;

    // Find VectorVolume, SeedPoint and Fieldlines Info from Lua
    if (!dictionary.getValue(keyVectorVolume, vectorVolumeInfo)) {
        LERROR("Renderable does not contain a key for '" << keyVectorVolume << "'");
        // deinitialize();
    }

    if (!dictionary.getValue(keyFieldlines, fieldlineInfo)) {
        LERROR("Renderable does not contain a key for '" << keyFieldlines << "'");
        // deinitialize();
    }

    if (!dictionary.getValue(keySeedPoints, seedPointsInfo)) {
        LERROR("Renderable does not contain a key for '" << keySeedPoints << "'");
        // deinitialize();
    }
}

bool RenderableFieldlinesSequence::isReady() const {}

bool RenderableFieldlinesSequence::initialize() {}

bool RenderableFieldlinesSequence::deinitialize() {}

void RenderableFieldlinesSequence::render(const RenderData& data) {}

void RenderableFieldlinesSequence::update(const UpdateData&) {}

} // namespace openspace
