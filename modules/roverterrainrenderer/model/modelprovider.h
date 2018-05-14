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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___MODEL_PROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___MODEL_PROVIDER___H__

#include <modules/roverterrainrenderer/filehandler/subsite.h>
#include <openspace/scene/scenegraphnode.h>

namespace openspace {
class ModelProvider : public properties::PropertyOwner {
public:
 static std::unique_ptr<ModelProvider> createFromDictionary(const ghoul::Dictionary& dictionary);

 ModelProvider(const ghoul::Dictionary& dictionary);

 virtual std::vector<std::shared_ptr<Subsite>> calculate(const std::vector<std::vector<std::shared_ptr<Subsite>>> subsites,
  const RenderData& data, const SceneGraphNode* parent) = 0;

 virtual void initialize() = 0;
};
} // namespace openspace

#endif //__OPENSPACE_MODULE_GLOBEBROWSING___MODEL_PROVIDER___H__
