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

#include <openspace/query/query.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
 
#include <modules/iswa/rendering/iswagroup.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <modules/iswa/util/iswamanager.h>
#include <modules/iswa/rendering/iswacygnet.h>

namespace openspace {

namespace {
    const std::string _loggerCat = "Query";
}

Scene* sceneGraph() {
    return OsEng.renderEngine().scene();
}

SceneGraphNode* sceneGraphNode(const std::string& name) {
    const Scene* graph = sceneGraph();
    return graph->sceneGraphNode(name);
}

Renderable* renderable(const std::string& name) {
    SceneGraphNode* node = sceneGraphNode(name);
    return node->renderable();
}

properties::Property* property(const std::string& uri) {
    properties::Property* globalProp = OsEng.globalPropertyOwner().property(uri);
    if (globalProp) {
        return globalProp;
    }
    else {
        // The URI consists of the following form at this stage:
        // <node name>.{<property owner>.}^(0..n)<property id>
        const size_t nameSeparator = uri.find(properties::PropertyOwner::URISeparator);
        if (nameSeparator == std::string::npos) {
            LERROR("Malformed URI '" << uri << "': At least one '" << nameSeparator
                   << "' separator must be present.");
            return nullptr;
        }
        const std::string nameUri = uri.substr(0, nameSeparator);
        const std::string remainingUri = uri.substr(nameSeparator + 1);

        SceneGraphNode* node = sceneGraphNode(nameUri);
        if (node) {
            properties::Property* property = node->property(remainingUri);
            return property;
        }

        std::shared_ptr<ScreenSpaceRenderable> ssr = OsEng.renderEngine().screenSpaceRenderable(nameUri);
        if(ssr){
            properties::Property* property = ssr->property(remainingUri);
            return property;
        }
#ifdef OPENSPACE_MODULE_ISWA_ENABLED
        std::shared_ptr<IswaGroup> group = IswaManager::ref().iswaGroup(nameUri);
        if(group){
            properties::Property* property = group->property(remainingUri);
            return property;
        }
#endif
        LERROR("Node or ScreenSpaceRenderable' " << nameUri << "' did not exist");
        return nullptr;
    }
}

}  // namespace
