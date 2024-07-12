/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>

namespace openspace {

Scene* sceneGraph() {
    return global::renderEngine->scene();
}

SceneGraphNode* sceneGraphNode(const std::string& name) {
    const Scene* graph = sceneGraph();
    if (!graph) {
        return nullptr;
    }
    return graph->sceneGraphNode(name);
}

const Renderable* renderable(const std::string& name) {
    SceneGraphNode* node = sceneGraphNode(name);
    if (!node) {
        return nullptr;
    }
    return node->renderable();
}

properties::Property* property(const std::string& uri) {
    properties::Property* property = global::rootPropertyOwner->property(uri);
    return property;
}

properties::PropertyOwner* propertyOwner(const std::string& uri) {
    properties::PropertyOwner* property = global::rootPropertyOwner->propertyOwner(uri);
    return property;
}

std::vector<properties::Property*> allProperties() {
    return global::rootPropertyOwner->propertiesRecursive();
}

}  // namespace openspace
