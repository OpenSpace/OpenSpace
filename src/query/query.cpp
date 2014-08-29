/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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
#include <openspace/rendering/renderable.h>

namespace openspace {

namespace {
const std::string _loggerCat = "Query";
}

SceneGraph* sceneGraph()
{
    return OsEng.renderEngine().sceneGraph();
}

SceneGraphNode* sceneGraphNode(const std::string& name)
{
    const SceneGraph* graph = sceneGraph();
    return graph->sceneGraphNode(name);
}
    
properties::Property* property(const std::string& uri)
{
    const size_t separator = uri.find('.');
    return property(uri.substr(0, separator), uri.substr(separator));
}
    
properties::Property* property(const std::string& nodeName, const std::string& propertyName)
{
    SceneGraphNode* node = sceneGraphNode(nodeName);
    if (!node) {
        LERROR("Node '" << nodeName << "' did not exist");
        return nullptr;
    }
    Renderable* propertyOwner = node->renderable();
    if (!propertyOwner) {
        LERROR("Node '" << nodeName << "' is not a PropertyOwner");
        return nullptr;
    }
    properties::Property* property = propertyOwner->property(propertyName);;
    if (!property) {
        LERROR("Node '" << nodeName << "' did not have property '" <<
               propertyName << "'");
        return nullptr;
    }
    return property;
}

}  // namespace
