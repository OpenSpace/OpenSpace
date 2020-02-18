/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <modules/vislab/rendering/renderabledistancelabel.h>

#include <modules/base/rendering/renderablenodeline.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <ghoul/logging/logmanager.h>
#include <string>

namespace {
    constexpr const char* _loggerCat = "RenderableDistanceLabel";

    constexpr openspace::properties::Property::PropertyInfo NodeLineInfo = {
        "NodeLine",
        "Node Line",
        "Property to track a nodeline. When tracking the label text will be updating the "
        "distance from the nodeline start and end."
    };
}

namespace openspace {

documentation::Documentation RenderableDistanceLabel::Documentation() {
    using namespace documentation;
    return {
        "Renderable Distance Label",
        "vislab_renderable_distance_label",
        {
            {
                NodeLineInfo.identifier,
                new StringVerifier,
                Optional::No,
                NodeLineInfo.description
            },
        }
    };
}

RenderableDistanceLabel::RenderableDistanceLabel(const ghoul::Dictionary& dictionary)
    : RenderableLabels(dictionary)
    , _nodelineId(NodeLineInfo)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableDistanceLabel"
    );

    if (dictionary.hasKey(NodeLineInfo.identifier)) {
        _nodelineId = dictionary.value<std::string>(NodeLineInfo.identifier);
        addProperty(_nodelineId);
    }
}

void RenderableDistanceLabel::update(const UpdateData&) {
    if (_errorThrown) {
        return;
    }

    RenderEngine& RE = global::renderEngine;

    SceneGraphNode* nodelineNode = RE.scene()->sceneGraphNode(_nodelineId);
    if (nodelineNode) {
        // Calculate distance
        RenderableNodeLine* nodeline = dynamic_cast<RenderableNodeLine*>(
            nodelineNode->renderable()
        );
        if (!nodeline) {
            LERROR("Expected renderable to be of type 'RenderableNodeLine'");
            _errorThrown = true;
            return;
        }

        double myDistance = nodeline->distance();

        // Format string
        float scale = unit(Kilometer);
        std::string distanceText = std::to_string(std::round(myDistance / scale));
        int pos = static_cast<int>(distanceText.find("."));
        std::string subStr = distanceText.substr(pos);
        distanceText.erase(pos, subStr.size());
        std::string finalText = distanceText + " Km";
        setLabelText(finalText);

        // Update placement of label with transformation matrix
        SceneGraphNode* startNode = RE.scene()->sceneGraphNode(nodeline->start());
        glm::dvec3 start = startNode->worldPosition();
        SceneGraphNode* endNode = RE.scene()->sceneGraphNode(nodeline->end());
        glm::dvec3 end = endNode->worldPosition();
        glm::dvec3 goalPos = start + (end - start) / 2.0;
        _transformationMatrix = glm::translate(glm::dmat4(1.0), goalPos);
    }
    else {
        LERROR(fmt::format("There is no scenegraph node with id {}", _nodelineId));
        _errorThrown = true;
    }
}

} // namespace openspace

