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

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

namespace openspace {

documentation::Documentation SceneGraphNode::Documentation() {
    using namespace documentation;

    return {
        "Scenegraph Node",
        "core_scene_node",
        {
        {
            "Name",
            new StringVerifier,
            "The name of this scenegraph node. This name must be unique among all scene "
            "graph nodes that are loaded in a specific scene. If a duplicate is detected "
            "the loading of the node will fail, as will all childing that depend on the "
            "node.",
            Optional::No
        },
        {
            "Parent",
            new StringAnnotationVerifier(
                "Must be a name for another scenegraph node, or 'Root'"
            ),
            "This names the parent of the currently specified scenegraph node. The "
            "parent must not have been defined earlier, but must exist at loading time, "
            "or the scenegraph node creation will fail. A special parent 'Root' is "
            "available that denotes the root of the scenegraph.",
            Optional::No
        },
        {
            "Renderable",
            new ReferencingVerifier("renderable"),
            "The renderable that is to be created for this scenegraph node. A renderable "
            "is a component of a scenegraph node that will lead to some visual result on "
            "the screen. The specifics heavily depend on the 'Type' of the renderable. "
            "If no Renderable is specified, this scenegraph node is an internal node and "
            "can be used for either group children, or apply common transformations to a "
            "group of children.",
            Optional::Yes
        },
        {
            "Transform",
            new TableVerifier({
                {
                    "Translation",
                    new ReferencingVerifier("core_transform_translation"),
                    "This node describes a translation that is applied to the scenegraph "
                    "node and all its children. Depending on the 'Type' of the "
                    "translation, this can either be a static translation or a "
                    "time-varying one.",
                    Optional::Yes
                },
                {
                    "Rotation",
                    new ReferencingVerifier("core_transform_rotation"),
                    "This nodes describes a rotation that is applied to the scenegraph "
                    "node and all its children. Depending on the 'Type' of the rotation, "
                    "this can either be a static rotation or a time-varying one.",
                    Optional::Yes
                },
                {
                    "Scale",
                    new ReferencingVerifier("core_transform_scaling"),
                    "This node describes a scaling that is applied to the scenegraph "
                    "node and all its children. Depending on the 'Type' of the scaling, "
                    "this can either be a static scaling or a time-varying one.",
                    Optional::Yes
                }
            }),
            "This describes a set of transformations that are applied to this scenegraph "
            "node and all of its children. There are only three possible values "
            "corresponding to a 'Translation', a 'Rotation', and a 'Scale'.",
            Optional::Yes
        },
        }
    };
}

} // namespace openspace
