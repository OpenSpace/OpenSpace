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
            "Identifier",
            new StringVerifier,
            Optional::No,
            "The identifier of this scenegraph node. This name must be unique among all "
            "scene graph nodes that are loaded in a specific scene. If a duplicate is "
            "detected the loading of the node will fail, as will all childing that "
            "depend on the node. The identifier must not contain any whitespaces or '.'."
        },
        {
            "Parent",
            new StringAnnotationVerifier(
                "If specified, this must be a name for another scenegraph node"
            ),
            Optional::Yes,
            "This names the parent of the currently specified scenegraph node. The "
            "parent must already exist in the scene graph. If not specified, the node "
            "will be attached to the root of the scenegraph.",
        },
        {
            "Renderable",
            new ReferencingVerifier("renderable"),
            Optional::Yes,
            "The renderable that is to be created for this scenegraph node. A renderable "
            "is a component of a scenegraph node that will lead to some visual result on "
            "the screen. The specifics heavily depend on the 'Type' of the renderable. "
            "If no Renderable is specified, this scenegraph node is an internal node and "
            "can be used for either group children, or apply common transformations to a "
            "group of children."
        },
        {
            "Transform",
            new TableVerifier({
                {
                    "Translation",
                    new ReferencingVerifier("core_transform_translation"),
                    Optional::Yes,
                    "This node describes a translation that is applied to the scenegraph "
                    "node and all its children. Depending on the 'Type' of the "
                    "translation, this can either be a static translation or a "
                    "time-varying one."
                },
                {
                    "Rotation",
                    new ReferencingVerifier("core_transform_rotation"),
                    Optional::Yes,
                    "This nodes describes a rotation that is applied to the scenegraph "
                    "node and all its children. Depending on the 'Type' of the rotation, "
                    "this can either be a static rotation or a time-varying one."
                },
                {
                    "Scale",
                    new ReferencingVerifier("core_transform_scaling"),
                    Optional::Yes,
                    "This node describes a scaling that is applied to the scenegraph "
                    "node and all its children. Depending on the 'Type' of the scaling, "
                    "this can either be a static scaling or a time-varying one."
                }
            }),
            Optional::Yes,
            "This describes a set of transformations that are applied to this scenegraph "
            "node and all of its children. There are only three possible values "
            "corresponding to a 'Translation', a 'Rotation', and a 'Scale'."
        },
        {
            "TimeFrame",
            new ReferencingVerifier("core_time_frame"),
            Optional::Yes,
            "Specifies the time frame for when this node should be active."
        },
        {
            "GUI",
            new TableVerifier({
                {
                    "Name",
                    new StringVerifier,
                    Optional::Yes,
                    "An optional user-facing name for this SceneGraphNode, which does "
                    "not have to be unique, though it is recommended, and can contain "
                    "any characters."
                },
                {
                    "Path",
                    new StringVerifier,
                    Optional::Yes,
                    "If this value is specified, this '/' separated URI specifies the "
                    "location of this scenegraph node in a GUI representation, for "
                    "instance '/SolarSystem/Earth/Moon'."
                },
                {
                    "Hidden",
                    new BoolVerifier,
                    Optional::Yes,
                    "If this value is specified, GUI applications are incouraged to "
                    "ignore this scenegraph node. This is most useful to trim collective "
                    "lists of nodes and not display, for example, barycenters."
                }
            }),
            Optional::Yes,
            "Additional information that is passed to GUI applications. These are all "
            "hints and do not have any impact on the actual function of the scenegraph "
            "node."
        },

        }
    };
}

} // namespace openspace
