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

#include <openspace/documentation/verifier.h>

namespace openspace {

Documentation Scene::Documentation() {
    using namespace documentation;

    return {
        "Scene Description",
        {
        {
            "ScenePath",
            new StringVerifier,
            "The path to the base directory of the scene. The path is considered "
            "relative to the location of the scene description file.",
            Optional::Yes
        },
        {
            "CommonFolder",
            new StringAnnotationVerifier("A valid scene module folder"),
            "The path to the common folder that is loaded and will be bound to the "
            "${COMMON_MODULE} path token so that assets can be reused easily.",
            Optional::Yes
        },
        {
            "Camera",
            new TableVerifier({
                {
                    "Focus",
                    new StringAnnotationVerifier("A valid object in the scene"),
                    "The initial focus node of the camera, i.e., the node around which "
                    "the interaction will be performed."
                },
                {
                    "Position",
                    new DoubleVector3Verifier,
                    "The initial camera positive relative to the focus object.",
                    Optional::Yes
                },
                {
                    "Rotation",
                    new DoubleVector4Verifier,
                    "The initial camera rotation expressed as a quaternion.",
                    Optional::Yes
                }
            }),
            "Definitions of the camera starting parameters, such as focus, location, and "
            "orientation.",
            Optional::Yes
        },
        {
            "Modules",
            new TableVerifier({
                { "*", new StringAnnotationVerifier(
                    "Loadable module folders. This means that they either have to point "
                    "to a folder that contains a ModuleFile or a folder which contains "
                    "other folders that eventually contain ModuleFile. This second "
                    "recursive approach is useful for grouping modules into logical "
                    "units."
                )}
            }),
            "This is the list of modules that will be loaded into the initial scene. The "
            "values in this table have to correspond to folders relative to the "
            "ScenePath key. The order in which the modules are loaded is the same as the "
            "order in which they are specified in this table."
        }
        }
    };
}

} // namespace openspace
