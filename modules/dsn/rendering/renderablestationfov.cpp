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
#include <modules/dsn/rendering/renderablestationfov.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/logging/logmanager.h>
//#include <openspace/engine/globals.h>
//#include <openspace/rendering/renderengine.h>
//#include <openspace/util/updatestructures.h>
//#include <ghoul/opengl/programobject.h>

//namespace {
//    //constexpr const char* ProgramName = "ConeProgram";
//    constexpr const char* _loggerCat = "RenderableStationFov";
//} // namespace

namespace openspace {


documentation::Documentation RenderableStationFov::Documentation() {
    using namespace documentation;
    return {
        "Renderable Station Fov",
        "dsn_renderable_renderablestationfov",
        {
            {
                "Type",
                new StringEqualVerifier("RenderableStationFov"),
                Optional::No
            }
        }
    };
}

    RenderableStationFov::RenderableStationFov(const ghoul::Dictionary& dictionary) : RenderableCone(dictionary)
{

    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableStationFov"
    );

}

    //void RenderableStationFov::initializeGL()
    //{
    //    RenderableCone::initializeGL();
    //}

    //void RenderableStationFov::deinitializeGL()
    //{
    //    RenderableCone::deinitializeGL();
    //}

    //void RenderableStationFov::update(const UpdateData & data)
    //{
    //    RenderableCone::update(data);
    //}

    //void RenderableStationFov::render(const RenderData & data, RendererTasks & rendererTask)
    //{
    //    RenderableCone::render(data, rendererTask);
    //}

    //bool RenderableStationFov::isReady() const
    //{
    //    return true;
    //}

} // namespace openspace
