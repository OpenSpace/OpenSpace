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

#ifndef __OPENSPACE_MODULE_DSN___RENDERABLESTATIONFOV___H__
#define __OPENSPACE_MODULE_DSN___RENDERABLESTATIONFOV___H__

#include <modules/dsn/rendering/renderablecone.h>

#include <modules/dsn/dsnmodule.h>
#include <openspace/scene/scene.h>
//#include <ghoul/logging/logmanager.h>
//#include <ghoul/opengl/ghoul_gl.h>
//#include <ghoul/opengl/uniformcache.h>

//namespace ghoul::opengl {
//    class ProgramObject;
//    class Texture;
//} // namespace ghoul::opengl

namespace openspace {
    class RenderableCone;

    namespace documentation { struct Documentation; }

    //class Translation;

    /**
     * This is a class for rendering cones
     **/
    class RenderableStationFov : public RenderableCone{

    public:
         RenderableStationFov(const ghoul::Dictionary& dictionary);
        ~RenderableStationFov() = default;
        static documentation::Documentation Documentation();

        //void initializeGL();
        //void deinitializeGL();
        //void update(const UpdateData & data);
        //void render(const RenderData & data, RendererTasks & rendererTask);
        //bool isReady() const;

    };

} // namespace openspace
#endif //__OPENSPACE_MODULE_DSN___RENDERABLESTATIONFOV___H__
