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

#ifndef __OPENSPACE_MODULE_VIDEO___SCREENSPACEVIDEO___H__
#define __OPENSPACE_MODULE_VIDEO___SCREENSPACEVIDEO___H__

#include <openspace/rendering/screenspacerenderable.h>

#include <modules/video/include/videoplayer.h>
#include <openspace/properties/stringproperty.h>

namespace ghoul::opengl { class Texture; }

namespace openspace {

namespace documentation { struct Documentation; }

class ScreenSpaceVideo : public ScreenSpaceRenderable {
public:
    ScreenSpaceVideo(const ghoul::Dictionary& dictionary);

    bool initializeGL() override;
    bool deinitializeGL() override;
    void update() override;
    void render(const RenderData& renderData) override;

    static documentation::Documentation Documentation();

private:
    void bindTexture() override;

    VideoPlayer _videoPlayer;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_VIDEO___SCREENSPACEVIDEO___H__
