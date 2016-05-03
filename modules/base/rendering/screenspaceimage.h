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
#ifndef __SCREENSPACEIMAGE_H__
#define __SCREENSPACEIMAGE_H__

#include <openspace/rendering/screenspacerenderable.h>
#include <ghoul/opengl/texture.h>
 
namespace openspace {
/**
 * @brief Creates a textured plane rendered in screenspace
 * @details The plane gets the same ratio as the texture. Implements
 * the interface that ScreenSpaceImage speciefies.
 * 
 * @param texturePath Path to the image that should be used as texture
 */
class ScreenSpaceImage : public ScreenSpaceRenderable {

public:
    ScreenSpaceImage(std::string texturePath);
    ScreenSpaceImage(const ghoul::Dictionary& dictionary);
    ~ScreenSpaceImage();

    bool initialize() override;
    bool deinitialize() override;
    void render() override;
    void update() override;
    bool isReady() const override;

private:
    void loadTexture();
    static int id();
    
    properties::StringProperty _texturePath;
    int _id;
};

} //namespace openspace
#endif //__SCREENSPACEIMAGE_H__