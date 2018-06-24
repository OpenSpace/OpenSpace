/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2018                                                               *
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

#include <modules/marsrover/heighthandler/texturewriterfreeimage.h>

#ifdef GHOUL_USE_FREEIMAGE

#include <ghoul/opengl/texture.h>
#include <ghoul/glm.h>
#include <ghoul/fmt.h>

#define FREEIMAGE_COLORORDER_BGR 0
#define FREEIMAGE_COLORORDER_RGB 1
#if defined(APPLE) || defined(FREEIMAGE_BIGENDIAN)     
#define FREEIMAGE_COLORORDER FREEIMAGE_COLORORDER_BGR
#else
#define FREEIMAGE_COLORORDER FREEIMAGE_COLORORDER_RGB
#endif


#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4005)
#endif // _MSC_VER

#include <FreeImage.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER


namespace ghoul::io {
    void TextureWriterFreeImage::writeTexture(unsigned char* texturePixels) {
        //const std::char filename = "heightmap.png";

        //Maybe we need to use FreeImage and create an own function to handle this. Look at on Wednesday!!
        FreeImage_Initialise();


        /***CREATE BITMAP***/
        //FIBITMAP* bitmap = FreeImage_Allocate(1024, 768, 24);
        //RGBQUAD color;

        //fullscreen window size 1920,1080
        FIBITMAP* Image = FreeImage_ConvertFromRawBits(texturePixels, 1280, 720, 4*1280, 24, 0xFF0000, 0x00FF00, 0x0000FF, false); 
        FreeImage_Save(FIF_PNG, Image, "kristin.png", 0);

        //for (int i = 0; i < 1920; i++) {
        //    for (int j = 0; j< 1080; j++) {
        //        color.rgbRed = 0;
        //        color.rgbGreen = 0;
        //        color.rgbBlue = 0;
        //        FreeImage_SetPixelColor(bitmap, i, j, &color);
        //    }
        //}
        //FreeImage_Save(FIF_PNG, bitmap, "newKristin.png", 0);

        FreeImage_DeInitialise();

    }

    bool TextureWriterFreeImage::success(int ok) {
        if(ok == 1)
            return true;
        else
            return false;
    }


} // namespace ghoul::io

#endif // GHOUL_USE_FREEIMAGE
