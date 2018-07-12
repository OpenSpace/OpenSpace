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
    void TextureWriterFreeImage::writeTexture(std::vector<GLubyte> texturePixels, int w, int h) {

        FreeImage_Initialise();


        /***CREATE BITMAP***/
        //FIBITMAP* bitmap = FreeImage_Allocate(1024, 768, 24);
        //RGBQUAD color;
        //int w = 1920;
        //int h = 1137;
        //fullscreen window size 1920,1080
        //FIBITMAP* Image = FreeImage_ConvertFromRawBits(texturePixels, w, h, 4*w, 8, 0xFF0000, 0x00FF00, 0x0000FF, false); 
        //FreeImage_Save(FIF_PNG, Image, "kristin.png", 0);


        //unsigned char texturePixels2[1920][1080];
        //for (int i = 0; i < 1920; i++) {
        //    for (int j = 0; j < 1080; j++) {
        //        texturePixels2[i][j] = 255;
        //    }
        //}
        //unsigned char* pix = texturePixels2[0][0];

        //FIBITMAP* Image2 = FreeImage_ConvertFromRawBits(pix, 1920, 1080, 3*1920, 24, 0xFF0000, 0xFF0000, 0xFF0000, false); 
        //FreeImage_Save(FIF_PNG, Image2, "caroline.png", 0);


        //FIBITMAP* bitmap = FreeImage_Allocate(1920, 1080, 24);
        //RGBQUAD color;
        //for (int i = 0; i < 1920; i++) {
        //    for (int j = 0; j< 1080; j++) {
        //        color.rgbRed = 255.0;
        //        color.rgbGreen = 0; //(double)i / 1920 * 255.0;
        //        color.rgbBlue = 0; //(double)j / 1080 * 255.0;
        //        FreeImage_SetPixelColor(bitmap, i, j, &color);
        //    }
        //}
        //FreeImage_Save(FIF_PNG, bitmap, "redImage.png", 0);

        //Use iterator
        
        //FIBITMAP* bitmap = FreeImage_Allocate(w, h, 24);    //w = 1280, h=720
        //RGBQUAD color;
        //for (std::vector<GLubyte>::iterator it = texturePixels.begin(); it != texturePixels.end(); ++it)          
        //{
        //    color.rgbRed = it->data(); 
        //    color.rgbGreen = 0; //(double)i / 1920 * 255.0;
        //    color.rgbBlue = 0; //(double)j / 1080 * 255.0;
        //    FreeImage_SetPixelColor(bitmap, 1, 2, &color);
        //}

        //FIBITMAP* bitmap = FreeImage_Allocate(w, h, 24);    //w = 1280, h=720
        //RGBQUAD color;
        //for (int i = 0; i < w; i++) {
        //    for (int j = 0; j< h; j++) {
        //        color.rgbRed = texturePixels[i][j]; 
        //        color.rgbGreen = 0; //(double)i / 1920 * 255.0;
        //        color.rgbBlue = 0; //(double)j / 1080 * 255.0;
        //        FreeImage_SetPixelColor(bitmap, i, j, &color);
        //    }
        //}
        //FreeImage_Save(FIF_PNG, bitmap, "redImage.png", 0);

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
