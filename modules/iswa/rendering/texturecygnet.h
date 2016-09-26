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

#ifndef __TEXTURECYGNET_H__
#define __TEXTURECYGNET_H__

#include <modules/iswa/rendering/iswacygnet.h>

namespace openspace{

/**
 * This class exist to abstract away the loading of images
 * from iSWA and updating of the textures for child geometries.
 * The class specifies the minimum interface that child classes 
 * needs to implement.
 */
class TextureCygnet : public IswaCygnet {
public:
    TextureCygnet(const ghoul::Dictionary& dictionary);
    ~TextureCygnet();

protected:

    bool updateTexture() override;
    bool downloadTextureResource(double timestamp = Time::ref().j2000Seconds()) override;
    bool readyToRender() const override;
    bool updateTextureResource() override;

    // Interface for concrete subclasses
    virtual void setUniforms() = 0;
    virtual bool createGeometry() = 0;
    virtual bool destroyGeometry() = 0;
    virtual void renderGeometry() const = 0;

private:
    DownloadManager::MemoryFile _imageFile;
};
} //namespace openspace

#endif //__TEXTURECYGNET_H__