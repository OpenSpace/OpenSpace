/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2015                                                               *
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

#ifndef __TEXTUREPLANE_H__
#define __TEXTUREPLANE_H__

// #include <openspace/rendering/renderable.h>
#include <modules/iswa/rendering/cygnetplane.h>
#include <ghoul/opengl/texture.h>
#include <openspace/engine/downloadmanager.h>

 namespace openspace{
 
 class TexturePlane : public CygnetPlane{
 public:
 	// TexturePlane(int cygnetId, std::string path);
 	TexturePlane(std::shared_ptr<Metadata> data);
 	~TexturePlane();

 	virtual bool initialize();
    virtual bool deinitialize();

	virtual void render();
	virtual void update();
 
 private:
 	virtual void loadTexture() override;
    virtual void updateTexture() override;

    static int id();
 
	std::shared_ptr<DownloadManager::FileFuture> _futureTexture;
 };
 
 } // namespace openspace

#endif